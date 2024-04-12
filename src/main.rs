use std::{
    cmp::{max, min, Reverse},
    collections::{BinaryHeap, HashSet},
    error::Error,
    mem::forget,
    process::{Command, Stdio},
    ptr,
};

use config::{KEYBINDS, MOUSEBINDS};
use x11rb::{
    connection::Connection,
    errors::{ReplyError, ReplyOrIdError},
    protocol::{
        randr::{
            Connection as randr_Connection, ConnectionExt as randr_ConnectionExt, GetCrtcInfoReply,
            ScreenChangeNotifyEvent,
        },
        xproto::{ConnectionExt, *},
        ErrorKind, Event,
    },
    xcb_ffi::XCBConnection,
    CURRENT_TIME,
};
use xcb::{
    ffi::{
        xcb_connection_t, xcb_get_modifier_mapping_keycodes, xcb_get_modifier_mapping_reply,
        xcb_get_modifier_mapping_reply_t, xcb_get_modifier_mapping_unchecked, xcb_keycode_t,
        XCB_CURRENT_TIME, XCB_CURSOR_NONE, XCB_NO_SYMBOL, XCB_WINDOW_NONE,
    },
    GRAB_ANY, MOD_MASK_LOCK,
};
use xcb_util::{
    icccm::get_wm_protocols,
    keysyms::{KeySymbols, KeycodeIter},
};
use xkbcommon::xkb::{ffi::xkb_keysym_get_name, keysyms, Keysym};

mod config;

macro_rules! CLEANMASK {
    ( $m:expr ) => {
        u32::from($m) & !(unsafe { NUM_LOCK_MASK } | MOD_MASK_LOCK)
    };
}

macro_rules! requests_to_atom_array {
    ( $d:expr, $s:ident ) => {
        let mut i: usize = 0;
        for r in $s {
            $d[i] = r.reply()?.atom;
            i += 1;
        }
    };
}

macro_rules! to_base {
    ( $e:expr ) => {
        unsafe { xcb::base::Connection::from_raw_conn($e.get_raw_xcb_connection() as _) }
    };
}

macro_rules! unwrap_or_return_o {
    ( $e:expr, $r:expr) => {
        match $e {
            Some(x) => x,
            None => return $r,
        }
    };
}

macro_rules! unwrap_or_return_r {
    ( $e:expr, $r:expr ) => {
        match $e {
            Ok(x) => x,
            Err(_) => return $r,
        }
    };
}

macro_rules! BUTTONMASK {
    () => {
        EventMask::BUTTON_PRESS | EventMask::BUTTON_RELEASE
    };
}

macro_rules! MOUSEMASK {
    () => {
        BUTTONMASK!() | EventMask::POINTER_MOTION
    };
}
static mut NUM_LOCK_MASK: u32 = 0;
const NUMTAGS: usize = 9;

enum WMAtom {
    WMProtocols,
    WMDelete,
    WMState,
    WMTakeFocus,
    COUNT,
}

macro_rules! wa {
    ( $e:ident ) => {
        WMAtom::$e as usize
    };
}

enum NetAtom {
    NetActiveWindow,
    NetSupported,
    NetWMName,
    NetWMState,
    NetWMSupportingWMCheck,
    NetWMFullscreen,
    NetWMWindowType,
    NetWMWindowTypeDialog,
    NetClientList,
    COUNT,
}

macro_rules! na {
    ( $e:tt ) => {
        NetAtom::tt as usize
    };
}

#[derive(PartialEq)]
enum Click {
    RootWin,
    ClientWin,
}

#[derive(Clone)]
struct Client {
    name: String,
    x: i16, // Window geometries
    y: i16,
    w: u16,
    h: u16,
    cfact: f32, // The weight of the window within the parent or child stack
    tags: u32,  // bitfield containing all of the tags to display on
    window_id: u32,
    monitor: usize,
    fullscreen: bool,
}

impl Client {
    fn new(
        dpy: &XCBConnection,
        win: Window,
        geometry: &GetGeometryReply,
        monitor: usize,
    ) -> Result<Client, ReplyOrIdError> {
        let wm_name_reply = dpy
            .get_property(false, win, AtomEnum::WM_NAME, AtomEnum::STRING, 0, 0)?
            .reply()?;

        let wm_name = String::from_utf8(wm_name_reply.value).unwrap_or("unknown".to_owned());

        return Ok(Client {
            name: wm_name,
            x: geometry.x,
            y: geometry.y,
            w: geometry.width,
            h: geometry.height,
            cfact: 0.0,
            tags: 0,
            window_id: win,
            monitor,
            fullscreen: false,
        });
    }

    fn close_x_position(&self) -> i16 {
        return std::cmp::max(0, self.w) as _;
    }

    fn send_event(&self, state: &State, proto: Atom) -> bool {
        let c = to_base!(state.dpy);

        let mut found: bool = false;
        let res =
            get_wm_protocols(&c, self.window_id, state.wm_atoms[wa!(WMProtocols)]).get_reply();

        if let Ok(reply) = res {
            let atoms = reply.atoms();

            for atom in atoms {
                if *atom == proto {
                    found = true;
                    break;
                }
            }

            if found {
                let event = ClientMessageEvent::new(
                    32,
                    self.window_id,
                    state.wm_atoms[wa!(WMProtocols)],
                    [proto, XCB_CURRENT_TIME, 0, 0, 0],
                );
                let _ = state
                    .dpy
                    .send_event(false, self.window_id, EventMask::NO_EVENT, event);
            }
        }

        forget(c);

        return found;
    }

    fn kill(&self, state: &State) {
        if !&self.send_event(state, state.wm_atoms[wa!(WMProtocols)]) {
            let _ = state.dpy.kill_client(self.window_id);
        }
    }
}

#[derive(Clone, Copy)]
struct TagInfo {
    num_masters: usize,
    mfact: f32,    // The weight of the master window
    layout: usize, // Index of layout in the Layout's array
}

struct Layout<'a> {
    symbol: &'a str,
    arrange: fn(&Monitor),
}

struct Monitor {
    clients: Vec<Client>,   // All of the client windows within the monitor
    selected_client: usize, // Index of selected client
    selected_tags: u16,
    tag_info: [TagInfo; NUMTAGS],
    num: usize,
    mx: i16, // Screen geometries
    my: i16,
    mw: u16,
    mh: u16,
    wx: u16, // Window area geometries
    wy: u16,
    ww: u16,
    wh: u16,
    barwin: Window,
}

impl Monitor {
    fn new(crtc_info: GetCrtcInfoReply, num: usize) -> Monitor {
        Monitor {
            clients: Vec::new(),
            selected_client: 0,
            selected_tags: 0,
            tag_info: [TagInfo {
                num_masters: 0,
                mfact: 0.0,
                layout: 0,
            }; NUMTAGS],
            num,
            mx: crtc_info.x,
            my: crtc_info.y,
            mw: crtc_info.width,
            mh: crtc_info.height,
            wx: 0,
            wy: 0,
            ww: 0,
            wh: 0,
            barwin: 0,
        }
    }

    fn selclient(&self) -> Option<&Client> {
        if self.clients.len() == 0 {
            return None;
        }

        return Some(&self.clients[self.selected_client]);
    }
}

struct State<'a> {
    dpy: &'a XCBConnection,
    screen: &'a Screen,
    root: u32,
    black_gc: Gcontext,
    monitors: Vec<Monitor>,  // All of the monitors connected to the machine
    selected_monitor: usize, // Index of selected monitor
    wm_atoms: [Atom; WMAtom::COUNT as usize],
    net_atoms: [Atom; NetAtom::COUNT as usize],
    sequences_to_ignore: BinaryHeap<Reverse<u16>>,
    pending_expose: HashSet<Window>,
    drag_window: Option<(Window, (i16, i16))>,
}

impl<'a> State<'a> {
    fn new(dpy: &'a XCBConnection, screen_num: usize) -> Result<State<'a>, Box<dyn Error>> {
        let screen = &dpy.setup().roots[screen_num];
        let black_gc = dpy.generate_id()?;
        let font = dpy.generate_id()?;
        dpy.open_font(font, b"9x15")?;

        let gc_aux = CreateGCAux::new()
            .graphics_exposures(0)
            .background(screen.white_pixel)
            .foreground(screen.black_pixel)
            .font(font);
        dpy.create_gc(black_gc, screen.root, &gc_aux)?;
        dpy.close_font(font)?;

        let wm_atom_requests = [
            dpy.intern_atom(false, b"WM_PROTOCOLS")?,
            dpy.intern_atom(false, b"WM_DELETE_WINDOW")?,
            dpy.intern_atom(false, b"WM_STATE")?,
            dpy.intern_atom(false, b"WM_TAKE_FOCUS")?,
        ];

        let net_atom_requests = [
            dpy.intern_atom(false, b"_NET_ACTIVE_WINDOW")?,
            dpy.intern_atom(false, b"_NET_SUPPORTED")?,
            dpy.intern_atom(false, b"_NET_WM_NAME")?,
            dpy.intern_atom(false, b"_NET_WM_STATE")?,
            dpy.intern_atom(false, b"_NET_SUPPORTING_WM_CHECK")?,
            dpy.intern_atom(false, b"_NET_WM_STATE_FULLSCREEN")?,
            dpy.intern_atom(false, b"_NET_WM_WINDOW_TYPE")?,
            dpy.intern_atom(false, b"_NET_WM_WINDOW_TYPE_DIALOG")?,
            dpy.intern_atom(false, b"_NET_CLIENT_LIST")?,
        ];

        let mut state = State {
            dpy,
            screen,
            root: screen.root,
            black_gc,
            monitors: Vec::new(),
            selected_monitor: 0,
            wm_atoms: [Atom::MIN; WMAtom::COUNT as usize],
            net_atoms: [Atom::MIN; NetAtom::COUNT as usize],
            sequences_to_ignore: Default::default(),
            pending_expose: Default::default(),
            drag_window: None,
        };

        requests_to_atom_array!(state.wm_atoms, wm_atom_requests);
        requests_to_atom_array!(state.net_atoms, net_atom_requests);

        state.update_numlock()?;
        state.query_monitors()?;
        state.query_clients()?;
        state.grab_keys()?;

        return Ok(state);
    }

    fn cleanup(&self) -> Result<(), Box<dyn Error>> {
        return Ok(());
    }

    // TODO: Move to be implemented on state??
    fn button_press_handler(&mut self, event: ButtonPressEvent) -> Result<(), Box<dyn Error>> {
        println!("buttonpress handler");
        let mut click = Click::RootWin;

        if let Some(_) = self.find_client_by_window_id(event.event) {
            println("clientwin");
            click = Click::ClientWin;
            let _ = self.dpy.allow_events(Allow::REPLAY_POINTER, event.time);
            let _ = self.dpy.flush();
        }

        for bind in MOUSEBINDS {
            if bind.click == click && CLEANMASK!(bind.button) == CLEANMASK!(event.state) {
                (bind.func)(self);
            }
        }

        return Ok(());
    }

    fn configure_request_handler(
        &self,
        event: ConfigureRequestEvent,
    ) -> Result<(), Box<dyn Error>> {
        // if let Some(c) = self.find_client_by_window_id(event.window) {
        //     let _ = unimplemented!();
        // }

        let aux = ConfigureWindowAux::from_configure_request(&event)
            .sibling(None)
            .stack_mode(None);
        self.dpy.configure_window(event.window, &aux)?;

        return Ok(());
    }

    fn enter_notify_handler(&mut self, event: EnterNotifyEvent) -> Result<(), Box<dyn Error>> {
        if let Some(c) = self.find_client_by_window_id(event.event) {
            self.dpy
                .set_input_focus(InputFocus::PARENT, c.window_id, CURRENT_TIME)?;
        }

        return Ok(());
    }

    fn expose_handler(&mut self, event: ExposeEvent) -> Result<(), Box<dyn Error>> {
        // self.pending_expose.insert(event.window);

        return Ok(());
    }

    fn find_client_by_window_id(&self, win: Window) -> Option<&Client> {
        for monitor in &self.monitors {
            for client in &monitor.clients {
                if client.window_id == win {
                    return Some(&client);
                }
            }
        }

        return None;
    }

    fn grab_buttons(&self, client: &Client, focused: bool) -> Result<(), Box<dyn Error>> {
        self.update_numlock();

        self.dpy
            .ungrab_button(ButtonIndex::ANY.into(), self.root, ModMask::ANY)?;

        if !focused {
            self.dpy.grab_button(
                false,
                client.window_id,
                BUTTONMASK!(),
                GrabMode::SYNC,
                GrabMode::SYNC,
                XCB_WINDOW_NONE,
                XCB_CURSOR_NONE,
                ButtonIndex::ANY,
                ModMask::ANY,
            );
        }
        for bind in MOUSEBINDS {
            if bind.click == Click::ClientWin {
                self.dpy.grab_button(
                    false,
                    self.root,
                    EventMask::BUTTON_PRESS,
                    GrabMode::ASYNC,
                    GrabMode::SYNC,
                    XCB_WINDOW_NONE,
                    XCB_CURSOR_NONE,
                    bind.button,
                    bitwise_mask(&bind.modifiers),
                );
            }
        }

        return Ok(());
    }

    fn grab_keys(&self) -> Result<(), Box<dyn Error>> {
        self.dpy
            .ungrab_key(GRAB_ANY as u8, self.root, ModMask::ANY)?;

        for bind in config::KEYBINDS {
            let keycodes = xcb_get_keycodes(self.dpy, bind.key);

            for keycode in keycodes {
                if keycode as u32 == XCB_NO_SYMBOL {
                    break;
                }

                self.dpy.grab_key(
                    true,
                    self.root,
                    bitwise_mask(&bind.modifiers),
                    keycode,
                    GrabMode::ASYNC,
                    GrabMode::ASYNC,
                )?;
                println!(
                    "Grabbed key {} successfully",
                    xcb_get_name_from_keycode(self.dpy, keycode).unwrap_or("unknown".to_owned())
                );
            }
        }

        return Ok(());
    }

    // This function sucks
    fn handle_event(&mut self, event: Event) -> Result<(), Box<dyn Error>> {
        return match event {
            Event::ButtonPress(event) => self.button_press_handler(event),
            // Client message
            Event::ConfigureRequest(event) => self.configure_request_handler(event),
            // Configure notify
            // Destroy notify
            Event::EnterNotify(event) => self.enter_notify_handler(event),
            Event::Expose(event) => self.expose_handler(event),
            // Focus in
            Event::KeyPress(event) => self.key_press_handler(event),
            Event::MapRequest(event) => self.map_request_handler(event),
            Event::MotionNotify(event) => self.motion_notify_handler(event),
            // Property notify
            Event::RandrScreenChangeNotify(event) => self.screen_change_handler(event),
            Event::UnmapNotify(event) => self.unmap_notify_handler(event),
            _ => Ok(()),
        };
    }

    fn key_press_handler(&mut self, event: KeyPressEvent) -> Result<(), Box<dyn Error>> {
        let keysym: Keysym = unwrap_or_return_o!(xcb_get_keysym(self.dpy, event.detail), Ok(()));

        for bind in KEYBINDS {
            if keysym == bind.key
                && CLEANMASK!(event.state) == CLEANMASK!(bitwise_mask(&bind.modifiers))
            {
                println!(
                    "Calling binding for {}",
                    xcb_get_name_from_keycode(self.dpy, event.detail)
                        .unwrap_or("unknown".to_owned())
                );
                bind.call(&self);
            }
        }

        return Ok(());
    }

    fn manage_window(
        &mut self,
        win: Window,
        geometry: &GetGeometryReply,
    ) -> Result<(), ReplyOrIdError> {
        assert!(self.find_client_by_window_id(win).is_none());

        self.dpy.grab_server()?;
        self.dpy.change_save_set(SetMode::INSERT, win)?;
        let cookie = self.dpy.reparent_window(win, self.root, 0, 0)?;
        self.dpy.map_window(win)?;
        self.dpy.ungrab_server()?;
        self.sequences_to_ignore
            .push(Reverse(cookie.sequence_number() as u16));

        self.monitors[self.selected_monitor]
            .clients
            .push(Client::new(self.dpy, win, geometry, self.selected_monitor)?);

        return Ok(());
    }

    fn map_request_handler(&mut self, event: MapRequestEvent) -> Result<(), Box<dyn Error>> {
        self.manage_window(event.window, &self.dpy.get_geometry(event.window)?.reply()?)?;

        return Ok(());
    }

    fn motion_notify_handler(&mut self, event: MotionNotifyEvent) -> Result<(), Box<dyn Error>> {
        if event.event == self.root {
            return Ok(());
        }

        return Ok(());
    }

    fn move_mouse(&mut self) {
        println!("called");
        let selclient = self.selmon().selclient();

        if selclient.is_none() {
            return;
        }

        let client = selclient.unwrap().clone();

        if client.fullscreen {
            return;
        }

        let reply = unwrap_or_return_r!(
            unwrap_or_return_r!(
                self.dpy.grab_pointer(
                    false,
                    self.root,
                    MOUSEMASK!(),
                    GrabMode::ASYNC,
                    GrabMode::ASYNC,
                    XCB_WINDOW_NONE,
                    XCB_CURSOR_NONE,
                    CURRENT_TIME,
                ),
                ()
            )
            .reply(),
            ()
        );

        if reply.status != GrabStatus::SUCCESS {
            return;
        }

        let pointer = unwrap_or_return_r!(
            unwrap_or_return_r!(self.dpy.query_pointer(self.root), ()).reply(),
            ()
        );

        let mx = pointer.root_x;
        let my = pointer.root_y;
        let mut ungrab: bool = false;

        loop {
            let _ = self.dpy.flush();

            let mut event_res = self.dpy.wait_for_event();

            while event_res.is_err() {
                event_res = self.dpy.wait_for_event();
                let _ = self.dpy.flush();
            }

            let event = event_res.unwrap();

            match event {
                Event::ConfigureRequest(_) | Event::MapRequest(_) => {
                    unwrap_or_return_r!(self.handle_event(event), ())
                }
                Event::MotionNotify(me) => {
                    let xw = client.x + me.root_x - mx;
                    let yh = client.y + me.root_y - my;
                    let _ = self.dpy.configure_window(
                        client.window_id,
                        &ConfigureWindowAux::new().x(xw as i32).y(yh as i32),
                    );
                }
                Event::KeyPress(_)
                | Event::KeyRelease(_)
                | Event::ButtonPress(_)
                | Event::ButtonRelease(_) => ungrab = true,
                _ => (),
            };

            if ungrab {
                break;
            }
        }

        println!("Ungrabbed pointer");
        let _ = self.dpy.ungrab_pointer(CURRENT_TIME);
    }

    fn query_clients(&mut self) -> Result<(), ReplyOrIdError> {
        let tree_reply = self.dpy.query_tree(self.root)?.reply()?;

        let mut cookies = Vec::with_capacity(tree_reply.children_len() as usize);
        for win in tree_reply.children {
            let attr = self.dpy.get_window_attributes(win)?;
            let g = self.dpy.get_geometry(win)?;
            cookies.push((win, attr, g));
        }

        for (win, attr, geometry) in cookies {
            if let (Ok(attr), Ok(g)) = (attr.reply(), geometry.reply()) {
                if !attr.override_redirect && attr.map_state != MapState::UNMAPPED {
                    self.manage_window(win, &g)?;
                }
            }
        }

        return Ok(());
    }

    fn query_monitors(&mut self) -> Result<(), ReplyError> {
        let reply = self
            .dpy
            .randr_get_screen_resources_current(self.root)?
            .reply()?;
        // let len: usize = reply.length as usize;
        let len = reply.outputs.len();
        let outputs = reply.outputs;

        for i in 0..len {
            let output = self
                .dpy
                .randr_get_output_info(outputs[i], reply.config_timestamp)?
                .reply()?;

            if output.crtc == x11rb::NONE || output.connection == randr_Connection::DISCONNECTED {
                continue;
            }

            let crtc_info = self
                .dpy
                .randr_get_crtc_info(output.crtc, reply.config_timestamp)?
                .reply()?;

            self.monitors.push(Monitor::new(crtc_info, i));
        }

        return Ok(());
    }

    fn rect_to_monitor(&self, x: i16, y: i16, w: u16, h: u16) -> &Monitor {
        let mut area: i16 = 0;
        let mut ret: &Monitor = self.selmon();

        for monitor in &self.monitors {
            let a = intersect(x, y, w, h, monitor);
            if a > area {
                area = a;
                ret = monitor;
            }
        }

        return ret;
    }

    // TODO: Add something here if pendingexpose is ever used
    //
    // fn refresh(&mut self) -> Result<(), Box<dyn Error>> {
    //     while let Some(&winid) = self.pending_expose.iter().next() {
    //         self.pending_expose.remove(&winid);

    //         if let Some(c) = self.find_client_by_window_id(winid) {
    //             if let Err()
    //         }
    //     }
    // }

    fn run(&mut self) -> Result<(), Box<dyn Error>> {
        loop {
            self.dpy.flush()?;

            let event = self.dpy.wait_for_event()?;
            let mut event_opt = Some(event);

            while let Some(event) = event_opt {
                let mut should_ignore = false;
                // Borrowed from https://github.com/psychon/x11rb/blob/master/x11rb/examples/simple_window_manager.rs#L253
                if let Some(seqno) = event.wire_sequence_number() {
                    while let Some(&Reverse(to_ignore)) = self.sequences_to_ignore.peek() {
                        if to_ignore.wrapping_sub(seqno) <= u16::max_value() / 2 {
                            should_ignore = to_ignore == seqno;
                            break;
                        }

                        self.sequences_to_ignore.pop();
                    }
                }

                if !should_ignore {
                    self.handle_event(event)?;
                }

                event_opt = self.dpy.poll_for_event()?;
            }
        }
    }

    fn screen_change_handler(&mut self, _: ScreenChangeNotifyEvent) -> Result<(), Box<dyn Error>> {
        // Need to handle connection, disconnection, their clients, etc...
        self.query_monitors()?;

        return Ok(()); // Unimplemented
    }

    fn selmon(&self) -> &Monitor {
        return &self.monitors[self.selected_monitor];
    }

    fn unmap_notify_handler(&mut self, event: UnmapNotifyEvent) -> Result<(), Box<dyn Error>> {
        let root = self.root;
        let dpy = self.dpy;

        for monitor in &mut self.monitors {
            monitor.clients.retain(|c| {
                if c.window_id != event.window {
                    return true;
                }

                dpy.change_save_set(SetMode::DELETE, c.window_id).unwrap();
                dpy.reparent_window(c.window_id, root, c.x, c.y).unwrap();

                return false;
            });
        }

        return Ok(());
    }

    fn update_numlock(&self) -> Result<(), ReplyError> {
        unsafe {
            let raw_c: *mut xcb_connection_t = self.dpy.get_raw_xcb_connection() as _;

            let reply: *mut xcb_get_modifier_mapping_reply_t;
            let modmap: *mut xcb_keycode_t;
            let numlock_iter: KeycodeIter;

            reply = xcb_get_modifier_mapping_reply(
                raw_c,
                xcb_get_modifier_mapping_unchecked(raw_c),
                ptr::null_mut(),
            );

            modmap = xcb_get_modifier_mapping_keycodes(reply);
            numlock_iter = xcb_get_keycodes(self.dpy, keysyms::KEY_Num_Lock);
            let mut numlock_vec = Vec::<Keycode>::new();
            numlock_vec.extend(numlock_iter);

            for i in 0u8..8u8 {
                for j in 0..(*reply).keycodes_per_modifier {
                    let keycode =
                        modmap.wrapping_add((i * (*reply).keycodes_per_modifier + j) as usize);

                    if u32::from(*keycode) == XCB_NO_SYMBOL {
                        continue;
                    }

                    for n in 0..numlock_vec.len() {
                        let numlock_code = numlock_vec[n];
                        if numlock_code == *keycode {
                            println!("found numlock {}", 1 << i);
                            NUM_LOCK_MASK = 1 << i;
                            break;
                        }
                    }
                }
            }
        }

        return Ok(());
    }
}

fn bitwise_mask(masks: &[ModMask]) -> ModMask {
    let mut ret: ModMask = masks[0];

    for i in 1..masks.len() {
        ret |= masks[i];
    }

    return ret;
}

fn intersect(x: i16, y: i16, w: u16, h: u16, m: &Monitor) -> i16 {
    let iw: i16 = w as i16;
    let ih: i16 = h as i16;
    let miwx: i16 = m.wx as i16;
    let miwy: i16 = m.wy as i16;
    let miww: i16 = m.ww as i16;
    let miwh: i16 = m.wh as i16;

    return max(0, min(x + iw, miwx + miww) - max(x, miwx))
        * max(0, min(y + ih, miwy + miwh) - max(y, miwy));
}

fn spawn(cmd: &str, args: Vec<&str>) {
    let mut command = Command::new(cmd);

    for arg in args {
        command.arg(arg);
    }

    command.stdout(Stdio::null()).stderr(Stdio::null());

    let _ = command.spawn();
}

fn print_if_err<T>(arg: Result<T, Box<dyn Error>>) -> Result<T, Box<dyn Error>> {
    if let Err(e) = &arg {
        eprintln!("{:#?}", e);
    }

    return arg;
}

fn vec_i8_into_u8(v: Vec<i8>) -> Vec<u8> {
    // ideally we'd use Vec::into_raw_parts, but it's unstable,
    // so we have to do it manually:

    // first, make sure v's destructor doesn't free the data
    // it thinks it owns when it goes out of scope
    let mut v = std::mem::ManuallyDrop::new(v);

    // then, pick apart the existing Vec
    let p = v.as_mut_ptr();
    let len = v.len();
    let cap = v.capacity();

    // finally, adopt the data into a new Vec
    unsafe { Vec::from_raw_parts(p as *mut u8, len, cap) }
}

fn wmify(dpy: &XCBConnection, screen: &Screen) -> Result<(), Box<dyn Error>> {
    let change = ChangeWindowAttributesAux::default().event_mask(
        EventMask::SUBSTRUCTURE_REDIRECT
            | EventMask::STRUCTURE_NOTIFY
            | EventMask::SUBSTRUCTURE_NOTIFY
            | EventMask::PROPERTY_CHANGE,
    );

    let res = dpy.change_window_attributes(screen.root, &change)?.check();
    if let Err(ReplyError::X11Error(ref error)) = res {
        if error.error_kind == ErrorKind::Access {
            eprintln!("Unable to set attributes on the root window, are you running another window manager?");
            std::process::exit(1);
        }

        res?;
    }

    return Ok(());
}

fn xcb_get_name_from_keycode(dpy: &XCBConnection, keycode: Keycode) -> Option<String> {
    let keysym = unwrap_or_return_o!(xcb_get_keysym(dpy, keycode), None);

    let mut buf = vec![0i8; 0];
    let clen: usize = unwrap_or_return_r!(
        unsafe { xkb_keysym_get_name(keysym, buf.as_mut_ptr(), buf.len()) }.try_into(),
        None
    );

    buf = vec![0i8; clen + 1];
    let clen: usize = unwrap_or_return_r!(
        unsafe { xkb_keysym_get_name(keysym, buf.as_mut_ptr(), buf.len()) }.try_into(),
        None
    );

    buf.truncate(clen);
    return match String::from_utf8(vec_i8_into_u8(buf)) {
        Ok(s) => Some(s),
        Err(_) => None,
    };
}

fn xcb_get_keycodes(dpy: &XCBConnection, keysym: Keysym) -> KeycodeIter {
    let c = to_base!(dpy);
    let keysyms = KeySymbols::new(&c);

    let ret = keysyms.get_keycode(keysym);

    drop(keysyms);
    forget(c);

    return ret;
}

fn xcb_get_keysym(dpy: &XCBConnection, keycode: Keycode) -> Option<Keysym> {
    let c = to_base!(dpy);
    let keysyms = KeySymbols::new(&c);

    let ret = Some(keysyms.get_keysym(keycode, 0));

    drop(keysyms);
    forget(c);

    return ret;
}

fn main() -> Result<(), Box<dyn Error>> {
    let (dpy, screen_num) = x11rb::xcb_ffi::XCBConnection::connect(None)?;
    let screen = &dpy.setup().roots[screen_num];

    println!("Welcome to ppebwm!!");

    print_if_err(wmify(&dpy, screen))?;
    println!("Became wm successfully!!");
    let mut state = print_if_err(State::new(&dpy, screen_num))?;
    print_if_err(state.run())?;
    return print_if_err(state.cleanup());
}
