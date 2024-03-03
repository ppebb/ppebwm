use std::{cmp::Reverse, collections::BinaryHeap, error::Error, ptr::null_mut};

use x11rb::{
    connection::Connection,
    errors::ReplyOrIdError,
    protocol::{
        randr::{
            Connection as randr_Connection, ConnectionExt as randr_ConnectionExt, GetCrtcInfoReply,
            SCREEN_CHANGE_NOTIFY_EVENT,
        },
        xproto::{
            Atom, AtomEnum, ChangeWindowAttributesAux, ConnectionExt as xproto_ConnectionExt,
            CreateGCAux, CreateWindowAux, EventMask, Gcontext, GetGeometryReply, MapState, Screen,
            SetMode, Window, WindowClass, BUTTON_PRESS_EVENT, BUTTON_RELEASE_EVENT,
            CLIENT_MESSAGE_EVENT, DESTROY_NOTIFY_EVENT, ENTER_NOTIFY_EVENT, FOCUS_IN_EVENT,
            FOCUS_OUT_EVENT, KEY_PRESS_EVENT, MAP_REQUEST_EVENT, MOTION_NOTIFY_EVENT,
        },
        ErrorKind, Event,
    },
    rust_connection::{ReplyError, RustConnection},
    COPY_DEPTH_FROM_PARENT,
};

trait LinkableListable {
    fn add_next(self, element: Self);
}

const NUMTAGS: usize = 9;

struct Client {
    name: String,
    x: i16, // Window geometries
    y: i16,
    w: u16,
    h: u16,
    cfact: f32, // The weight of the window within the parent or child stack
    tags: u32,  // bitfield containing all of the tags to display on
    window_id: u32,
    frame_window_id: u32,
    next: *mut Client,
    monitor: *const Monitor,
}

impl Client {
    fn new(
        dpy: &RustConnection,
        win: Window,
        frame_win: Window,
        geometry: &GetGeometryReply,
        monitor: *mut Monitor,
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
            frame_window_id: frame_win,
            next: null_mut(),
            monitor,
        });
    }
}

impl LinkableListable for *mut Client {
    fn add_next(self, element: *mut Client) {
        unsafe {
            let mut next_elem: *mut Client = self as *mut Client;

            while !(*next_elem).next.is_null() {
                next_elem = (*next_elem).next;
            }

            (*next_elem).next = element;
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
    arrange: fn(*mut Monitor),
}

struct Monitor {
    clients: *mut Client, // Linked list of clients for each monitor
    selected_client: *mut Client,
    selected_tags: u16,
    tag_info: [TagInfo; NUMTAGS],
    next: *mut Monitor,
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
            clients: null_mut(),
            selected_client: null_mut(),
            selected_tags: 0,
            tag_info: [TagInfo {
                num_masters: 0,
                mfact: 0.0,
                layout: 0,
            }; NUMTAGS],
            next: null_mut(),
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
}

struct State<'a> {
    dpy: &'a RustConnection,
    screen: &'a Screen,
    root: u32,
    black_gc: Gcontext,
    monitors: *mut Monitor, // Linked list of monitors
    selected_monitor: *mut Monitor,
    protocols: Atom,
    delete_window: Atom,
    sequences_to_ignore: BinaryHeap<Reverse<u16>>,
}

impl<'a> State<'a> {
    fn new(dpy: &'a RustConnection, screen_num: usize) -> Result<State<'a>, ReplyOrIdError> {
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

        let protocols = dpy.intern_atom(false, b"WM_PROTOCOLS")?;
        let delete_window = dpy.intern_atom(false, b"WM_DELETE_WINDOW")?;

        let mut state = State {
            dpy,
            screen,
            root: screen.root,
            black_gc,
            monitors: null_mut(),
            selected_monitor: null_mut(),
            protocols: protocols.reply()?.atom,
            delete_window: delete_window.reply()?.atom,
            sequences_to_ignore: Default::default(),
        };

        state.monitors = state.query_monitors()?;
        state.selected_monitor = state.monitors; // This is just the pointer to the first monitor
        state.query_clients()?;
        unsafe {
            (*state.monitors).selected_client = (*state.monitors).clients;
        }

        return Ok(state);
    }

    fn query_monitors(&mut self) -> Result<*mut Monitor, ReplyError> {
        let reply = self
            .dpy
            .randr_get_screen_resources_current(self.root)?
            .reply()?;
        let len: usize = reply.length as usize;
        let outputs = reply.outputs;

        let mut ret: *mut Monitor = null_mut();

        // This should never be zero. Might segfault tho lol
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

            // This will probably segfault
            let next = &mut Monitor::new(crtc_info, i) as *mut _;
            if !ret.is_null() {
                unsafe {
                    (*ret).next = next;
                }
            }
            ret = next;
        }

        return Ok(ret);
    }

    fn find_client_by_window_id(&self, win: Window) -> Option<&Client> {
        unsafe {
            let monitor = self.monitors;
            while !monitor.is_null() {
                let client = (*monitor).clients;

                while !client.is_null() {
                    if (*client).window_id == win || (*client).frame_window_id == win {
                        return Some(&(*client));
                    }
                }
            }

            return None;
        }
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

    fn manage_window(
        &mut self,
        win: Window,
        geometry: &GetGeometryReply,
    ) -> Result<(), ReplyOrIdError> {
        assert!(self.find_client_by_window_id(win).is_none());

        let frame_win = self.dpy.generate_id()?;
        let win_aux = CreateWindowAux::new()
            .event_mask(
                EventMask::EXPOSURE
                    | EventMask::SUBSTRUCTURE_NOTIFY
                    | EventMask::BUTTON_PRESS
                    | EventMask::BUTTON_RELEASE
                    | EventMask::POINTER_MOTION
                    | EventMask::ENTER_WINDOW,
            )
            .background_pixel(self.screen.white_pixel);

        self.dpy.create_window(
            COPY_DEPTH_FROM_PARENT,
            frame_win,
            self.root,
            geometry.x,
            geometry.y,
            geometry.width,
            geometry.height,
            1,
            WindowClass::INPUT_OUTPUT,
            0,
            &win_aux,
        )?;

        self.dpy.grab_server()?;
        self.dpy.change_save_set(SetMode::INSERT, win)?;
        let cookie = self.dpy.reparent_window(win, frame_win, 0, 0)?;
        self.dpy.map_window(win)?;
        self.dpy.map_window(frame_win)?;
        self.dpy.ungrab_server()?;
        self.sequences_to_ignore
            .push(Reverse(cookie.sequence_number() as u16));

        unsafe {
            let clients = (*self.selected_monitor).clients;
            let new_client =
                &mut Client::new(self.dpy, win, frame_win, geometry, self.selected_monitor)?
                    as *mut _;
            if clients.is_null() {
                (*self.selected_monitor).clients = new_client;
            } else {
                clients.add_next(new_client);
            }
        }

        return Ok(());
    }
}

// Handler func can just return true to kill the wm
type HandlerFunc = fn(&mut State, Event) -> Result<bool, Box<dyn Error>>;

fn get_handler_func(opcode: u8) -> Option<HandlerFunc> {
    return match opcode {
        CLIENT_MESSAGE_EVENT => Some(client_message_handler),
        MOTION_NOTIFY_EVENT => Some(motion_notify_handler),
        ENTER_NOTIFY_EVENT => Some(enter_notify_handler),
        DESTROY_NOTIFY_EVENT => Some(destroy_notify_handler),
        BUTTON_PRESS_EVENT => Some(button_press_handler),
        BUTTON_RELEASE_EVENT => Some(button_release_handler),
        KEY_PRESS_EVENT => Some(key_press_handler),
        MAP_REQUEST_EVENT => Some(map_request_handler),
        FOCUS_IN_EVENT => Some(focus_in_handler),
        FOCUS_OUT_EVENT => Some(focus_out_handler),
        SCREEN_CHANGE_NOTIFY_EVENT => Some(screen_change_handler),
        _ => None,
    };
}

// TODO: Move to be implemented on state??
fn client_message_handler(state: &mut State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(true); // Returns true because the event probably means we need to close the wm
}

fn motion_notify_handler(state: &mut State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn enter_notify_handler(state: &mut State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn destroy_notify_handler(state: &mut State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn key_press_handler(state: &mut State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn button_press_handler(state: &mut State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn button_release_handler(state: &mut State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn map_request_handler(state: &mut State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn focus_in_handler(state: &mut State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn focus_out_handler(state: &mut State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn screen_change_handler(state: &mut State, _: Event) -> Result<bool, Box<dyn Error>> {
    // Need to handle connection, disconnection, their clients, etc...
    state.monitors = state.query_monitors()?;

    return Ok(false); // Unimplemented
}

fn run(dpy: &RustConnection, mut state: State) -> Result<(), Box<dyn Error>> {
    loop {
        dpy.flush()?;

        let mut event = dpy.wait_for_event()?;
        let mut handler_opt = get_handler_func(event.raw_response_type());

        while let Some(handler) = handler_opt {
            let mut should_ignore = false;
            // Borrowed from https://github.com/psychon/x11rb/blob/master/x11rb/examples/simple_window_manager.rs#L253
            if let Some(seqno) = event.wire_sequence_number() {
                while let Some(&Reverse(to_ignore)) = state.sequences_to_ignore.peek() {
                    if to_ignore.wrapping_sub(seqno) <= u16::max_value() / 2 {
                        should_ignore = to_ignore == seqno;
                        break;
                    }

                    state.sequences_to_ignore.pop();
                }
            }

            if !should_ignore {
                match (handler)(&mut state, event) {
                    Ok(should_ret) => {
                        if should_ret {
                            return Ok(());
                        }
                    }
                    Err(e) => return Err(e),
                }
            }

            let event_opt = dpy.poll_for_event()?;
            if let Some(event_unwapped) = event_opt {
                event = event_unwapped;
                handler_opt = get_handler_func(event.raw_response_type());
            } else {
                break;
            }
        }
    }
}

fn setup<'a>(dpy: &'a RustConnection, screen: &Screen) -> Result<State<'a>, Box<dyn Error>> {
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

        return Err(error.error_code.to_string().into());
    }

    return Ok(State::new(dpy, screen.root as usize)?);
}

fn main() -> Result<(), Box<dyn Error>> {
    let (dpy, screen_num) = x11rb::connect(None).unwrap();
    let screen = &dpy.setup().roots[screen_num];

    let state = setup(&dpy, screen)?;
    return run(&dpy, state);
}
