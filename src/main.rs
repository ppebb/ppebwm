use std::error::Error;

use x11rb::{
    connection::Connection,
    protocol::{
        xproto::{
            ChangeWindowAttributesAux, ConnectionExt, EventMask, Screen, BUTTON_PRESS_EVENT,
            BUTTON_RELEASE_EVENT, CLIENT_MESSAGE_EVENT, DESTROY_NOTIFY_EVENT, ENTER_NOTIFY_EVENT,
            FOCUS_IN_EVENT, FOCUS_OUT_EVENT, KEY_PRESS_EVENT, MAP_REQUEST_EVENT,
            MOTION_NOTIFY_EVENT,
        },
        ErrorKind, Event,
    },
    rust_connection::{ReplyError, RustConnection},
};

const NUMTAGS: usize = 9;

struct Client {
    name: String,
    x: u16, // Window geometries
    y: u16,
    w: u16,
    h: u16,
    cfact: f32, // The weight of the window within the parent or child stack
    tags: u32,  // bitfield containing all of the tags to display on
    window_id: u64,
    next: *const Client,
    monitor: *const Monitor,
}

struct TagInfo {
    num_masters: usize,
    mfact: f32, // The weight of the master window
}

struct Monitor {
    clients: *mut Client, // Linked list of clients for each monitor
    selected_client: *mut Client,
    selected_tags: u16,
    tag_info: [TagInfo; NUMTAGS],
    next: *mut Monitor,
    num: usize,
    mx: u16, // Screen geometries
    my: u16,
    mw: u16,
    mh: u16,
    wx: u16, // Window area geometries
    wy: u16,
    ww: u16,
    wh: u16,
}

struct State<'a> {
    dpy: &'a RustConnection,
    root: u32,
    //screen_num: usize,
    monitors: *mut Monitor, // Linked list of monitors
    selected_monitor: *mut Monitor,
}

// Handler func can just return true to kill the wm
type HandlerFunc = fn(&State, Event) -> Result<bool, Box<dyn Error>>;

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
        _ => None,
    };
}

fn client_message_handler(state: &State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(true); // Returns true because the event probably means we need to close the wm
}

fn motion_notify_handler(state: &State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn enter_notify_handler(state: &State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn destroy_notify_handler(state: &State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn key_press_handler(state: &State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn button_press_handler(state: &State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn button_release_handler(state: &State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn map_request_handler(state: &State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn focus_in_handler(state: &State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn focus_out_handler(state: &State, _: Event) -> Result<bool, Box<dyn Error>> {
    return Ok(false); // Unimplemented
}

fn run(dpy: &RustConnection, screen: &Screen) -> Result<(), Box<dyn Error>> {
    let state = State {
        dpy,
        root: screen.root,
        monitors: None,
        selected_monitor: None,
    };

    loop {
        dpy.flush()?;

        let mut event = dpy.wait_for_event()?;
        let mut handler_opt = get_handler_func(event.raw_response_type());

        while let Some(handler) = handler_opt {
            match (handler)(&state, event) {
                Ok(should_ret) => match should_ret {
                    true => return Ok(()),
                    false => {
                        let event_opt = dpy.poll_for_event()?;
                        if let Some(event_unwapped) = event_opt {
                            event = event_unwapped;
                            handler_opt = get_handler_func(event.raw_response_type());
                        } else {
                            break;
                        }
                    }
                },
                Err(e) => return Err(e),
            }
        }
    }
}

fn setup(dpy: &RustConnection, screen: &Screen) -> Result<(), ReplyError> {
    let change = ChangeWindowAttributesAux::default().event_mask(
        EventMask::SUBSTRUCTURE_REDIRECT
            | EventMask::STRUCTURE_NOTIFY
            | EventMask::SUBSTRUCTURE_NOTIFY
            | EventMask::PROPERTY_CHANGE,
    );

    let res = dpy.change_window_attributes(screen.root, &change)?.check();
    if let Err(ReplyError::X11Error(ref error)) = res {
        if error.error_kind == ErrorKind::Access {
            std::process::exit(1);
        } else {
            res
        }
    } else {
        res
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let (dpy, screen_num) = x11rb::connect(None).unwrap();
    let screen = &dpy.setup().roots[screen_num];

    setup(&dpy, screen)?;
    run(&dpy, screen)
}
