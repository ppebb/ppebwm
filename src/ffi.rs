// use x11rb::xcb_ffi::XCBConnection;

// #[allow(non_camel_case_types)]
// #[repr(C)]
// union u {
//     cookie: u32,
//     reply: *mut u32,
// }

// #[allow(non_camel_case_types)]
// #[repr(C)]
// pub struct xcb_key_symbols_t {
//     connection: *mut XCBConnection,
//     tag: u32,
//     u: u,
// }

// #[allow(non_camel_case_types)]
// pub type xcb_keycode_t = u8;

// #[allow(non_camel_case_types)]
// type xcb_keysym_t = u32;

// #[link(name = "xcb-keysyms")]
// extern "C" {
//     pub fn xcb_key_symbols_alloc(connection: &xcb::Connection) -> Option<*mut xcb_key_symbols_t>;
//     pub fn xcb_key_symbols_free(keysyms: *mut xcb_key_symbols_t);
//     pub fn xcb_key_symbols_get_keycode(
//         keysyms: *mut xcb_key_symbols_t,
//         keysym: xcb_keysym_t,
//     ) -> xcb_keycode_t;
// }
