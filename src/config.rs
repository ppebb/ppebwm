use std::process::exit;

use x11rb::protocol::xproto::{ButtonIndex, ModMask};
use xkbcommon::xkb::keysyms;

use crate::{spawn, Click, State};

pub struct KeyBind {
    pub modifiers: &'static [ModMask],
    pub key: u32,
    pub func: fn(state: &State),
}

impl KeyBind {
    const fn new(modifiers: &'static [ModMask], key: u32, func: fn(state: &State)) -> KeyBind {
        return KeyBind {
            modifiers,
            key,
            func,
        };
    }

    pub fn call(&self, state: &State) {
        (self.func)(state);
    }
}

pub struct MouseBind {
    pub click: Click,
    pub modifiers: &'static [ModMask],
    pub button: ButtonIndex,
    pub func: fn(state: &mut State),
}

impl MouseBind {
    const fn new(
        click: Click,
        modifiers: &'static [ModMask],
        button: ButtonIndex,
        func: fn(state: &mut State),
    ) -> MouseBind {
        return MouseBind {
            click,
            modifiers,
            button,
            func,
        };
    }
}

pub const KEYBINDS: [KeyBind; 4] = [
    KeyBind::new(&[ModMask::M4], keysyms::KEY_p, |_| {
        spawn("sh", vec!["-c", "dmenu_run", "-m", "0"]);
    }),
    KeyBind::new(&[ModMask::M4, ModMask::SHIFT], keysyms::KEY_t, |_| {
        spawn("alacritty", vec![]);
    }),
    KeyBind::new(
        &[ModMask::M4, ModMask::SHIFT, ModMask::CONTROL],
        keysyms::KEY_q,
        |_| {
            exit(0);
        },
    ),
    KeyBind::new(&[ModMask::M4, ModMask::SHIFT], keysyms::KEY_c, |s| {
        if let Some(selclient) = s.selmon().selclient() {
            selclient.kill(s);
        }
    }),
];

pub const MOUSEBINDS: [MouseBind; 1] = [MouseBind::new(
    Click::ClientWin,
    &[ModMask::M4],
    ButtonIndex::M1,
    |s| s.move_mouse(),
)];
