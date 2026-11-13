//! av's own colors: the website's surfaces and accents (websites/Asciiverse
//! css/site.css), green on black with amber, as truecolor.

use ratatui::style::Color;

pub const BG: Color = Color::Rgb(0x0a, 0x0d, 0x11);
pub const PANEL: Color = Color::Rgb(0x10, 0x15, 0x1b);
pub const PANEL_HI: Color = Color::Rgb(0x1d, 0x25, 0x30);
pub const LINE: Color = Color::Rgb(0x2e, 0x39, 0x46);
pub const TEXT: Color = Color::Rgb(0xbc, 0xc4, 0xcc);
pub const TEXT_HI: Color = Color::Rgb(0xe6, 0xea, 0xee);
pub const DIM: Color = Color::Rgb(0x8a, 0x94, 0x9e);
pub const FAINT: Color = Color::Rgb(0x5b, 0x65, 0x70);
pub const GREEN: Color = Color::Rgb(0x5f, 0xbf, 0x45);
pub const GREEN_HI: Color = Color::Rgb(0x7f, 0xdc, 0x5e);
pub const AMBER: Color = Color::Rgb(0xff, 0xb0, 0x00);
pub const AMBER_SOFT: Color = Color::Rgb(0xe9, 0xc2, 0x7a);

// list colors from the design: installed, update available, not installed
pub const INSTALLED: Color = Color::Rgb(0x7f, 0xdc, 0x5e);
pub const UPDATE: Color = Color::Rgb(0xc0, 0x3a, 0x42);
pub const NOT_INSTALLED: Color = Color::Rgb(0xe6, 0xea, 0xee);
