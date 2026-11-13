//! The built-in `.cast` player: asciicast v2 and v3 parsing, a timing loop
//! honoring idle_time_limit, avt as the terminal emulator, and drawing avt's
//! cell grid into a Ratatui buffer with the Asciiverse palette.

use anyhow::{Context, Result, bail};
use avt::Vt;
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};

/// The 16 basic colors every cast plays with, in av and on the website
/// (`.asciinema-player-theme-asciiverse` in the website's site.css).
/// 256-color and RGB output keeps its exact colors.
pub const PALETTE: [(u8, u8, u8); 16] = [
    // black is the background colour itself: curses games mix explicit
    // black and the default background, and a different black showed as
    // ghost patches (worm's trail, nstars' squares)
    (0x0a, 0x0c, 0x0e), (0xd9, 0x47, 0x4f), (0x3e, 0x9b, 0x4f), (0xe3, 0xa3, 0x3b),
    (0x3a, 0x78, 0xc9), (0xa8, 0x5c, 0xc4), (0x35, 0xa8, 0xa0), (0xc4, 0xc9, 0xce),
    (0x4a, 0x52, 0x5a), (0xff, 0x6b, 0x6b), (0x7b, 0xdc, 0x62), (0xff, 0xc8, 0x57),
    (0x6a, 0xa6, 0xff), (0xd6, 0x8c, 0xf0), (0x6f, 0xe0, 0xd4), (0xf2, 0xf4, 0xf5),
];
pub const FG: Color = Color::Rgb(0xc9, 0xd1, 0xcb);
pub const BG: Color = Color::Rgb(0x0a, 0x0c, 0x0e);

pub struct Cast {
    pub cols: usize,
    pub rows: usize,
    /// (seconds from start, after idle limiting; output text)
    pub events: Vec<(f64, String)>,
    pub duration: f64,
}

impl Cast {
    pub fn parse(src: &str) -> Result<Cast> {
        const MAX_COLS: u64 = 400;
        const MAX_ROWS: u64 = 200;
        let mut lines = src.lines().filter(|l| !l.trim().is_empty());
        let head: serde_json::Value =
            serde_json::from_str(lines.next().context("empty cast")?).context("cast header is not JSON")?;
        let version = head["version"].as_u64().unwrap_or(0);
        let (cols, rows) = match version {
            2 => (head["width"].as_u64(), head["height"].as_u64()),
            3 => (head["term"]["cols"].as_u64(), head["term"]["rows"].as_u64()),
            v => bail!("asciicast version {v} is not supported"),
        };
        let (cols, rows) = (cols.unwrap_or(80), rows.unwrap_or(24));
        // casts are not pinned by a checksum: a size avt can't take, or one
        // too big to draw, would bring av down rather than one preview
        if !(1..=MAX_COLS).contains(&cols) || !(1..=MAX_ROWS).contains(&rows) {
            bail!("a {cols}x{rows} terminal is not one av can show");
        }
        let (cols, rows) = (cols as usize, rows as usize);
        // no limit unless a positive one: a negative one would panic in clamp
        let idle = head["idle_time_limit"].as_f64().filter(|i| *i > 0.0).unwrap_or(f64::INFINITY);
        let mut events = Vec::new();
        let (mut t, mut prev_abs) = (0.0f64, 0.0f64);
        for line in lines {
            let Ok(serde_json::Value::Array(ev)) = serde_json::from_str::<serde_json::Value>(line) else { continue };
            let (Some(time), Some(code)) = (ev.first().and_then(|v| v.as_f64()), ev.get(1).and_then(|v| v.as_str()))
            else {
                continue;
            };
            // v2 stores absolute times, v3 intervals since the previous event
            let gap = if version == 2 { time - prev_abs } else { time };
            prev_abs = time;
            t += gap.clamp(0.0, idle);
            if code == "o"
                && let Some(data) = ev.get(2).and_then(|v| v.as_str())
            {
                events.push((t, data.to_string()));
            }
        }
        Ok(Cast { cols, rows, events, duration: t })
    }
}

pub struct Player {
    pub cast: Cast,
    vt: Vt,
    next: usize,
    pub pos: f64,
    pub playing: bool,
}

impl Player {
    pub fn new(cast: Cast) -> Self {
        let vt = Vt::new(cast.cols, cast.rows);
        Player { cast, vt, next: 0, pos: 0.0, playing: true }
    }

    /// Jump to a time; rewinding replays from the start (casts are small enough).
    pub fn seek(&mut self, t: f64) {
        if t < self.pos {
            self.vt = Vt::new(self.cast.cols, self.cast.rows);
            self.next = 0;
        }
        self.pos = t;
        while self.next < self.cast.events.len() && self.cast.events[self.next].0 <= t {
            self.vt.feed_str(&self.cast.events[self.next].1);
            self.next += 1;
        }
    }

    /// Advance by dt seconds of wall time, looping at the end.
    pub fn tick(&mut self, dt: f64) {
        if !self.playing {
            return;
        }
        let t = self.pos + dt;
        if t > self.cast.duration + 2.0 {
            self.seek(0.0);
        } else {
            self.seek(t);
        }
    }

    /// Draw the terminal grid into `area`, cropped from the top-left if it does not fit.
    pub fn render(&self, area: Rect, buf: &mut Buffer) {
        let blank = Style::default().fg(FG).bg(BG);
        buf.set_style(area, blank);
        for (y, line) in self.vt.view().enumerate() {
            if y as u16 >= area.height {
                break;
            }
            let mut x: u16 = 0;
            for cell in line.cells() {
                if cell.width() == 0 {
                    continue; // tail of a wide character, drawn by its head
                }
                if x >= area.width {
                    break;
                }
                if let Some(c) = buf.cell_mut((area.x + x, area.y + y as u16)) {
                    let pen = cell.pen();
                    let mut fg = pen.foreground().map(|c| color(c, pen.is_bold())).unwrap_or(FG);
                    let mut bg = pen.background().map(|c| color(c, false)).unwrap_or(BG);
                    if pen.is_inverse() {
                        std::mem::swap(&mut fg, &mut bg);
                    }
                    let mut m = Modifier::empty();
                    if pen.is_bold() { m |= Modifier::BOLD; }
                    if pen.is_faint() { m |= Modifier::DIM; }
                    if pen.is_italic() { m |= Modifier::ITALIC; }
                    if pen.is_underline() { m |= Modifier::UNDERLINED; }
                    let mut s = [0u8; 4];
                    c.set_symbol(cell.char().encode_utf8(&mut s)).set_style(Style::default().fg(fg).bg(bg).add_modifier(m));
                }
                x += cell.width() as u16;
            }
        }
    }
}

/// Indexed colors always become RGB, so the Asciiverse palette shows whatever
/// the player's own terminal theme is. Bold basic colors take the bright half.
fn color(c: avt::Color, bold: bool) -> Color {
    match c {
        avt::Color::RGB(rgb) => Color::Rgb(rgb.r, rgb.g, rgb.b),
        avt::Color::Indexed(i) => {
            let i = if bold && i < 8 { i + 8 } else { i };
            let (r, g, b) = xterm256(i);
            Color::Rgb(r, g, b)
        }
    }
}

fn xterm256(i: u8) -> (u8, u8, u8) {
    match i {
        0..=15 => PALETTE[i as usize],
        16..=231 => {
            let n = i - 16;
            let v = |k: u8| if k == 0 { 0 } else { 55 + k * 40 };
            (v(n / 36), v(n / 6 % 6), v(n % 6))
        }
        _ => {
            let g = 8 + (i - 232) * 10;
            (g, g, g)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_cast_av_cannot_show_is_refused_not_a_crash() {
        for head in [r#"{"version":2,"width":0,"height":0}"#, r#"{"version":3,"term":{"cols":100,"rows":0}}"#, r#"{"version":2,"width":65535,"height":65535}"#] {
            assert!(Cast::parse(&format!("{head}\n[0.5,\"o\",\"a\"]\n")).is_err(), "{head}");
        }
        // a negative idle limit means none, and the player can seek through it
        let c = Cast::parse("{\"version\":2,\"width\":10,\"height\":2,\"idle_time_limit\":-1}\n[0.5,\"o\",\"a\"]\n[5.5,\"o\",\"b\"]\n").unwrap();
        assert_eq!(c.duration, 5.5);
        Player::new(c).seek(3.0);
    }

    #[test]
    fn parses_v2_and_v3_with_idle_limit() {
        let v2 = "{\"version\":2,\"width\":10,\"height\":2,\"idle_time_limit\":1}\n[0.5,\"o\",\"a\"]\n[5.5,\"o\",\"b\"]\n";
        let c = Cast::parse(v2).unwrap();
        assert_eq!((c.cols, c.rows), (10, 2));
        assert_eq!(c.events.len(), 2);
        assert!((c.events[1].0 - 1.5).abs() < 1e-9);

        let v3 = "{\"version\":3,\"term\":{\"cols\":100,\"rows\":30},\"idle_time_limit\":2}\n[0.5,\"o\",\"a\"]\n[9,\"o\",\"b\"]\n[0.1,\"x\",\"0\"]\n";
        let c = Cast::parse(v3).unwrap();
        assert_eq!((c.cols, c.rows), (100, 30));
        assert!((c.events[1].0 - 2.5).abs() < 1e-9);
        assert!((c.duration - 2.6).abs() < 1e-9);
    }

    #[test]
    fn seek_replays_into_the_grid() {
        let v3 = "{\"version\":3,\"term\":{\"cols\":5,\"rows\":1}}\n[0.1,\"o\",\"hi\"]\n[0.1,\"o\",\"!\"]\n";
        let mut p = Player::new(Cast::parse(v3).unwrap());
        p.seek(0.15);
        assert_eq!(p.vt.text()[0].trim_end(), "hi");
        p.seek(1.0);
        assert_eq!(p.vt.text()[0].trim_end(), "hi!");
        p.seek(0.0);
        assert_eq!(p.vt.text()[0].trim_end(), "");
    }

    #[test]
    fn cube_colors() {
        assert_eq!(xterm256(16), (0, 0, 0));
        assert_eq!(xterm256(231), (255, 255, 255));
        assert_eq!(xterm256(255), (238, 238, 238));
    }
}
