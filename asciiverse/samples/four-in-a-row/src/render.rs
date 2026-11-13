//! Drawing: the title menu and the board, scaled to the window.

use crate::app::{App, Match, Mode, Outcome, Screen, MENU, MENU_BOARD, MENU_LEVEL};
use crate::board::Variant;
use crate::engine::LEVELS;
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};

const BG: Color = Color::Rgb(8, 10, 18);
const FRAME: Color = Color::Rgb(28, 64, 190);
const FRAME_LIT: Color = Color::Rgb(44, 88, 225);
const HOLE: Color = Color::Rgb(10, 14, 32);
const TEXT: Color = Color::Rgb(200, 205, 220);
const DIM: Color = Color::Rgb(110, 116, 135);
const RED: Color = Color::Rgb(232, 58, 48);
const YELLOW: Color = Color::Rgb(250, 202, 40);

pub fn disc_colour(colour: u8) -> Color {
    if colour == 0 {
        RED
    } else {
        YELLOW
    }
}

pub fn name(colour: u8) -> &'static str {
    if colour == 0 {
        "Red"
    } else {
        "Yellow"
    }
}

fn mix(a: Color, b: Color, k: f32) -> Color {
    match (a, b) {
        (Color::Rgb(r1, g1, b1), Color::Rgb(r2, g2, b2)) => {
            let m = |x: u8, y: u8| (x as f32 + (y as f32 - x as f32) * k).round() as u8;
            Color::Rgb(m(r1, r2), m(g1, g2), m(b1, b2))
        }
        _ => a,
    }
}

/// One disc's picture, by cell size: rows of text drawn in the disc's colour
/// on the frame's colour.
fn glyph(cw: u16) -> &'static [&'static str] {
    match cw {
        6 => &[" ▄▄▄▄ ", "▐████▌", " ▀▀▀▀ "],
        4 => &["▗██▖", "▝██▘"],
        _ => &["▐█▌"],
    }
}

/// Where the board sits in the window.
#[derive(Clone, Copy, Debug)]
pub struct Geom {
    pub cw: u16,
    pub ch: u16,
    /// left edge of column 0's cell
    pub x: u16,
    /// top of the hover line, where the disc waits above the board
    pub hover_y: u16,
    /// top of the board's top row of cells
    pub board_y: u16,
    pub status_y: u16,
    pub numbers_y: u16,
    pub message_y: u16,
    pub help_y: u16,
}

const SIZES: [(u16, u16); 3] = [(6, 3), (4, 2), (3, 1)];

pub fn geom(area: Rect, v: Variant) -> Option<Geom> {
    for (cw, ch) in SIZES {
        let w = v.w as u16 * cw + 4;
        // status, gap, hover, cells, rim, numbers, message, help
        let h = 2 + ch + v.h as u16 * ch + 1 + 1 + 1 + 1;
        if w <= area.width && h <= area.height {
            let top = area.y + (area.height - h) / 2;
            let x = area.x + (area.width - (w - 4)) / 2;
            let hover_y = top + 2;
            let board_y = hover_y + ch;
            let numbers_y = board_y + v.h as u16 * ch + 1;
            return Some(Geom {
                cw,
                ch,
                x,
                hover_y,
                board_y,
                status_y: top,
                numbers_y,
                message_y: numbers_y + 1,
                help_y: numbers_y + 2,
            });
        }
    }
    None
}

/// The column under a mouse at x, if any.
pub fn col_at(g: &Geom, v: Variant, x: u16) -> Option<usize> {
    if x < g.x {
        return None;
    }
    let c = ((x - g.x) / g.cw) as usize;
    (c < v.w).then_some(c)
}

pub fn min_size(v: Variant) -> (u16, u16) {
    let (cw, ch) = SIZES[SIZES.len() - 1];
    (v.w as u16 * cw + 4, 2 + ch + v.h as u16 * ch + 4)
}

fn put(buf: &mut Buffer, area: Rect, x: u16, y: u16, s: &str, style: Style) {
    if y < area.y || y >= area.bottom() {
        return;
    }
    let mut x = x;
    for ch in s.chars() {
        if x >= area.right() {
            break;
        }
        if x >= area.x {
            buf[(x, y)].set_char(ch).set_style(style);
        }
        x += 1;
    }
}

fn centre(buf: &mut Buffer, area: Rect, y: u16, s: &str, style: Style) {
    let w = s.chars().count() as u16;
    let x = area.x + area.width.saturating_sub(w) / 2;
    put(buf, area, x, y, s, style);
}

fn fill(buf: &mut Buffer, r: Rect, bg: Color) {
    for y in r.y..r.bottom() {
        for x in r.x..r.right() {
            buf[(x, y)].set_char(' ').set_style(Style::default().bg(bg));
        }
    }
}

fn draw_disc(buf: &mut Buffer, area: Rect, cw: u16, x: u16, y: i32, fg: Color, bg: Color) {
    for (i, row) in glyph(cw).iter().enumerate() {
        let yy = y + i as i32;
        if yy < area.y as i32 || yy >= area.bottom() as i32 {
            continue;
        }
        let mut xx = x;
        for ch in row.chars() {
            if xx < area.right() {
                let cell = &mut buf[(xx, yy as u16)];
                if ch == ' ' {
                    cell.set_char(' ').set_style(Style::default().bg(bg));
                } else {
                    cell.set_char(ch).set_style(Style::default().fg(fg).bg(bg));
                }
            }
            xx += 1;
        }
    }
}

pub fn draw(app: &App, area: Rect, buf: &mut Buffer) {
    fill(buf, area, BG);
    match app.screen {
        Screen::Title => title(app, area, buf),
        Screen::Play => {
            if let Some(m) = &app.play {
                play(app, m, area, buf);
            }
        }
    }
}

/// Where each menu line is drawn, for the mouse.
pub fn menu_rows(area: Rect) -> Vec<Rect> {
    let top = area.y + area.height.saturating_sub(17) / 2 + 6;
    (0..MENU.len()).map(|i| Rect::new(area.x, top + i as u16 + if i >= MENU_LEVEL { 1 } else { 0 } + if i == MENU.len() - 1 { 1 } else { 0 }, area.width, 1)).collect()
}

fn title(app: &App, area: Rect, buf: &mut Buffer) {
    let top = area.y + area.height.saturating_sub(17) / 2;
    // four discs, lighting up one after another
    let n = ((app.t * 2.0) as usize) % 6;
    let x0 = area.x + area.width.saturating_sub(4 * 7) / 2;
    for i in 0..4u16 {
        let lit = (i as usize) < n.min(4);
        let c = disc_colour((i % 2) as u8);
        let fg = if lit { c } else { mix(c, BG, 0.75) };
        draw_disc(buf, area, 6, x0 + i * 7, top as i32, fg, BG);
    }
    centre(buf, area, top + 4, "F O U R   I N   A   R O W", Style::default().fg(TEXT).add_modifier(Modifier::BOLD));
    let rows = menu_rows(area);
    for (i, label) in MENU.iter().enumerate() {
        let sel = i == app.menu;
        let text = match i {
            MENU_LEVEL => format!("Computer   < {} >", app.level.name()),
            MENU_BOARD => format!("Board   < {} >", app.variant().label()),
            _ => label.to_string(),
        };
        let text = if sel { format!("▶ {text} ◀") } else { text };
        let style = if sel { Style::default().fg(YELLOW).add_modifier(Modifier::BOLD) } else { Style::default().fg(TEXT) };
        centre(buf, area, rows[i].y, &text, style);
    }
    let li = LEVELS.iter().position(|&l| l == app.level).unwrap_or(0);
    let (won, lost) = app.store.record[li];
    let y = rows[MENU.len() - 1].y + 2;
    if won + lost > 0 {
        centre(buf, area, y, &format!("Against {}: {won} won, {lost} lost", app.level.name()), Style::default().fg(DIM));
    }
    centre(buf, area, y + 1, "↑↓ choose   ←→ change   Enter start   M sound   Q quit", Style::default().fg(DIM));
}

fn play(app: &App, m: &Match, area: Rect, buf: &mut Buffer) {
    let v = m.board.v;
    let Some(g) = geom(area, v) else {
        let (w, h) = min_size(v);
        centre(buf, area, area.y + area.height / 2, &format!("Make the window at least {w}x{h}"), Style::default().fg(TEXT));
        return;
    };
    let bw = v.w as u16 * g.cw;
    let over = m.over.is_some();

    // status: who is who, and the score
    let label = |c: u8| -> String {
        match m.mode {
            Mode::Solo(l) if c == 1 => format!("Computer ({})", l.name()),
            Mode::Solo(_) => "You".to_string(),
            Mode::Duo => name(c).to_string(),
            Mode::Watch(l) => format!("{} ({})", name(c), l.name()),
        }
    };
    let left = format!("● {}  {}", label(0), m.score[0]);
    let right = format!("{}  {} ●", m.score[1], label(1));
    let mid = if m.score[2] > 0 { format!("   draws {}   ", m.score[2]) } else { "   ".to_string() };
    let total = (left.chars().count() + mid.chars().count() + right.chars().count()) as u16;
    let sx = area.x + area.width.saturating_sub(total) / 2;
    let turn = m.to_move();
    let st = |c: u8| {
        let s = Style::default().fg(disc_colour(c));
        if !over && turn == c {
            s.add_modifier(Modifier::BOLD)
        } else {
            s
        }
    };
    put(buf, area, sx, g.status_y, &left, st(0));
    put(buf, area, sx + left.chars().count() as u16, g.status_y, &mid, Style::default().fg(DIM));
    put(buf, area, sx + (left.chars().count() + mid.chars().count()) as u16, g.status_y, &right, st(1));

    // the frame: one column of margin each side, a rim below
    let frame = Rect::new(g.x - 2, g.board_y, bw + 4, v.h as u16 * g.ch + 1);
    fill(buf, frame.intersection(area), FRAME);
    // a stripe of light on the column the disc would go in
    let lit = if m.human_turn() { Some(m.cursor) } else { m.fall.map(|f| f.col) };
    for c in 0..v.w {
        let bg = if lit == Some(c) { FRAME_LIT } else { FRAME };
        let cx = g.x + c as u16 * g.cw;
        if bg != FRAME {
            fill(buf, Rect::new(cx, g.board_y, g.cw, v.h as u16 * g.ch).intersection(area), bg);
        }
        for r in 0..v.h {
            let y = g.board_y + (v.h - 1 - r) as u16 * g.ch;
            let fg = match m.colour_at(c, r) {
                None => HOLE,
                Some(col) => {
                    let base = disc_colour(col);
                    match &m.over {
                        Some(Outcome::Win(_, cells)) if cells.contains(&(c, r)) => {
                            let k = ((m.over_for * 6.0).sin() * 0.5 + 0.5) * 0.55;
                            mix(base, Color::Rgb(255, 255, 255), k)
                        }
                        Some(Outcome::Win(..)) => mix(base, HOLE, 0.5),
                        _ => base,
                    }
                }
            };
            draw_disc(buf, area, g.cw, cx, y as i32, fg, bg);
        }
    }
    // column numbers under the board
    for c in 0..v.w {
        let n = if c == 9 { "0".to_string() } else { (c + 1).to_string() };
        let cx = g.x + c as u16 * g.cw + (g.cw - 1) / 2;
        let style = if lit == Some(c) { Style::default().fg(TEXT) } else { Style::default().fg(DIM) };
        put(buf, area, cx, g.numbers_y, &n, style);
    }

    // the disc waiting above, or falling
    if let Some(f) = m.fall {
        let y = g.hover_y as f32 + f.y * g.ch as f32;
        let top_row = y.round() as i32;
        let x = g.x + f.col as u16 * g.cw;
        // over the board its background is the frame's
        let bg = if top_row >= g.board_y as i32 { FRAME_LIT } else { BG };
        draw_disc(buf, area, g.cw, x, top_row, disc_colour(f.colour), bg);
    } else if !over {
        let x = g.x + m.cursor as u16 * g.cw;
        let c = disc_colour(turn);
        let fg = if m.thinking() {
            mix(c, BG, ((app.t * 5.0).sin() * 0.5 + 0.5) * 0.6)
        } else {
            c
        };
        draw_disc(buf, area, g.cw, x, g.hover_y as i32, fg, BG);
    }

    // what is happening, and the keys
    let (msg, colour) = match &m.over {
        Some(Outcome::Win(c, _)) => {
            let s = match m.mode {
                Mode::Solo(_) if *c == 0 => "You win!".to_string(),
                Mode::Solo(_) => "The computer wins.".to_string(),
                _ => format!("{} wins!", name(*c)),
            };
            (s, disc_colour(*c))
        }
        Some(Outcome::Draw) => ("The board is full: a draw.".to_string(), TEXT),
        None if m.thinking() || (m.is_computer(turn) && m.fall.is_none()) => {
            let dots = ".".repeat(1 + ((app.t * 3.0) as usize) % 3);
            (format!("{} is thinking{dots:<3}", if matches!(m.mode, Mode::Solo(_)) { "The computer".to_string() } else { name(turn).to_string() }), disc_colour(turn))
        }
        None if m.fall.is_some() => (String::new(), TEXT),
        None => {
            let s = match m.mode {
                Mode::Solo(_) => "Your move".to_string(),
                _ => format!("{} to move", name(turn)),
            };
            (s, disc_colour(turn))
        }
    };
    centre(buf, area, g.message_y, &msg, Style::default().fg(colour).add_modifier(Modifier::BOLD));
    let help = if over {
        "Space next game   U take back   Esc menu"
    } else if matches!(m.mode, Mode::Watch(_)) {
        "Esc menu   M sound"
    } else {
        "←→ or mouse   Space drop   U take back   M sound   Esc menu"
    };
    centre(buf, area, g.help_y, help, Style::default().fg(DIM));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board::VARIANTS;

    #[test]
    fn every_board_fits_its_minimum_and_100x30() {
        for v in VARIANTS {
            let (w, h) = min_size(v);
            assert!(geom(Rect::new(0, 0, w, h), v).is_some(), "{v:?} at {w}x{h}");
            assert!(geom(Rect::new(0, 0, w - 1, h), v).is_none());
            let g = geom(Rect::new(0, 0, 100, 30), v).unwrap();
            assert!(g.help_y < 30);
        }
    }

    #[test]
    fn drawing_never_panics_at_any_size() {
        use crate::app::App;
        use crate::audio::Audio;
        use crate::store::Store;
        for v in 0..VARIANTS.len() {
            let mut a = App::new(Audio::silent(), Store::default(), 1);
            a.variant = v;
            for (w, h) in [(1, 1), (20, 5), (40, 12), (80, 24), (100, 30), (200, 60)] {
                let area = Rect::new(0, 0, w, h);
                let mut buf = Buffer::empty(area);
                a.to_title();
                draw(&a, area, &mut buf);
                a.start(Mode::Duo);
                a.play.as_mut().unwrap().cursor = VARIANTS[v].w - 1;
                a.drop_disc();
                for _ in 0..3 {
                    a.tick(0.05);
                    draw(&a, area, &mut buf);
                }
            }
        }
    }
}
