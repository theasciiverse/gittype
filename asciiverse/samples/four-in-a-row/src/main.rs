mod app;
mod audio;
mod board;
mod engine;
mod render;
mod rng;
mod store;

use app::{App, Mode, Screen};
use board::{Variant, VARIANTS};
use engine::Level;
use ratatui::crossterm::event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use ratatui::crossterm::execute;
use ratatui::layout::Rect;
use std::io;
use std::time::{Duration, Instant, SystemTime};

const FRAME: Duration = Duration::from_millis(16);
const SNAPSHOT_EVERY: Duration = Duration::from_secs(1);

const HELP: &str = "four-in-a-row [--level L] [--board B] [--two] [--watch] [--demo] [--mute] [--seed N]

Drop discs into the columns; the first to line up four in a row, across,
up or on a slant, wins. Play the computer at four levels, a friend at the
same keyboard, or watch the computer play itself.

Keys: ← → (or A D, H L) choose a column, Space or Enter or ↓ drops,
1 to 9 and 0 drop straight into that column. The mouse works too: point
at a column and click. U takes a move back, M turns the sound off and on,
Esc goes back to the menu, Q quits from the menu.
Gamepad: d-pad left and right, A drops, X takes back, Start for the menu.

  --level L   the computer: easy, normal, hard or master
  --board B   classic (7x6), wide (8x7), grand (9x7) or five (10x8, five in a row)
  --two       start a two-player game right away
  --watch     start with the computer playing itself
  --demo      the computer plays itself, game after game
  --mute      no sound
  --seed N    fixed random seed

Logging: RUST_LOG=debug four-in-a-row 2>four-in-a-row.log
";

fn main() -> io::Result<()> {
    env_logger::init();
    let args: Vec<String> = std::env::args().skip(1).collect();
    let has = |f: &str| args.iter().any(|a| a == f);
    let after = |f: &str| args.iter().position(|a| a == f).and_then(|i| args.get(i + 1)).cloned();
    if has("--help") || has("-h") {
        print!("{HELP}");
        return Ok(());
    }
    if has("--version") || has("-V") {
        println!("four-in-a-row {}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }
    let seed = after("--seed")
        .and_then(|v| v.parse().ok())
        .unwrap_or_else(|| SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).map_or(1, |d| d.as_nanos() as u64));

    // open audio before the alternate screen, so any ALSA chatter lands above it
    let store = store::Store::load();
    let audio = audio::Audio::new(has("--mute") || store.muted);
    log::info!("start version {} audio {} muted {} seed {seed}", env!("CARGO_PKG_VERSION"), audio.available(), audio.muted);
    let mut app = App::new(audio, store, seed);
    if let Some(l) = after("--level") {
        app.level = Level::parse(&l).unwrap_or_else(|| {
            eprintln!("--level takes easy, normal, hard or master");
            std::process::exit(2);
        });
    }
    if let Some(b) = after("--board") {
        app.variant = Variant::by_name(&b).unwrap_or_else(|| {
            eprintln!("--board takes {}", VARIANTS.iter().map(|v| v.name.to_ascii_lowercase()).collect::<Vec<_>>().join(", "));
            std::process::exit(2);
        });
    }
    if has("--demo") {
        app.demo = true;
        app.start(Mode::Watch(app.level));
    } else if has("--watch") {
        app.start(Mode::Watch(app.level));
    } else if has("--two") {
        app.start(Mode::Duo);
    }

    let mut terminal = ratatui::init();
    execute!(io::stdout(), EnableMouseCapture)?;
    let size = terminal.size()?;
    log::info!("terminal {}x{}", size.width, size.height);
    let mut pads = Pads::new();
    let result = run(&mut terminal, &mut app, &mut pads);
    let _ = execute!(io::stdout(), DisableMouseCapture);
    ratatui::restore();
    result
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Cmd {
    Up,
    Down,
    Left,
    Right,
    Confirm,
    Column(usize),
    Drop,
    Undo,
    Mute,
    Back,
    Quit,
}

fn run(terminal: &mut ratatui::DefaultTerminal, app: &mut App, pads: &mut Pads) -> io::Result<()> {
    let mut last = Instant::now();
    let mut snapshot = Instant::now();
    let mut area = {
        let s = terminal.size()?;
        Rect::new(0, 0, s.width, s.height)
    };
    loop {
        terminal.draw(|f| {
            area = f.area();
            render::draw(app, f.area(), f.buffer_mut())
        })?;
        let deadline = Instant::now() + FRAME;
        while let Some(wait) = deadline.checked_duration_since(Instant::now()) {
            if !event::poll(wait)? {
                break;
            }
            match event::read()? {
                Event::Key(k) => {
                    log::debug!("key {:?} {:?} {:?}", k.code, k.kind, k.modifiers);
                    if let Some(cmd) = key_cmd(app, k) {
                        apply(app, cmd);
                    }
                }
                Event::Mouse(m) => mouse(app, area, m),
                Event::Resize(w, h) => log::info!("resize {w}x{h}"),
                _ => {}
            }
            if app.quit {
                return Ok(());
            }
        }
        for cmd in pads.poll(app) {
            apply(app, cmd);
        }
        if app.quit {
            return Ok(());
        }
        let now = Instant::now();
        let dt = now.duration_since(last).as_secs_f32().min(0.1);
        last = now;
        app.tick(dt);
        if snapshot.elapsed() >= SNAPSHOT_EVERY {
            snapshot = Instant::now();
            log::debug!("snapshot {}", app.snapshot());
        }
    }
}

fn key_cmd(app: &App, k: KeyEvent) -> Option<Cmd> {
    if k.kind == KeyEventKind::Release {
        return None;
    }
    if k.modifiers.contains(KeyModifiers::CONTROL) && matches!(k.code, KeyCode::Char('c')) {
        return Some(Cmd::Quit);
    }
    let ch = match k.code {
        KeyCode::Char(c) => Some(c.to_ascii_lowercase()),
        _ => None,
    };
    if ch == Some('m') {
        return Some(Cmd::Mute);
    }
    if app.screen == Screen::Title {
        return match (k.code, ch) {
            (KeyCode::Up, _) | (_, Some('w' | 'k')) => Some(Cmd::Up),
            (KeyCode::Down, _) | (_, Some('s' | 'j')) => Some(Cmd::Down),
            (KeyCode::Left, _) | (_, Some('a' | 'h')) => Some(Cmd::Left),
            (KeyCode::Right, _) | (_, Some('d' | 'l')) => Some(Cmd::Right),
            (KeyCode::Enter, _) | (_, Some(' ')) => Some(Cmd::Confirm),
            (KeyCode::Esc, _) | (_, Some('q')) => Some(Cmd::Quit),
            _ => None,
        };
    }
    match (k.code, ch) {
        (KeyCode::Esc, _) => Some(Cmd::Back),
        (_, Some('q')) if app.play.as_ref().is_some_and(|m| m.over.is_some() || matches!(m.mode, Mode::Watch(_))) => Some(Cmd::Back),
        (KeyCode::Left, _) | (_, Some('a' | 'h')) => Some(Cmd::Left),
        (KeyCode::Right, _) | (_, Some('d' | 'l')) => Some(Cmd::Right),
        (KeyCode::Enter | KeyCode::Down, _) | (_, Some(' ' | 's' | 'j')) => Some(Cmd::Drop),
        (KeyCode::Backspace, _) | (_, Some('u')) => Some(Cmd::Undo),
        (_, Some(d @ '0'..='9')) => {
            let n = d.to_digit(10).unwrap() as usize;
            Some(Cmd::Column(if n == 0 { 9 } else { n - 1 }))
        }
        _ => None,
    }
}

fn apply(app: &mut App, cmd: Cmd) {
    log::debug!("cmd {cmd:?}");
    match cmd {
        Cmd::Quit => app.quit = true,
        Cmd::Mute => {
            app.audio.muted = !app.audio.muted;
            app.store.muted = app.audio.muted;
            app.store.save();
            log::info!("muted {}", app.audio.muted);
        }
        Cmd::Up => app.menu_move(-1),
        Cmd::Down => app.menu_move(1),
        Cmd::Left if app.screen == Screen::Title => app.menu_side(-1),
        Cmd::Right if app.screen == Screen::Title => app.menu_side(1),
        Cmd::Left => app.cursor_move(-1),
        Cmd::Right => app.cursor_move(1),
        Cmd::Confirm => app.menu_pick(),
        Cmd::Column(c) => {
            if app.play.as_ref().is_some_and(|m| m.human_turn() && c < m.board.v.w) {
                app.cursor_to(c);
                app.drop_disc();
            }
        }
        Cmd::Drop => app.drop_disc(),
        Cmd::Undo => app.undo(),
        Cmd::Back => app.to_title(),
    }
}

fn mouse(app: &mut App, area: Rect, m: MouseEvent) {
    match m.kind {
        MouseEventKind::Down(MouseButton::Left) => log::debug!("mouse click {},{}", m.column, m.row),
        MouseEventKind::Moved | MouseEventKind::Drag(_) => log::trace!("mouse at {},{}", m.column, m.row),
        _ => log::debug!("mouse {:?} {},{}", m.kind, m.column, m.row),
    }
    if app.screen == Screen::Title {
        let rows = render::menu_rows(area);
        let Some(i) = rows.iter().position(|r| r.y == m.row) else { return };
        match m.kind {
            MouseEventKind::Moved if i != app.menu => app.menu_move(i as i32 - app.menu as i32),
            MouseEventKind::Down(MouseButton::Left) => {
                app.menu = i;
                app.menu_pick();
            }
            MouseEventKind::Down(MouseButton::Right) => {
                app.menu = i;
                app.menu_side(-1);
            }
            _ => {}
        }
        return;
    }
    let Some(p) = app.play.as_ref() else { return };
    let v = p.board.v;
    let Some(g) = render::geom(area, v) else { return };
    match m.kind {
        MouseEventKind::Moved | MouseEventKind::Drag(_) => {
            if let Some(c) = render::col_at(&g, v, m.column) {
                app.cursor_to(c);
            }
        }
        MouseEventKind::Down(MouseButton::Left) => {
            if p.over.is_some() {
                app.drop_disc();
            } else if let Some(c) = render::col_at(&g, v, m.column) {
                app.cursor_to(c);
                app.drop_disc();
            }
        }
        MouseEventKind::Down(MouseButton::Right) => app.undo(),
        MouseEventKind::ScrollUp => app.cursor_move(-1),
        MouseEventKind::ScrollDown => app.cursor_move(1),
        _ => {}
    }
}

/// Gamepads: any pad plays whoever's turn it is.
struct Pads {
    #[cfg(feature = "gamepad")]
    gilrs: Option<gilrs::Gilrs>,
}

impl Pads {
    fn new() -> Pads {
        #[cfg(feature = "gamepad")]
        {
            let gilrs = match gilrs::Gilrs::new() {
                Ok(g) => Some(g),
                Err(e) => {
                    log::warn!("no gamepads: {e}");
                    None
                }
            };
            Pads { gilrs }
        }
        #[cfg(not(feature = "gamepad"))]
        Pads {}
    }

    #[cfg(not(feature = "gamepad"))]
    fn poll(&mut self, _app: &App) -> Vec<Cmd> {
        Vec::new()
    }

    #[cfg(feature = "gamepad")]
    fn poll(&mut self, app: &App) -> Vec<Cmd> {
        use gilrs::{Button, EventType};
        let mut out = Vec::new();
        let Some(g) = &mut self.gilrs else { return out };
        while let Some(ev) = g.next_event() {
            match ev.event {
                EventType::Connected => log::info!("gamepad connected: {}", g.gamepad(ev.id).name()),
                EventType::Disconnected => log::info!("gamepad disconnected"),
                EventType::ButtonPressed(b, _) => {
                    log::debug!("pad {:?} button {b:?}", ev.id);
                    // gilrs also reports 3D mice and tablet key pads as
                    // gamepads; only trust the ones SDL knows
                    let any = std::env::var("FOUR_IN_A_ROW_ANY_PAD").is_ok_and(|v| v == "1");
                    if !any && g.gamepad(ev.id).mapping_source() != gilrs::MappingSource::SdlMappings {
                        continue;
                    }
                    let title = app.screen == Screen::Title;
                    let cmd = match b {
                        Button::DPadUp if title => Some(Cmd::Up),
                        Button::DPadDown if title => Some(Cmd::Down),
                        Button::DPadLeft => Some(Cmd::Left),
                        Button::DPadRight => Some(Cmd::Right),
                        Button::South | Button::Start if title => Some(Cmd::Confirm),
                        Button::East if title => Some(Cmd::Quit),
                        Button::South | Button::DPadDown => Some(Cmd::Drop),
                        Button::West | Button::East => Some(Cmd::Undo),
                        Button::Start | Button::Select => Some(Cmd::Back),
                        _ => None,
                    };
                    out.extend(cmd);
                }
                _ => {}
            }
        }
        out
    }
}
