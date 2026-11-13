//! av, the Asciiverse terminal client.

mod app;
mod cast;
mod catalog;
mod clipboard;
mod cli;
mod complete;
mod downloads;
mod glyphs;
mod install;
mod launch;
mod libs;
mod md;
mod paths;
mod selfupdate;
mod state;
#[cfg(test)]
mod testutil;
mod theme;
mod ui;

use std::time::Duration;

use anyhow::Result;
use ratatui::crossterm::event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyEventKind};
use ratatui::crossterm::execute;

use app::App;

fn main() -> Result<()> {
    let paths = paths::Paths::new()?;
    let args: Vec<String> = std::env::args().skip(1).collect();
    // Tab completion runs av on every Tab, and a shell's startup file on
    // every new shell: those, help and the version stay out of the log
    let quiet = args.first().is_some_and(|a| ["__complete", "completions", "help", "-h", "--help", "-V", "--version", "version"].contains(&a.as_str()));
    if !quiet {
        init_logging(&paths, args.is_empty());
    }
    let url = catalog::url();
    log::info!("av {} starting, catalog {url}", env!("CARGO_PKG_VERSION"));
    if !args.is_empty() {
        if let Err(e) = cli::run(paths, url, args) {
            eprintln!("av: {e:#}");
            std::process::exit(1);
        }
        return Ok(());
    }

    // the first fc-list and ldconfig can take a moment; not on the UI thread
    std::thread::spawn(|| {
        glyphs::missing("braille");
        libs::missing(&[]);
    });
    let mut app = App::new(paths, url);
    let mut terminal = ratatui::init();
    let _ = execute!(std::io::stdout(), EnableMouseCapture);
    let res = run(&mut terminal, &mut app);
    let _ = execute!(std::io::stdout(), DisableMouseCapture);
    ratatui::restore();
    res
}

/// Session logging per the Asciiverse convention, to av.log in the cache
/// dir. Always on at info (actions, downloads, launches, failures), so a
/// problem report can come with a log; RUST_LOG=av=debug adds every key and
/// click, and a RUST_LOG set for something else adds to av=info rather than
/// replacing it. Past 1 MB the log moves to av.log.1, one kept; only the
/// TUI does that, so a command run beside it never moves the log it writes.
fn init_logging(paths: &paths::Paths, tui: bool) {
    let file = paths.log();
    if tui && std::fs::metadata(&file).is_ok_and(|m| m.len() > LOG_MAX) {
        let _ = std::fs::rename(&file, file.with_extension("log.1"));
    }
    let filter = match std::env::var("RUST_LOG") {
        Ok(more) if !more.trim().is_empty() => format!("av=info,{more}"),
        _ => "av=info".into(),
    };
    if let Ok(f) = std::fs::OpenOptions::new().create(true).append(true).open(&file) {
        env_logger::Builder::new().parse_filters(&filter).target(env_logger::Target::Pipe(Box::new(f))).init();
    }
}

const LOG_MAX: u64 = 1 << 20;

fn run(terminal: &mut ratatui::DefaultTerminal, app: &mut App) -> Result<()> {
    let mut last_snapshot = std::time::Instant::now();
    while !app.quit {
        app.tick();
        terminal.draw(|f| ui::draw(f, app))?;
        // wait for input up to one frame, then take everything queued, so a
        // fast wheel spin is handled at once instead of one notch per frame
        let mut wait = Duration::from_millis(33);
        while event::poll(wait)? {
            match event::read()? {
                Event::Key(k) if k.kind == KeyEventKind::Press => app.on_key(k),
                Event::Mouse(m) => app.on_mouse(m),
                _ => {}
            }
            if app.launch.is_some() || app.quit {
                break;
            }
            wait = Duration::ZERO;
        }
        if let Some(t) = app.launch.take() {
            // the title gets a plain terminal; it turns on its own mouse mode if it wants one
            let _ = execute!(std::io::stdout(), DisableMouseCapture);
            ratatui::restore();
            launch(app, &t);
            // back the way ratatui::init came in, without another panic hook
            ratatui::crossterm::terminal::enable_raw_mode()?;
            let _ = execute!(std::io::stdout(), ratatui::crossterm::terminal::EnterAlternateScreen, EnableMouseCapture);
            // a clean screen and a full redraw; resize, not clear, which asks the
            // terminal where its cursor is and gives up on av if it doesn't say
            let size = terminal.size()?;
            terminal.resize(ratatui::layout::Rect::new(0, 0, size.width, size.height))?;
        }
        if last_snapshot.elapsed() > Duration::from_millis(500) {
            last_snapshot = std::time::Instant::now();
            log::debug!(
                "snapshot show={:?} filter={:?} selected={} focus={:?} downloading={:?} queued={}",
                app.show, app.filter, app.selected, app.focus, app.downloads.running().map(|d| &d.id), app.downloads.queued()
            );
        }
    }
    Ok(())
}

/// Run a title as a separate process in the same terminal; av never links it.
fn launch(app: &mut App, t: &catalog::Title) {
    match launch::command(&app.paths, t, app.state.installed.get(&t.id)) {
        Ok((cmd, log)) => {
            let outcome = launch::run(cmd, log);
            app.finished(t, outcome);
        }
        Err(e) => app.status = e,
    }
}
