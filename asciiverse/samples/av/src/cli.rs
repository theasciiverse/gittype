//! `av <command>`: the same catalog and actions as the TUI, from the shell.
//! `av` alone opens the TUI; anything after it is a command:
//!
//! ```text
//! av install <title>...     install, or update to the catalog's version
//! av update [<title>...]    update these titles, or every one with an update
//! av uninstall <title>...   remove titles; their saves and settings stay
//! av play <title> [args]    start a title, or connect to a MUD
//! av list [installed|updates|wishlist]
//! av search <words>
//! av info <title>
//! ```
//!
//! Downloads draw a progress bar on the terminal; when the output is not a
//! terminal, each step is one plain line instead. Colors follow the TUI's
//! theme and are left out when the output is not a terminal or `NO_COLOR`
//! is set.

use std::io::{IsTerminal, Write};
use std::sync::mpsc::channel;
use std::time::{Duration, Instant};

use anyhow::{Result, bail};
use ratatui::crossterm::style::{Color as CColor, Stylize};
use ratatui::crossterm::{cursor, queue, style, terminal};
use ratatui::style::Color;

use crate::catalog::{self, Title};
use crate::install::{self, Progress};
use crate::paths::{Paths, tilde};
use crate::state::State;
use crate::theme;

pub const USAGE: &str = "\
av, the Asciiverse client

Usage:
  av                        open the catalog and library
  av install <title>...     install titles, or update them to the latest version
  av update [<title>...]    update these titles, or every title with an update
  av uninstall <title>...   remove titles; their saves and settings are kept
  av play <title> [args]    start a title (for a MUD: connect to it)
  av list [installed|updates|wishlist]
                            list the catalog, or part of it
  av search <words>         find titles by name, topic, genre or developer
  av info <title>           show a title's details
  av completions <shell>    print tab completion for bash, zsh, fish or powershell
  av help                   show this
  av --version              show av's version

A title is its id (as in `av install solitty`) or its full name.";

struct Ctx {
    paths: Paths,
    state: State,
    titles: Vec<Title>,
    collections: catalog::Collections,
    offline: bool,
    url: String,
    color: bool,
    /// colours on stderr
    ecolor: bool,
    tty: bool,
    /// the catalog's av when it is newer than this one
    av_newer: Option<catalog::AvRelease>,
}

pub fn run(paths: Paths, url: String, args: Vec<String>) -> Result<()> {
    let tty = std::io::stdout().is_terminal();
    #[cfg(windows)]
    let ansi = ratatui::crossterm::ansi_support::supports_ansi();
    #[cfg(not(windows))]
    let ansi = true;
    let no_color = std::env::var_os("NO_COLOR").is_some_and(|v| !v.is_empty());
    let color = tty && ansi && !no_color;
    // stderr is its own question: `av install x 2>err.log` gets plain text there
    let ecolor = std::io::stderr().is_terminal() && ansi && !no_color;
    let (cmd, rest) = (args[0].as_str(), &args[1..]);
    // its arguments (a title's own, a password among them) stay out of the
    // log users attach to reports, unless asked for with RUST_LOG=av=debug
    log::info!("command {cmd}");
    log::debug!("arguments {rest:?}");
    match cmd {
        "help" | "-h" | "--help" => {
            println!("{USAGE}");
            return Ok(());
        }
        "-V" | "--version" | "version" => {
            println!("av {}", env!("CARGO_PKG_VERSION"));
            return Ok(());
        }
        "completions" => {
            let Some(shell) = rest.first() else {
                bail!("usage: av completions <{}>", crate::complete::SHELLS.join("|"));
            };
            print!("{}", crate::complete::script(shell)?);
            return Ok(());
        }
        // what a completion script asks on every Tab; not in the help
        "__complete" => {
            for c in crate::complete::candidates(rest, &crate::complete::Known::load(&paths)) {
                println!("{c}");
            }
            return Ok(());
        }
        _ => {}
    }
    let state = State::load(&paths.state);
    for f in &state.set_aside {
        eprintln!("av: could not read it, so set it aside: {f}");
    }
    let mut cx = Ctx { state, paths, titles: Vec::new(), collections: Default::default(), offline: false, url, color, ecolor, tty, av_newer: None };
    let known = ["install", "update", "upgrade", "uninstall", "remove", "play", "run", "list", "ls", "search", "info", "show"];
    if !known.contains(&cmd) {
        bail!("unknown command {cmd:?}\n\n{USAGE}");
    }
    cx.load()?;
    // first, on stderr: every way a command can end still shows it, and a
    // script reading the output is not disturbed
    if let Some(r) = &cx.av_newer {
        eprintln!("{}", cx.epaint(crate::selfupdate::notice(r), theme::AMBER));
    }
    match cmd {
        "install" => need(rest, "install <title>...").and_then(|_| cx.install(rest)),
        "update" | "upgrade" => cx.update(rest),
        "uninstall" | "remove" => need(rest, "uninstall <title>...").and_then(|_| cx.uninstall(rest)),
        "play" | "run" => need(rest, "play <title>").and_then(|_| cx.play(&rest[0], &rest[1..])),
        "list" | "ls" => cx.list(rest.first().map(String::as_str)),
        "search" => need(rest, "search <words>").and_then(|_| cx.search(&rest.join(" "))),
        "info" | "show" => need(rest, "info <title>").and_then(|_| cx.info(&rest[0])),
        _ => unreachable!(),
    }
}

fn need(rest: &[String], usage: &str) -> Result<()> {
    if rest.is_empty() {
        bail!("usage: av {usage}");
    }
    Ok(())
}

fn rgb(c: Color) -> CColor {
    match c {
        Color::Rgb(r, g, b) => CColor::Rgb { r, g, b },
        _ => CColor::Reset,
    }
}

/// 1000-based, the way apt and most download tools count.
pub fn human(n: u64) -> String {
    match n {
        n if n < 1000 => format!("{n} B"),
        n if n < 1_000_000 => format!("{:.0} kB", n as f64 / 1e3),
        n if n < 1_000_000_000 => format!("{:.1} MB", n as f64 / 1e6),
        n => format!("{:.2} GB", n as f64 / 1e9),
    }
}

impl Ctx {
    fn paint(&self, s: impl std::fmt::Display, c: Color) -> String {
        if self.color { s.to_string().with(rgb(c)).to_string() } else { s.to_string() }
    }

    /// The same for stderr, coloured by whether stderr is a terminal.
    fn epaint(&self, s: impl std::fmt::Display, c: Color) -> String {
        if self.ecolor { s.to_string().with(rgb(c)).to_string() } else { s.to_string() }
    }

    fn load(&mut self) -> Result<()> {
        let l = catalog::load(&self.url, &self.paths.cache, false)?;
        self.av_newer = crate::selfupdate::newer(l.catalog.av.as_ref()).cloned();
        if let Some(msg) = &l.outdated {
            eprintln!("{}", self.epaint(format!("{msg}\nUntil then, av uses the last catalog it could read."), theme::AMBER));
            // that says how to update already
            self.av_newer = None;
        }
        let gone = catalog::gone(&l.catalog.titles, &self.state.installed);
        let mut titles: Vec<Title> = l.catalog.titles.into_values().chain(gone).collect();
        catalog::sort(&mut titles);
        catalog::index(&mut titles, &l.catalog.collections);
        self.titles = titles;
        self.collections = l.catalog.collections;
        self.offline = l.offline;
        if l.offline {
            eprintln!("{}", self.epaint("offline: using the cached catalog", theme::AMBER));
        }
        Ok(())
    }

    /// A title by id, or by its full name in any case.
    fn find(&self, name: &str) -> Result<Title> {
        let low = name.to_lowercase();
        if let Some(t) = self.titles.iter().find(|t| t.id == low || t.title.to_lowercase() == low) {
            return Ok(t.clone());
        }
        let near: Vec<String> = self.titles.iter().filter(|t| t.matches(&low)).take(5).map(|t| t.id.clone()).collect();
        if near.is_empty() {
            bail!("no title {name:?} in the catalog; try `av search {name}`");
        }
        bail!("no title {name:?} in the catalog; did you mean: {}", near.join(", "));
    }

    fn update_waiting(&self, t: &Title) -> bool {
        self.state.installed.get(&t.id).is_some_and(|i| !t.version.is_empty() && i.version != t.version)
    }

    fn install(&mut self, names: &[String]) -> Result<()> {
        let mut failed = Vec::new();
        for name in names {
            let res = self.find(name).and_then(|t| self.install_one(&t, false));
            if let Err(e) = res {
                eprintln!("{} {e:#}", self.epaint("error:", theme::UPDATE));
                failed.push(name.clone());
            }
        }
        if names.len() > 1 && !failed.is_empty() {
            bail!("not installed: {}", failed.join(", "));
        } else if !failed.is_empty() {
            std::process::exit(1);
        }
        self.path_note();
        Ok(())
    }

    fn update(&mut self, names: &[String]) -> Result<()> {
        let titles: Vec<Title> = if names.is_empty() {
            self.titles.iter().filter(|t| self.update_waiting(t)).cloned().collect()
        } else {
            names.iter().map(|n| self.find(n)).collect::<Result<_>>()?
        };
        if titles.is_empty() {
            println!("Everything is up to date.");
            return Ok(());
        }
        let mut failed = Vec::new();
        for t in &titles {
            if let Err(e) = self.install_one(t, true) {
                eprintln!("{} {e:#}", self.epaint("error:", theme::UPDATE));
                failed.push(t.id.clone());
            }
        }
        if titles.len() > 1 && !failed.is_empty() {
            bail!("not updated: {}", failed.join(", "));
        } else if !failed.is_empty() {
            std::process::exit(1);
        }
        Ok(())
    }

    /// Install or update one title, drawing the download as it goes.
    fn install_one(&mut self, t: &Title, updating: bool) -> Result<()> {
        if t.is_mud() {
            bail!("{} is a MUD, a world on someone's server: there is nothing to install. Connect with `av play {}`", t.title, t.id);
        }
        let have = self.state.installed.get(&t.id).map(|i| i.version.clone());
        match &have {
            Some(v) if !self.update_waiting(t) => {
                println!("{} {} is already installed{}", t.title, v, if updating { " and up to date" } else { "" });
                return Ok(());
            }
            None if updating => bail!("{} is not installed; install it with `av install {}`", t.title, t.id),
            _ => {}
        }
        if self.offline {
            bail!("offline: cannot download {}", t.title);
        }
        let Some((target, _)) = catalog::pick_platform(t) else {
            bail!("{} has no build for this system ({} {})", t.title, std::env::consts::OS, std::env::consts::ARCH);
        };
        let what = match &have {
            Some(v) => format!("updating {} {v} to {}", t.id, t.version),
            None => format!("fetching {} {}", t.id, t.version),
        };
        println!("{} {}", self.paint(what, theme::TEXT_HI), self.paint(format!("({target})"), theme::FAINT));

        let dest = self.paths.title_dir(&t.kind, &t.id);
        let (tx, rx) = channel();
        install::spawn(
            install::Job {
                title: t.clone(),
                catalog_url: self.url.clone(),
                cache: self.paths.cache.clone(),
                dest: dest.clone(),
                saves: self.paths.saves(&t.id),
                bin: self.paths.bin.clone(),
                previous: Some(self.paths.installed_dir(t, self.state.installed.get(&t.id))),
                cancel: None,
                job: 0,
            },
            tx,
        );
        let mut bar = Bar::new(self.tty, self.color);
        for p in rx {
            match p {
                Progress::Bytes { done, total, .. } => bar.draw(done, total),
                Progress::Status { msg, .. } => {
                    if msg != "downloading" {
                        bar.finish();
                        println!("  {}", self.paint(msg, theme::DIM));
                    }
                }
                Progress::Done { installed, .. } => {
                    bar.finish();
                    println!("  {} {}", self.paint("installed to", theme::DIM), tilde(&dest));
                    self.state.installed.insert(t.id.clone(), installed);
                    self.state.save()?;
                    return Ok(());
                }
                Progress::Failed { error, .. } => {
                    bar.finish();
                    bail!("{}: {error}", t.title);
                }
            }
        }
        bail!("{}: the install stopped without a result", t.title)
    }

    /// Once per run, after installing: how to start titles from the shell.
    fn path_note(&self) {
        if let Some(b) = &self.paths.bin
            && !self.paths.bin_on_path()
        {
            println!(
                "\n{} is not on your PATH, so start titles with `av play <title>`, or add this to your shell's startup file:\n  export PATH=\"$HOME/.local/bin:$PATH\"",
                tilde(b)
            );
        }
    }

    fn uninstall(&mut self, names: &[String]) -> Result<()> {
        let mut failed = Vec::new();
        for name in names {
            // what installed.json knows is enough, even for a title that has left the catalog
            let low = name.to_lowercase();
            let id = self.find(name).map(|t| t.id).unwrap_or(low);
            let Some(i) = self.state.installed.get(&id).cloned() else {
                eprintln!("{} {name} is not installed", self.epaint("error:", theme::UPDATE));
                failed.push(name.clone());
                continue;
            };
            let kind = if i.kind.is_empty() { "game".to_string() } else { i.kind.clone() };
            let dest = self.paths.title_dir(&kind, &id);
            match install::uninstall(&dest, self.paths.bin.as_deref(), &id, &self.paths.saves(&id), true) {
                Ok(()) => {
                    self.state.installed.remove(&id);
                    self.state.save()?;
                    println!("uninstalled {id} {}; its saves and settings were kept", i.version);
                }
                Err(e) => {
                    eprintln!("{} {id}: {e:#}", self.epaint("error:", theme::UPDATE));
                    failed.push(name.clone());
                }
            }
        }
        if names.len() > 1 && !failed.is_empty() {
            bail!("not uninstalled: {}", failed.join(", "));
        } else if !failed.is_empty() {
            std::process::exit(1);
        }
        Ok(())
    }

    fn play(&mut self, name: &str, extra: &[String]) -> Result<()> {
        let t = self.find(name)?;
        if !t.is_mud() && !self.state.installed.contains_key(&t.id) {
            bail!("{} is not installed; install it with `av install {}`", t.title, t.id);
        }
        if let (Some(m), Ok((w, h))) = (t.min_terminal, terminal::size())
            && (w < m.cols || h < m.rows)
        {
            eprintln!(
                "{}",
                self.epaint(format!("{} needs at least {}x{}; this terminal is {w}x{h}", t.title, m.cols, m.rows), theme::AMBER)
            );
        }
        if !t.is_mud() && self.update_waiting(&t) {
            let have = &self.state.installed[&t.id].version;
            eprintln!(
                "{}",
                self.epaint(format!("{} {} is out; you have {have}. Update with `av update {}`", t.title, t.version, t.id), theme::AMBER)
            );
        }
        let (mut cmd, log) = crate::launch::command(&self.paths, &t, self.state.installed.get(&t.id)).map_err(anyhow::Error::msg)?;
        cmd.args(extra);
        let o = crate::launch::run(cmd, log);
        self.state.record_play(&t.id, o.seconds);
        self.state.save()?;
        match &o.status {
            Ok(st) => {
                if let Some(how) = crate::launch::failure(st) {
                    let mut text = format!("{} {how} after {}.", t.title, crate::launch::duration(o.seconds));
                    if let Some(log) = &o.log {
                        let tail = crate::launch::tail(log, 12);
                        if !tail.is_empty() {
                            text += &format!("\n\nLast lines it printed:\n\n{tail}");
                        }
                        text += &format!("\n\nFull log: {}", tilde(log));
                    }
                    bail!(text);
                }
                Ok(())
            }
            Err(e) if t.connect.is_some() && e.kind() == std::io::ErrorKind::NotFound => {
                bail!("telnet is not installed; install a telnet client to connect to {}", t.title)
            }
            Err(e) => bail!("could not start {}: {e}", t.title),
        }
    }

    fn list(&self, which: Option<&str>) -> Result<()> {
        let titles: Vec<&Title> = match which {
            None | Some("all") => self.titles.iter().collect(),
            Some("installed") => self.titles.iter().filter(|t| self.state.installed.contains_key(&t.id)).collect(),
            Some("updates") => self.titles.iter().filter(|t| self.update_waiting(t)).collect(),
            Some("wishlist") => self.titles.iter().filter(|t| self.state.wishlist.contains(&t.id)).collect(),
            Some(w) => bail!("usage: av list [installed|updates|wishlist], not {w:?}"),
        };
        if titles.is_empty() {
            println!(
                "{}",
                match which {
                    Some("installed") => "Nothing installed yet. Find something with `av list` or `av search`.",
                    Some("updates") => "Everything is up to date.",
                    Some("wishlist") => "The wishlist is empty.",
                    _ => "The catalog is empty.",
                }
            );
            return Ok(());
        }
        self.table(&titles);
        Ok(())
    }

    fn search(&self, words: &str) -> Result<()> {
        let f = words.to_lowercase();
        let mut v: Vec<&Title> = self.titles.iter().filter(|t| t.matches(&f)).collect();
        // titles whose name matches first, the rest after, catalog order kept inside each
        v.sort_by_key(|t| !t.title.to_lowercase().contains(&f));
        if v.is_empty() {
            println!("Nothing matches {words:?}.");
            return Ok(());
        }
        self.table(&v);
        Ok(())
    }

    /// One row per title, grouped by kind like the TUI's list: a mark, the id
    /// to type, the name, the version and the one-line description.
    fn table(&self, titles: &[&Title]) {
        let idw = titles.iter().map(|t| t.id.chars().count()).max().unwrap_or(0);
        let namew = titles.iter().map(|t| t.title.chars().count()).max().unwrap_or(0).min(28);
        let cols = if self.tty { terminal::size().map(|s| s.0 as usize).unwrap_or(100) } else { usize::MAX };
        let ver_of = |t: &Title| match self.state.installed.get(&t.id) {
            Some(i) if self.update_waiting(t) => format!("{} → {}", i.version, t.version),
            Some(i) => i.version.clone(),
            None => t.version.clone(),
        };
        let verw = titles.iter().map(|t| ver_of(t).chars().count()).max().unwrap_or(0);
        let mut last_kind: Option<&str> = None;
        for t in titles {
            if last_kind != Some(t.kind.as_str()) {
                let label = match t.kind.as_str() {
                    "game" => "Games",
                    "adventure" => "Text adventures",
                    "program" => "Programs",
                    "art" => "Art & Demos",
                    "movie" => "Movies",
                    "mud" => "MUDs",
                    k => k,
                };
                if last_kind.is_some() {
                    println!();
                }
                println!("{}", self.paint(label, theme::AMBER_SOFT));
                last_kind = Some(&t.kind);
            }
            let (mark, c) = match self.state.installed.get(&t.id) {
                Some(_) if self.update_waiting(t) => ("▲", theme::UPDATE),
                Some(_) => ("●", theme::INSTALLED),
                None => (" ", theme::NOT_INSTALLED),
            };
            let name: String = t.title.chars().take(namew).collect();
            let head = format!("{mark} {:<idw$}  {:<namew$}  {:<verw$}", t.id, name, ver_of(t));
            let room = cols.saturating_sub(head.chars().count() + 2);
            let desc: String = if room > 12 {
                let d: String = t.description.chars().take(room).collect();
                if t.description.chars().count() > room { format!("{}…", d.chars().take(room - 1).collect::<String>()) } else { d }
            } else {
                String::new()
            };
            println!("{}  {}", self.paint(head, c), self.paint(desc, theme::FAINT));
        }
    }

    fn info(&self, name: &str) -> Result<()> {
        let t = self.find(name)?;
        let label = |s: &str| self.paint(format!("{s:<13}"), theme::DIM);
        let year = t.since.map(|y| format!(" ({y})")).unwrap_or_default();
        println!("{}{}  {}", self.paint(&t.title, theme::TEXT_HI), year, self.paint(&t.id, theme::FAINT));
        println!("{}\n", t.description);
        if !t.version.is_empty() {
            println!("{}{}", label("version"), t.version);
        }
        match self.state.installed.get(&t.id) {
            Some(i) if self.update_waiting(&t) => {
                println!("{}{}", label("installed"), self.paint(format!("{}, update to {} waiting", i.version, t.version), theme::UPDATE))
            }
            Some(i) => println!("{}{}", label("installed"), self.paint(&i.version, theme::INSTALLED)),
            None if t.is_mud() => {}
            None => println!("{}no", label("installed")),
        }
        println!("{}{}", label("kind"), t.kind);
        if let Some(g) = &t.genre {
            println!("{}{g}", label("genre"));
        }
        if let Some(d) = &t.developer {
            println!("{}{}", label("developer"), d.name);
        }
        if let Some(u) = &t.upstream {
            println!("{}{}", label("original"), u.name);
        }
        if let Some(name) = t.collection_name(&self.collections) {
            let others: Vec<&str> = self.titles.iter().filter(|o| o.collection == t.collection && o.id != t.id).map(|o| o.id.as_str()).collect();
            if others.is_empty() {
                println!("{}{name}", label("part of"));
            } else {
                println!("{}{name}; also {}", label("part of"), others.join(", "));
            }
        }
        if !t.license.is_empty() {
            println!("{}{}", label("license"), t.license);
        }
        if let Some(c) = &t.connect {
            println!("{}{}:{}", label("server"), c.host, c.port);
        } else {
            let here = catalog::pick_platform(&t).map(|(tri, p)| if p.size > 0 { format!("{tri}, {}", human(p.size)) } else { tri.to_string() });
            println!("{}{}", label("this system"), here.unwrap_or_else(|| "no build".into()));
        }
        if let Some(m) = t.min_terminal {
            println!("{}{}x{}", label("terminal"), m.cols, m.rows);
        }
        if t.requires.gamepad {
            println!("{}also plays with a gamepad", label("input"));
        }
        if !t.topics.is_empty() {
            println!("{}{}", label("topics"), t.topics.join(", "));
        }
        for (l, u) in [("website", &t.website), ("source", &t.repo), ("support", &t.funding)] {
            if let Some(u) = u {
                println!("{}{u}", label(l));
            }
        }
        if let Some(p) = self.state.played.get(&t.id) {
            println!("{}{} over {} sessions", label("played"), crate::launch::duration(p.seconds), p.sessions);
        }
        // the newest release's notes, as written (the page's Markdown reads fine as text)
        if let Some(n) = t.news.iter().find(|n| !n.notes.trim().is_empty()) {
            let date = if n.date.is_empty() { String::new() } else { format!(", {}", n.date) };
            println!("\n{}{}{date}", label("news"), n.version);
            for l in n.notes.trim().lines() {
                println!("  {l}");
            }
        }
        Ok(())
    }
}

/// The download bar: green blocks with eighth-steps on a faint track, then
/// percent, size and speed, redrawn in place at most 15 times a second.
struct Bar {
    tty: bool,
    color: bool,
    start: Instant,
    last: Option<Instant>,
    drawn: bool,
    done: u64,
    total: u64,
}

impl Bar {
    fn new(tty: bool, color: bool) -> Self {
        Bar { tty, color, start: Instant::now(), last: None, drawn: false, done: 0, total: 0 }
    }

    fn draw(&mut self, done: u64, total: u64) {
        (self.done, self.total) = (done, total);
        if !self.tty || self.last.is_some_and(|l| l.elapsed() < Duration::from_millis(66) && done < total) {
            return;
        }
        self.last = Some(Instant::now());
        // speed and time left once there is half a second to measure them by
        let secs = self.start.elapsed().as_secs_f64();
        let speed = done as f64 / secs.max(0.001);
        let rate = if secs < 0.5 {
            String::new()
        } else if total > done && speed > 0.0 {
            format!("{}/s  {} left", human(speed as u64), crate::launch::duration(((total - done) as f64 / speed).ceil() as u64))
        } else {
            format!("{}/s", human(speed as u64))
        };
        let info = match (done * 100).checked_div(total) {
            Some(pct) => format!(" {pct:>3}%  {} / {}  {rate}", human(done), human(total)),
            None => format!("  {}  {rate}", human(done)),
        };
        let cols = terminal::size().map(|s| s.0 as usize).unwrap_or(80);
        let w = cols.saturating_sub(2 + info.chars().count() + 1).clamp(10, 40);
        let eighths = if total > 0 { (u128::from(done.min(total)) * w as u128 * 8 / u128::from(total)) as usize } else { 0 };
        let (full, part) = (eighths / 8, eighths % 8);
        let mut fill = "█".repeat(full);
        if part > 0 && full < w {
            fill.push([' ', '▏', '▎', '▍', '▌', '▋', '▊', '▉'][part]);
        }
        let track = "─".repeat(w - fill.chars().count());
        let mut out = std::io::stdout();
        let _ = queue!(out, cursor::MoveToColumn(0), terminal::Clear(terminal::ClearType::CurrentLine), style::Print("  "));
        if self.color {
            let _ = queue!(
                out,
                style::SetForegroundColor(rgb(theme::GREEN)),
                style::Print(fill),
                style::SetForegroundColor(rgb(theme::LINE)),
                style::Print(track),
                style::SetForegroundColor(rgb(theme::AMBER)),
                style::Print(info),
                style::ResetColor
            );
        } else {
            let _ = queue!(out, style::Print(fill), style::Print(track), style::Print(info));
        }
        let _ = out.flush();
        self.drawn = true;
    }

    /// End the bar's line; without a terminal, say once how much came down.
    fn finish(&mut self) {
        if self.drawn {
            self.draw_final();
            println!();
            self.drawn = false;
        } else if !self.tty && self.done > 0 {
            println!("  downloaded {}", human(self.done));
            self.done = 0;
        }
    }

    fn draw_final(&mut self) {
        // always leave the last state on screen, even if the throttle skipped it
        self.last = None;
        let (d, t) = (self.done, self.total);
        self.draw(d, t);
    }
}

#[cfg(test)]
mod tests {
    use super::human;

    #[test]
    fn sizes() {
        assert_eq!(human(512), "512 B");
        assert_eq!(human(2_345), "2 kB");
        assert_eq!(human(2_345_678), "2.3 MB");
    }
}
