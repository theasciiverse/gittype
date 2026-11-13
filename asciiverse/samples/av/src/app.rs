//! App state and input. Arrow keys, Tab between panels, Enter selects, Esc
//! backs out one step; Esc at the top level asks before quitting.

use std::collections::{BTreeSet, HashMap, HashSet};
use std::path::Path;
use std::sync::mpsc::{Receiver, Sender, channel};
use std::time::Instant;

use ratatui::crossterm::event::{KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use ratatui::layout::{Position, Rect};

use crate::cast::{Cast, Player};
use crate::catalog::{self, Title};
use crate::downloads::{Downloads, Phase};
use serde::{Deserialize, Serialize};
use crate::install::{self, Progress};
use crate::paths::Paths;
use crate::state::State;

/// What the list shows, set in the card under the search box and kept in
/// the config dir. It stores the kinds switched off, so a kind that is new
/// to the catalog shows up. `installed`, `updates` and `wishlist` narrow the
/// list to the player's own titles: any one, or several together (a title
/// passing any of them).
#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
#[serde(default)]
pub struct Show {
    pub hidden: BTreeSet<String>,
    pub installed: bool,
    pub updates: bool,
    pub wishlist: bool,
}

/// Where donations to Asciiverse go: the same list as the website's Support
/// menu (`SUPPORT` in hub/site/js/site.js), kept in step by hand.
pub const SUPPORT: [(&str, &str); 2] = [
    ("GitHub Sponsors", "https://github.com/sponsors/theasciiverse"),
    ("Patreon", "https://www.patreon.com/asciiverse"),
];

/// Whether an installed title has a newer version in the catalog.
pub fn update_waiting(t: &Title, state: &State) -> bool {
    state.installed.get(&t.id).is_some_and(|i| !t.version.is_empty() && i.version != t.version)
}

/// A line in the downloads pane: a download, or underneath them an update
/// on offer, which only starts when the player says yes to it.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum PaneRow {
    Download(usize),
    Offer(String),
}

/// The header's shortcuts into the show card's "only mine" boxes.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Mine {
    Installed,
    Updates,
    Wishlist,
}

impl Show {
    fn file(config: &Path) -> std::path::PathBuf {
        config.join("show.json")
    }

    pub fn load(config: &Path) -> Show {
        crate::state::read(&Show::file(config))
    }

    fn save(&self, config: &Path) -> anyhow::Result<()> {
        std::fs::create_dir_all(config)?;
        crate::state::write_atomic(&Show::file(config), &serde_json::to_string_pretty(self)?)
    }

    pub fn lets_through(&self, t: &Title, state: &State) -> bool {
        !self.hidden.contains(&t.kind)
            && (!(self.installed || self.updates || self.wishlist)
                || (self.installed && state.installed.contains_key(&t.id))
                || (self.updates && update_waiting(t, state))
                || (self.wishlist && state.wishlist.contains(&t.id)))
    }
}

/// A line in the show card.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ShowRow {
    All,
    Kind(String),
    Installed,
    Updates,
    Wishlist,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Focus {
    /// the show card, open
    Show,
    List,
    Actions,
    Page,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Action {
    Install,
    Uninstall,
    Play,
    Wishlist,
}

pub enum Modal {
    Quit,
    Help,
    /// what av is and how to start, shown once on the first run, and again with
    /// W from help
    Welcome,
    /// the downloads pane; its selection is `App::dl_sel`
    Downloads,
    Uninstall { id: String },
    Message(String),
    /// a newer av: how to update it and what it brings
    AvUpdate,
    /// after the intro, when the terminal is smaller than av is made for;
    /// drawn with the size as it is now, so it follows a resize
    Size,
    /// a question with buttons, e.g. before launching
    Choice { title: String, text: String, options: Vec<(String, Then)>, sel: usize },
}

/// What a Choice button does.
#[derive(Clone, Debug)]
pub enum Then {
    Cancel,
    /// go on with the launch checks; size_ok once the size warning was accepted
    Play { id: String, size_ok: bool },
    /// launch now, all checks done
    Launch(String),
    /// update the title, then start it
    UpdateThenPlay(String),
    /// open a link in the browser
    Open(String),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Status {
    NotInstalled,
    Installed,
    Update,
    Busy,
}

/// What a screen area does when clicked; ui::draw records these every frame.
#[derive(Clone, Copy, Debug)]
pub enum Hit {
    /// a title row, by index into `visible()`
    Row(usize),
    /// the show card's folded line, which opens and closes it
    ShowHead,
    /// a line in the open show card, by index into `show_rows()`
    ShowRow(usize),
    /// a header shortcut: only installed, only updates, only wishlisted
    Mine(Mine),
    Help,
    Support,
    /// the menu row's "av X is out"
    AvUpdate,
    /// the menu row's download line and its F2 link
    Downloads,
    /// the menu row's F5 link: fetch the catalog again
    Refresh,
    /// an action button, by index into `actions()`
    Button(usize),
    /// a button in an open choice dialog, by index into its options
    Choice(usize),
    List,
    Search,
    Detail,
}

/// Text being selected with the mouse in the page column, in the column's
/// own cells (row 0 is its top, not the screen's), so it stays on the same
/// words while the page scrolls.
pub struct Selection {
    pub id: String,
    pub from: (u16, u16),
    pub to: (u16, u16),
    /// the button is still down
    pub dragging: bool,
}

pub enum CastMsg {
    Loaded(String, Result<Cast, String>),
}

pub struct App {
    pub paths: Paths,
    pub state: State,
    pub titles: Vec<Title>,
    pub collections: catalog::Collections,
    pub catalog_url: String,
    pub offline: bool,
    pub show: Show,
    /// the highlighted line in the open show card
    pub show_row: usize,
    pub filter: String,
    pub selected: usize,
    /// the list's scroll position, in rows, kept between frames so the list
    /// only moves when the selection would leave the view
    pub list_offset: usize,
    /// The last move in the list was a click: keep the list still, with no
    /// scroll margin, so the clicked title stays under the pointer.
    pub list_clicked: bool,
    pub focus: Focus,
    pub action: usize,
    pub page_scroll: u16,
    pub modal: Option<Modal>,
    pub uninstall_keep_data: bool,
    pub status: String,
    pub downloads: Downloads,
    /// the highlighted row in the downloads pane, in `pane_rows()`
    pub dl_sel: usize,
    /// offered updates the player said "not now" to, until av restarts
    pub not_now: HashSet<String>,
    pub quit: bool,
    /// set when a title should be launched; main suspends the TUI for it
    pub launch: Option<Title>,
    pub player: Option<(String, Player)>,
    pub hits: Vec<(Rect, Hit)>,
    cast_loading: HashSet<String>,
    cast_failed: HashMap<String, String>,
    tx: Sender<Progress>,
    rx: Receiver<Progress>,
    cast_tx: Sender<CastMsg>,
    cast_rx: Receiver<CastMsg>,
    pub selection: Option<Selection>,
    /// how far a tall note (help, welcome, a message) is scrolled
    pub modal_scroll: u16,
    /// the furthest it can scroll, as last drawn
    pub modal_scroll_max: std::cell::Cell<u16>,
    /// the catalog's av when it is newer than this one
    pub av_newer: Option<catalog::AvRelease>,
    /// the title to start once its update is in ("Update first")
    play_after: Option<String>,
    /// the page column on screen and how far it is scrolled, from the last
    /// frame, to place the mouse in it
    pub page_view: Option<(Rect, u16)>,
    /// selected text the next frame takes to the clipboard
    pub to_copy: Option<String>,
    /// set when a drag ends; drawing reads the text out of the column
    pub copy_wanted: bool,
    clipboard: crate::clipboard::Clipboard,
    /// while offline ticks Installed for the run: the player's own setting,
    /// which is what show.json keeps; None once they change it themselves
    installed_saved: Option<bool>,
    /// the catalog being fetched in the background; None when not loading
    loading: Option<Receiver<anyhow::Result<catalog::Loaded>>>,
    last_tick: Instant,
    /// when the intro started; None once it is over or skipped
    pub intro: Option<Instant>,
    /// the welcome screen waits for the intro to end
    welcome_due: bool,
}

/// The terminal size av is laid out for: the 102x32 preview, the list, and
/// the title, its buttons and description under the preview.
pub const MIN_COLS: u16 = 128;
pub const MIN_ROWS: u16 = 40;

/// How long the intro runs, unless a key or click skips it.
pub const INTRO_SECS: f64 = 2.6;

/// The file whose presence says the welcome screen has been seen.
fn welcomed_file(config: &std::path::Path) -> std::path::PathBuf {
    config.join("welcomed")
}

impl App {
    pub fn new(paths: Paths, catalog_url: String) -> Self {
        let state = State::load(&paths.state);
        let set_aside = state.set_aside.clone();
        let show = Show::load(&paths.config);
        let welcome_due = !welcomed_file(&paths.config).exists();
        let (tx, rx) = channel();
        let (cast_tx, cast_rx) = channel();
        let mut app = App {
            paths,
            state,
            titles: Vec::new(),
            collections: Default::default(),
            catalog_url,
            offline: false,
            show,
            show_row: 0,
            filter: String::new(),
            selected: 0,
            list_offset: 0,
            list_clicked: false,
            focus: Focus::List,
            action: 0,
            page_scroll: 0,
            modal: None,
            uninstall_keep_data: true,
            status: String::new(),
            downloads: Downloads::default(),
            dl_sel: 0,
            not_now: HashSet::new(),
            quit: false,
            launch: None,
            player: None,
            hits: Vec::new(),
            cast_loading: HashSet::new(),
            cast_failed: HashMap::new(),
            tx,
            rx,
            cast_tx,
            cast_rx,
            selection: None,
            modal_scroll: 0,
            modal_scroll_max: Default::default(),
            av_newer: None,
            play_after: None,
            page_view: None,
            to_copy: None,
            copy_wanted: false,
            clipboard: Default::default(),
            installed_saved: None,
            loading: None,
            last_tick: Instant::now(),
            intro: Some(Instant::now()),
            welcome_due,
        };
        if !set_aside.is_empty() {
            // shown once the intro is over; the files are kept for a look
            app.modal = Some(Modal::Message(format!(
                "Part of your library could not be read, so it was set aside and av starts from an empty list:\n\n{}",
                set_aside.join("\n")
            )));
        }
        app.refresh(false);
        app
    }

    /// Fetch the catalog on a thread, so the intro and the screen keep
    /// drawing while a slow or dead network makes it wait; `tick` takes
    /// the result.
    pub fn refresh(&mut self, force: bool) {
        if self.loading.is_some() {
            return;
        }
        // previews that failed (a network drop while browsing) get another go
        self.cast_failed.clear();
        let (tx, rx) = channel();
        let (url, cache) = (self.catalog_url.clone(), self.paths.cache.clone());
        std::thread::spawn(move || {
            let _ = tx.send(catalog::load(&url, &cache, force));
        });
        self.loading = Some(rx);
        self.status = "loading the catalog".into();
    }

    pub fn is_loading(&self) -> bool {
        self.loading.is_some()
    }

    fn loaded(&mut self, res: anyhow::Result<catalog::Loaded>) {
        match res {
            Ok(l) => {
                self.av_newer = crate::selfupdate::newer(l.catalog.av.as_ref()).cloned();
                if let Some(r) = &self.av_newer {
                    log::info!("av {} is out", r.version);
                }
                // grouped by kind (catalog::rank), alphabetical within each, and
                // last what is installed but no longer in the catalog
                let gone = catalog::gone(&l.catalog.titles, &self.state.installed);
                let mut titles: Vec<Title> = l.catalog.titles.into_values().chain(gone).collect();
                catalog::sort(&mut titles);
                catalog::index(&mut titles, &l.catalog.collections);
                self.titles = titles;
                self.collections = l.catalog.collections;
                self.offline = l.offline;
                if let Some(msg) = &l.outdated {
                    // the last catalog this av could read: the library still works
                    self.status = "the catalog is newer than this av; showing the last one it read (F1 says how to update)".into();
                    if self.modal.is_none() {
                        self.modal = Some(Modal::Message(msg.clone()));
                    }
                } else if l.offline {
                    // for this run only: show.json keeps the player's own choice
                    self.installed_saved.get_or_insert(self.show.installed);
                    self.show.installed = true;
                    self.status = "offline: showing the cached catalog".into();
                } else {
                    self.status = format!("{} titles", self.titles.len());
                }
            }
            Err(e) => {
                log::error!("catalog: {e:#}");
                // the status bar is one line; the note below has the whole message
                self.status = format!("{e:#}").lines().next().unwrap_or_default().to_string();
                // not over the welcome screen; the status line and the empty list say it then
                if self.modal.is_none() {
                    self.modal = Some(Modal::Message(format!("Could not load the catalog.\n\n{e:#}")));
                }
            }
        }
        self.clamp();
    }

    pub fn visible(&self) -> Vec<&Title> {
        let f = self.filter.to_lowercase();
        let mut v: Vec<&Title> = self.titles
            .iter()
            .filter(|t| self.show.lets_through(t, &self.state))
            .filter(|t| t.matches(&f))
            .collect();
        if !f.is_empty() {
            // within each kind's group, titles whose name matches come first;
            // sort_by_key is stable, so the alphabetical order is kept inside
            // the lowercased name is the search text's first line (catalog::index)
            v.sort_by_cached_key(|t| (catalog::rank(t), !t.haystack.lines().next().unwrap_or("").contains(&f)));
        }
        v
    }

    /// After the search changes: the first title whose name matches, wherever
    /// its kind's group sits, or the top of the list. Searching "tarts" lands
    /// on tarts, not on a story whose description says "starts".
    fn name_match(&self) -> usize {
        let f = self.filter.to_lowercase();
        let v = self.visible();
        v.iter()
            .position(|t| t.title.to_lowercase() == f)
            .or_else(|| v.iter().position(|t| t.title.to_lowercase().starts_with(&f)))
            .or_else(|| v.iter().position(|t| t.title.to_lowercase().contains(&f)))
            .unwrap_or(0)
    }

    /// The highlighted title.
    pub fn current(&self) -> Option<&Title> {
        self.visible().get(self.selected).copied()
    }

    pub fn status_of(&self, t: &Title) -> Status {
        if self.downloads.busy(&t.id) {
            return Status::Busy;
        }
        match self.state.installed.get(&t.id) {
            None => Status::NotInstalled,
            Some(_) if update_waiting(t, &self.state) => Status::Update,
            Some(_) => Status::Installed,
        }
    }

    pub fn actions(&self, t: &Title) -> Vec<(Action, String, bool)> {
        let wish = if self.state.wishlist.contains(&t.id) { "♥ Wishlisted" } else { "♡ Wishlist" };
        if t.is_mud() {
            return vec![(Action::Play, "Connect".into(), true), (Action::Wishlist, wish.into(), true)];
        }
        let st = self.status_of(t);
        let first = match st {
            Status::NotInstalled => (Action::Install, "Install".into(), catalog::pick_platform(t).is_some()),
            Status::Update => (Action::Install, "Update".into(), true),
            Status::Busy => (Action::Install, "Installing…".into(), false),
            Status::Installed => (Action::Uninstall, "Uninstall".into(), true),
        };
        let can_play = matches!(st, Status::Installed | Status::Update);
        let mut v = vec![first, (Action::Play, "Play".into(), can_play), (Action::Wishlist, wish.into(), true)];
        if st == Status::Update {
            v.insert(2, (Action::Uninstall, "Uninstall".into(), true));
        }
        v
    }

    /// The show card's lines: All, each kind the catalog has (in list order),
    /// then the two that narrow to the player's own titles.
    pub fn show_rows(&self) -> Vec<ShowRow> {
        let mut kinds: Vec<&str> = Vec::new();
        for t in &self.titles {
            if kinds.last() != Some(&t.kind.as_str()) && !kinds.contains(&t.kind.as_str()) {
                kinds.push(&t.kind);
            }
        }
        let mut rows = vec![ShowRow::All];
        rows.extend(kinds.into_iter().map(|k| ShowRow::Kind(k.to_string())));
        rows.extend([ShowRow::Installed, ShowRow::Updates, ShowRow::Wishlist]);
        rows
    }

    pub fn row_on(&self, r: &ShowRow) -> bool {
        match r {
            ShowRow::All => self.show.hidden.is_empty(),
            ShowRow::Kind(k) => !self.show.hidden.contains(k),
            ShowRow::Installed => self.show.installed,
            ShowRow::Updates => self.show.updates,
            ShowRow::Wishlist => self.show.wishlist,
        }
    }

    /// Installed titles with a newer version in the catalog.
    pub fn updates(&self) -> usize {
        self.titles.iter().filter(|t| update_waiting(t, &self.state)).count()
    }

    /// A header shortcut: show only that part of the player's titles, or,
    /// when the list already shows exactly that, all of them again.
    fn only(&mut self, m: Mine) {
        let keep = self.current().map(|t| t.id.clone());
        let want = (m == Mine::Installed, m == Mine::Updates, m == Mine::Wishlist);
        let now = (self.show.installed, self.show.updates, self.show.wishlist);
        (self.show.installed, self.show.updates, self.show.wishlist) = if now == want { (false, false, false) } else { want };
        self.installed_saved = None;
        self.show_changed(keep);
    }

    /// After the show settings change: save them, and keep the highlighted
    /// title selected when it is still in the list.
    fn show_changed(&mut self, keep: Option<String>) {
        log::info!("show {:?}", self.show);
        let mut saved = self.show.clone();
        if let Some(i) = self.installed_saved {
            saved.installed = i;
        }
        if let Err(e) = saved.save(&self.paths.config) {
            self.status = format!("could not save what to show: {e:#}");
        }
        self.selected = keep.and_then(|id| self.visible().iter().position(|t| t.id == id)).unwrap_or(0);
        self.page_scroll = 0;
        self.action = 0;
    }

    /// Put an install or update in the downloads queue and start it when
    /// nothing else runs.
    fn enqueue(&mut self, t: &Title) {
        let from = self.state.installed.get(&t.id).map(|i| i.version.clone());
        if self.downloads.queue(&t.id, &t.title, &t.version, from) {
            if self.downloads.running().is_some() {
                self.status = format!("{} queued; F2 shows the downloads", t.title);
            }
            self.pump();
        }
    }

    /// Start the next queued download if nothing runs.
    fn pump(&mut self) {
        let Some(d) = self.downloads.start_next() else { return };
        let (id, cancel, job) = (d.id.clone(), d.cancel.clone(), d.job);
        let Some(t) = self.titles.iter().find(|t| t.id == id).cloned() else {
            self.downloads.fail(job, "no longer in the catalog".into());
            return self.pump();
        };
        log::info!("download start {id}");
        install::spawn(
            install::Job {
                title: t.clone(),
                catalog_url: self.catalog_url.clone(),
                cache: self.paths.cache.clone(),
                dest: self.paths.title_dir(&t.kind, &t.id),
                saves: self.paths.saves(&t.id),
                bin: self.paths.bin.clone(),
                previous: Some(self.paths.installed_dir(&t, self.state.installed.get(&t.id))),
                cancel: Some(cancel),
                job,
            },
            self.tx.clone(),
        );
    }

    /// u in the pane: yes to every update on offer at once.
    fn update_all(&mut self) {
        let waiting: Vec<Title> = self.offers().into_iter().cloned().collect();
        self.status = match waiting.len() {
            0 => "everything is up to date".into(),
            n => format!("{n} update{} queued", if n == 1 { "" } else { "s" }),
        };
        for t in &waiting {
            self.enqueue(t);
        }
    }

    /// Updates waiting that are not queued, running or put off: offered under
    /// the downloads, each started only by a yes.
    pub fn offers(&self) -> Vec<&Title> {
        self.titles
            .iter()
            .filter(|t| update_waiting(t, &self.state) && !self.downloads.busy(&t.id) && !self.not_now.contains(&t.id))
            .collect()
    }

    /// The pane's lines: every download (running, queued, failed, cancelled,
    /// done), then underneath them the updates on offer.
    pub fn pane_rows(&self) -> Vec<PaneRow> {
        let mut rows: Vec<PaneRow> = self.downloads.ordered().into_iter().map(PaneRow::Download).collect();
        rows.extend(self.offers().into_iter().map(|t| PaneRow::Offer(t.id.clone())));
        rows
    }

    fn open_downloads(&mut self) {
        self.dl_sel = 0;
        self.modal = Some(Modal::Downloads);
    }

    fn downloads_key(&mut self, k: KeyEvent) {
        let rows = self.pane_rows();
        let at = rows.get(self.dl_sel).cloned();
        match k.code {
            KeyCode::Esc | KeyCode::F(2) => return,
            KeyCode::Up => self.dl_sel = self.dl_sel.saturating_sub(1),
            KeyCode::Down => self.dl_sel = (self.dl_sel + 1).min(rows.len().saturating_sub(1)),
            // yes: an offered update goes in the queue; a failed or cancelled
            // download goes back in it
            KeyCode::Enter => match at {
                Some(PaneRow::Offer(id)) => {
                    if let Some(t) = self.titles.iter().find(|t| t.id == id).cloned() {
                        self.enqueue(&t);
                    }
                }
                Some(PaneRow::Download(i)) => {
                    let retried = self.downloads.retry(i);
                    if retried {
                        self.pump();
                    }
                }
                None => {}
            },
            KeyCode::Delete | KeyCode::Backspace => match at {
                Some(PaneRow::Offer(id)) => {
                    self.not_now.insert(id);
                }
                Some(PaneRow::Download(i)) => {
                    // a cancel takes effect at once, even while the network hangs
                    self.downloads.cancel_or_remove(i);
                    self.pump();
                }
                None => {}
            },
            KeyCode::Char('u') => self.update_all(),
            KeyCode::Char('r') => {
                if self.downloads.retry_failed() > 0 {
                    self.pump();
                }
            }
            KeyCode::Char('c') => self.downloads.clear_finished(),
            _ => {}
        }
        self.dl_sel = self.dl_sel.min(self.pane_rows().len().saturating_sub(1));
        self.modal = Some(Modal::Downloads);
    }

    /// How to update av, and what the new one brings.
    pub fn av_update(&mut self) {
        if self.av_newer.is_some() {
            self.open(Modal::AvUpdate);
        }
    }

    /// Open a dialog at its top.
    pub fn open(&mut self, m: Modal) {
        self.modal_scroll = 0;
        self.modal = Some(m);
    }

    fn support(&mut self) {
        let mut options: Vec<(String, Then)> = SUPPORT.iter().map(|(n, u)| (n.to_string(), Then::Open(u.to_string()))).collect();
        options.push(("More ways to help".into(), Then::Open("https://asciiverse.sh/support.html".into())));
        options.push(("Close".into(), Then::Cancel));
        let links: String = SUPPORT.iter().map(|(n, u)| format!("\n  {n}: {u}")).collect();
        self.modal = Some(Modal::Choice {
            title: " support Asciiverse ".into(),
            text: format!(
                "Asciiverse is free and runs on donations. Titles whose developers take donations link to them on their own page.\n{links}\n\nPick one to open it in your browser."
            ),
            options,
            sel: 0,
        });
    }

    /// Switch a show card line; the highlighted title stays selected when it
    /// is still in the list.
    fn toggle_show(&mut self, i: usize) {
        let Some(r) = self.show_rows().get(i).cloned() else { return };
        let keep = self.current().map(|t| t.id.clone());
        match &r {
            // all on, or when they already are, all off
            ShowRow::All if self.show.hidden.is_empty() => {
                self.show.hidden = self.titles.iter().map(|t| t.kind.clone()).collect();
            }
            ShowRow::All => self.show.hidden.clear(),
            ShowRow::Kind(k) => {
                if !self.show.hidden.remove(k) {
                    self.show.hidden.insert(k.clone());
                }
            }
            ShowRow::Installed => {
                self.show.installed = !self.show.installed;
                self.installed_saved = None;
            }
            ShowRow::Updates => self.show.updates = !self.show.updates,
            ShowRow::Wishlist => self.show.wishlist = !self.show.wishlist,
        }
        log::info!("show card {r:?}");
        self.show_changed(keep);
    }

    fn show_key(&mut self, k: KeyEvent) {
        let n = self.show_rows().len();
        match k.code {
            KeyCode::Up => self.show_row = self.show_row.saturating_sub(1),
            KeyCode::Down if self.show_row + 1 >= n => self.focus = Focus::List,
            KeyCode::Down => self.show_row += 1,
            KeyCode::Char(' ') | KeyCode::Enter => self.toggle_show(self.show_row),
            // typing searches, as it does from the list
            KeyCode::Char(_) | KeyCode::Backspace => {
                self.focus = Focus::List;
                self.list_key(k);
            }
            _ => {}
        }
    }

    fn clamp(&mut self) {
        let n = self.visible().len();
        if self.selected >= n {
            self.selected = n.saturating_sub(1);
        }
    }

    fn select(&mut self, i: usize) {
        self.list_clicked = false;
        if i != self.selected {
            self.selected = i;
            self.selection = None;
            self.page_scroll = 0;
            self.action = 0;
        }
    }

    /// Called every frame: drain worker messages, advance the preview.
    pub fn tick(&mut self) {
        if self.intro.is_some_and(|t| t.elapsed().as_secs_f64() >= INTRO_SECS) {
            self.end_intro();
        }
        // the first-run welcome waits for whatever note came up after the intro
        if self.welcome_due && self.intro.is_none() && self.modal.is_none() {
            self.welcome();
        }
        if let Some(text) = self.to_copy.take() {
            self.status = self.clipboard.copy(&text);
        }
        if let Some(res) = self.loading.as_ref().and_then(|rx| rx.try_recv().ok()) {
            self.loading = None;
            self.loaded(res);
        }
        let dt = self.last_tick.elapsed().as_secs_f64();
        self.last_tick = Instant::now();
        let mut start = None;
        while let Ok(p) = self.rx.try_recv() {
            match p {
                Progress::Bytes { job, done, total } => {
                    if let Some(d) = self.downloads.running_mut(job) {
                        (d.done, d.total) = (done, total);
                    }
                }
                Progress::Status { job, msg } => {
                    log::info!("download {job}: {msg}");
                    if let Some(d) = self.downloads.running_mut(job) {
                        d.step = msg;
                    }
                }
                Progress::Done { job, id, installed } => {
                    // also one cancelled too late to stop: its files are in place
                    self.downloads.finish(job);
                    self.status = format!("installed {id} {}", installed.version);
                    if !self.paths.bin_on_path()
                        && let Some(b) = &self.paths.bin
                    {
                        // the welcome screen explains this once; here a reminder is enough
                        self.status += &format!("; {} is not on your PATH, so start it from av", crate::paths::tilde(b));
                    }
                    let current = self.current().is_some_and(|t| t.id == id);
                    if self.play_after.as_deref() == Some(id.as_str()) {
                        start = Some(id.clone());
                    }
                    self.state.installed.insert(id, installed);
                    self.save();
                    if current {
                        // the obvious next step after installing is playing
                        if let Some(t) = self.current() {
                            self.action = self.actions(t).iter().position(|(a, ..)| *a == Action::Play).unwrap_or(0);
                        }
                    }
                }
                Progress::Failed { job, id, error } => {
                    if !self.downloads.fail(job, error) {
                        // a download already cancelled in the list, giving up late
                        continue;
                    }
                    let cancelled = self.downloads.list.iter().any(|d| d.id == id && d.phase == Phase::Cancelled);
                    self.status = if cancelled { format!("{id}: cancelled") } else { format!("{id}: install failed; F2 shows why") };
                    if self.play_after.as_deref() == Some(id.as_str()) && !cancelled {
                        // they wanted to play: the version they have, and why
                        self.play_after = None;
                        let have = self.state.installed.get(&id).map(|i| i.version.clone()).unwrap_or_default();
                        self.status = format!("{id}: the update failed (F2 shows why), so playing {have}");
                        self.launch = self.titles.iter().find(|t| t.id == id).cloned();
                    } else if cancelled && self.play_after.as_deref() == Some(id.as_str()) {
                        self.play_after = None;
                    }
                }
            }
            self.pump();
        }
        // "Update first": the update is in, so on to the game
        if let Some(id) = start {
            self.play_after = None;
            if let Some(t) = self.titles.iter().find(|t| t.id == id).cloned() {
                self.request_play(&t, false);
            }
        }
        while let Ok(CastMsg::Loaded(id, res)) = self.cast_rx.try_recv() {
            self.cast_loading.remove(&id);
            match res {
                Ok(cast) => {
                    log::debug!("cast {id}: {}x{}, {:.1}s", cast.cols, cast.rows, cast.duration);
                    let at = self.titles.iter().find(|t| t.id == id).and_then(|t| t.thumbnail_at).unwrap_or(0.0);
                    let mut p = Player::new(cast);
                    p.seek((at - 3.0).max(0.0));
                    if self.current().is_some_and(|t| t.id == id) {
                        self.player = Some((id, p));
                    }
                }
                Err(e) => {
                    log::warn!("cast {id}: {e}");
                    self.cast_failed.insert(id, e);
                }
            }
        }
        self.ensure_cast();
        if let Some((_, p)) = &mut self.player {
            p.tick(dt.min(0.25));
        }
    }

    /// Load the selected title's cast in the background (cached on disk).
    fn ensure_cast(&mut self) {
        // every frame: only the id and the cast's address, not the whole title
        let Some((id, cast_url)) = self.current().map(|t| (t.id.clone(), t.cast_url.clone())) else { return };
        if self.player.as_ref().is_some_and(|(p, _)| *p == id) {
            return;
        }
        self.player = None;
        let Some(url) = cast_url else { return };
        if self.cast_loading.contains(&id) || self.cast_failed.contains_key(&id) {
            return;
        }
        let url = catalog::resolve(&self.catalog_url, &url);
        // keyed by URL too: a re-recorded cast gets a new URL from the catalog
        // build, and must not be shadowed by the old file in the cache
        let file = self.paths.cache.join("casts").join(format!("{id}-{:016x}.cast", catalog::fnv1a(&url)));
        self.cast_loading.insert(id.clone());
        let tx = self.cast_tx.clone();
        std::thread::spawn(move || {
            let res = (|| -> anyhow::Result<Cast> {
                let src = match std::fs::read_to_string(&file) {
                    Ok(s) => s,
                    Err(_) => {
                        let s = catalog::get_text(&url, 256 << 20)?;
                        std::fs::create_dir_all(file.parent().unwrap())?;
                        crate::state::write_atomic(&file, &s)?;
                        s
                    }
                };
                Cast::parse(&src)
            })();
            let _ = tx.send(CastMsg::Loaded(id, res.map_err(|e| format!("{e:#}"))));
        });
    }

    /// None when there is nothing to show, else a placeholder message.
    pub fn cast_note(&self, t: &Title) -> Option<String> {
        if t.gone {
            Some("no longer in the catalog, so no preview".into())
        } else if t.cast_url.is_none() {
            Some("no preview recorded yet".into())
        } else if let Some(e) = self.cast_failed.get(&t.id) {
            Some(format!("preview unavailable: {e}"))
        } else if self.cast_loading.contains(&t.id) {
            Some("loading preview…".into())
        } else {
            None
        }
    }

    fn save(&mut self) {
        if let Err(e) = self.state.save() {
            self.status = format!("could not save state: {e:#}");
        }
    }

    /// The intro is over, run out or skipped: on to the welcome screen on the
    /// first run (unless a note about the catalog is up), else the library.
    fn end_intro(&mut self) {
        if self.intro.take().is_none() {
            return;
        }
        log::info!("intro over");
        let small = ratatui::crossterm::terminal::size().is_ok_and(|(w, h)| w < MIN_COLS || h < MIN_ROWS);
        if small && self.modal.is_none() {
            // the welcome comes once this note is closed (see tick)
            self.modal = Some(Modal::Size);
        } else if self.welcome_due && self.modal.is_none() {
            self.welcome();
        }
    }

    /// Open the welcome screen, and remember it was seen.
    pub fn welcome(&mut self) {
        self.welcome_due = false;
        self.modal = Some(Modal::Welcome);
        let file = welcomed_file(&self.paths.config);
        let saved = std::fs::create_dir_all(&self.paths.config).and_then(|_| std::fs::write(&file, crate::state::now_rfc3339()));
        if let Err(e) = saved {
            log::warn!("could not write {}: {e}", file.display());
        }
    }

    pub fn on_key(&mut self, k: KeyEvent) {
        log::debug!("key {:?} {:?} focus={:?} modal={}", k.code, k.modifiers, self.focus, self.modal.is_some());
        if k.code == KeyCode::Char('c') && k.modifiers.contains(KeyModifiers::CONTROL) {
            self.quit = true;
            return;
        }
        // any key skips the intro, and does nothing else
        if self.intro.is_some() {
            self.end_intro();
            return;
        }
        if let Some(m) = self.modal.take() {
            self.on_modal_key(m, k);
            return;
        }
        match k.code {
            KeyCode::Tab => {
                self.focus = match self.focus {
                    Focus::Show | Focus::List => Focus::Actions,
                    Focus::Actions => Focus::Page,
                    Focus::Page => Focus::List,
                };
            }
            KeyCode::BackTab => {
                self.focus = match self.focus {
                    Focus::Show | Focus::List => Focus::Page,
                    Focus::Actions => Focus::List,
                    Focus::Page => Focus::Actions,
                };
            }
            KeyCode::F(5) => self.refresh(true),
            KeyCode::F(1) => self.open(Modal::Help),
            KeyCode::F(2) => self.open_downloads(),
            // "/ type to search": the slash starts a new search from any panel,
            // as in less and vim; no title name has one, so it is never typed
            KeyCode::Char('/') => {
                self.focus = Focus::List;
                if !self.filter.is_empty() {
                    let keep = self.current().map(|t| t.id.clone());
                    self.filter.clear();
                    self.selected = keep.and_then(|id| self.visible().iter().position(|t| t.id == id)).unwrap_or(0);
                }
            }
            KeyCode::Esc => {
                if self.focus != Focus::List {
                    self.focus = Focus::List;
                } else if !self.filter.is_empty() {
                    self.filter.clear();
                    self.clamp();
                } else {
                    self.modal = Some(Modal::Quit);
                }
            }
            _ => match self.focus {
                Focus::Show => self.show_key(k),
                Focus::List => self.list_key(k),
                Focus::Actions => self.actions_key(k),
                Focus::Page => self.page_key(k),
            },
        }
    }

    pub fn on_mouse(&mut self, m: MouseEvent) {
        let pos = Position::new(m.column, m.row);
        // later entries sit on top (buttons over the pane they are drawn in)
        let hit = self.hits.iter().rev().find(|(r, _)| r.contains(pos)).map(|(_, h)| *h);
        if !matches!(m.kind, MouseEventKind::Moved | MouseEventKind::Drag(_)) {
            log::debug!("mouse {:?} at {},{} hit {hit:?}", m.kind, m.column, m.row);
        }
        if self.intro.is_some() {
            if matches!(m.kind, MouseEventKind::Down(_)) {
                self.end_intro();
            }
            return;
        }
        if let Some(modal) = self.modal.take() {
            let (down, m_kind) = (matches!(m.kind, MouseEventKind::Down(_)), m.kind);
            match (modal, hit) {
                // a click on a choice dialog's button picks it
                (Modal::Choice { options, .. }, Some(Hit::Choice(i))) if m.kind == MouseEventKind::Down(MouseButton::Left) => {
                    if let Some((_, then)) = options.get(i).cloned() {
                        self.choose(then);
                    }
                }
                // a click closes a note; other dialogs stay open
                // the wheel scrolls a tall note
                (m @ (Modal::Message(_) | Modal::Help | Modal::Welcome | Modal::AvUpdate), _)
                    if matches!(m_kind, MouseEventKind::ScrollUp | MouseEventKind::ScrollDown) =>
                {
                    let step: i32 = if m_kind == MouseEventKind::ScrollUp { -3 } else { 3 };
                    self.modal_scroll = (self.modal_scroll as i32 + step).clamp(0, self.modal_scroll_max.get() as i32) as u16;
                    self.modal = Some(m);
                }
                (Modal::Message(_) | Modal::Help | Modal::Welcome | Modal::Size | Modal::AvUpdate, _) if down => {}
                (modal, _) => self.modal = Some(modal),
            }
            return;
        }
        let dragging = self.selection.as_ref().is_some_and(|s| s.dragging);
        if m.kind == MouseEventKind::Down(MouseButton::Left) {
            self.selection = None;
        }
        match (m.kind, hit) {
            // selecting text in the page: wherever the mouse goes while held
            (MouseEventKind::Drag(MouseButton::Left), _) if dragging => self.drag_to(pos),
            (MouseEventKind::Up(MouseButton::Left), _) if dragging => {
                if let Some(s) = self.selection.as_mut() {
                    s.dragging = false;
                }
                match &self.selection {
                    Some(s) if s.from == s.to => self.selection = None,
                    Some(_) => self.copy_wanted = true,
                    None => {}
                }
            }
            (MouseEventKind::Down(MouseButton::Left), Some(Hit::Row(i))) => {
                self.select(i);
                self.list_clicked = true;
                self.focus = Focus::List;
            }
            (MouseEventKind::Down(MouseButton::Left), Some(Hit::ShowHead)) => {
                self.focus = if self.focus == Focus::Show { Focus::List } else { Focus::Show };
            }
            (MouseEventKind::Down(MouseButton::Left), Some(Hit::Mine(m))) => self.only(m),
            (MouseEventKind::Down(MouseButton::Left), Some(Hit::Help)) => self.open(Modal::Help),
            (MouseEventKind::Down(MouseButton::Left), Some(Hit::Support)) => self.support(),
            (MouseEventKind::Down(MouseButton::Left), Some(Hit::AvUpdate)) => self.av_update(),
            (MouseEventKind::Down(MouseButton::Left), Some(Hit::Downloads)) => self.open_downloads(),
            (MouseEventKind::Down(MouseButton::Left), Some(Hit::Refresh)) => self.refresh(true),
            (MouseEventKind::Down(MouseButton::Left), Some(Hit::ShowRow(i))) => {
                self.focus = Focus::Show;
                self.show_row = i;
                self.toggle_show(i);
            }
            (MouseEventKind::Down(MouseButton::Left), Some(Hit::Button(i))) => {
                let Some(t) = self.current().cloned() else { return };
                self.focus = Focus::Actions;
                self.action = i;
                if let Some((a, _, true)) = self.actions(&t).get(i).cloned() {
                    self.run(a, &t);
                }
            }
            (MouseEventKind::Down(MouseButton::Left), Some(Hit::Detail)) => {
                self.focus = Focus::Page;
                if let (Some(id), Some(at)) = (self.current().map(|t| t.id.clone()), self.in_page(pos)) {
                    self.selection = Some(Selection { id, from: at, to: at, dragging: true });
                }
            }
            // typing goes to the search box whenever the list has focus
            (MouseEventKind::Down(MouseButton::Left), Some(Hit::Search | Hit::List)) => self.focus = Focus::List,
            (MouseEventKind::ScrollDown, Some(Hit::Row(_) | Hit::List)) => {
                let n = self.visible().len();
                self.select((self.selected + 1).min(n.saturating_sub(1)));
            }
            (MouseEventKind::ScrollUp, Some(Hit::Row(_) | Hit::List)) => self.select(self.selected.saturating_sub(1)),
            (MouseEventKind::ScrollDown, Some(Hit::Detail | Hit::Button(_))) => self.page_scroll += 3,
            (MouseEventKind::ScrollUp, Some(Hit::Detail | Hit::Button(_))) => {
                self.page_scroll = self.page_scroll.saturating_sub(3)
            }
            _ => {}
        }
    }

    /// A screen cell as a cell of the page column, held inside the column.
    fn in_page(&self, pos: Position) -> Option<(u16, u16)> {
        let (r, top) = self.page_view?;
        // max then min, not clamp: a column shrunk to nothing mid-drag must not panic
        let x = pos.x.max(r.x).min(r.right().saturating_sub(1)).saturating_sub(r.x);
        let y = pos.y.max(r.y).min(r.bottom().saturating_sub(1)).saturating_sub(r.y);
        Some((x, y + top))
    }

    /// The selection follows the mouse, and dragging past the top or bottom
    /// of the page scrolls it.
    fn drag_to(&mut self, pos: Position) {
        if let Some((r, _)) = self.page_view {
            if pos.y < r.y {
                self.page_scroll = self.page_scroll.saturating_sub(1);
            } else if pos.y >= r.bottom() {
                self.page_scroll += 1;
            }
        }
        let at = self.in_page(pos);
        if let (Some(s), Some(at)) = (self.selection.as_mut(), at) {
            s.to = at;
        }
    }

    fn list_key(&mut self, k: KeyEvent) {
        let n = self.visible().len();
        match k.code {
            // up from the top opens the show card above the list
            KeyCode::Up if self.selected == 0 => {
                self.focus = Focus::Show;
                self.show_row = 0;
            }
            KeyCode::Up => self.select(self.selected - 1),
            KeyCode::Down => self.select((self.selected + 1).min(n.saturating_sub(1))),
            KeyCode::PageUp => self.select(self.selected.saturating_sub(10)),
            KeyCode::PageDown => self.select((self.selected + 10).min(n.saturating_sub(1))),
            KeyCode::Home => self.select(0),
            KeyCode::End => self.select(n.saturating_sub(1)),
            KeyCode::Enter => {
                self.focus = Focus::Actions;
                self.action = 0;
            }
            KeyCode::Backspace => {
                self.filter.pop();
                self.selected = self.name_match();
            }
            KeyCode::Char(c) if !k.modifiers.contains(KeyModifiers::CONTROL) => {
                self.filter.push(c);
                self.selected = self.name_match();
            }
            _ => {}
        }
    }

    fn actions_key(&mut self, k: KeyEvent) {
        let Some(t) = self.current().cloned() else { return };
        let acts = self.actions(&t);
        match k.code {
            KeyCode::Left => self.action = self.action.saturating_sub(1),
            KeyCode::Right => self.action = (self.action + 1).min(acts.len() - 1),
            KeyCode::Up | KeyCode::Down => self.focus = Focus::List,
            KeyCode::Enter => {
                if let Some((a, _, enabled)) = acts.get(self.action)
                    && *enabled
                {
                    self.run(*a, &t);
                }
            }
            _ => {}
        }
    }

    fn page_key(&mut self, k: KeyEvent) {
        match k.code {
            KeyCode::Up => self.page_scroll = self.page_scroll.saturating_sub(1),
            KeyCode::Down => self.page_scroll += 1,
            KeyCode::PageUp => self.page_scroll = self.page_scroll.saturating_sub(10),
            KeyCode::PageDown => self.page_scroll += 10,
            KeyCode::Char(' ') => {
                if let Some((_, p)) = &mut self.player {
                    p.playing = !p.playing;
                }
            }
            _ => {}
        }
    }

    fn run(&mut self, a: Action, t: &Title) {
        log::info!("action {a:?} on {}", t.id);
        match a {
            Action::Install => self.enqueue(t),
            Action::Uninstall => {
                self.uninstall_keep_data = true;
                self.modal = Some(Modal::Uninstall { id: t.id.clone() });
            }
            Action::Play => self.request_play(t, false),
            Action::Wishlist => {
                if !self.state.wishlist.remove(&t.id) {
                    self.state.wishlist.insert(t.id.clone());
                }
                self.save();
            }
        }
    }

    /// Checks before launching: terminal size against the title's minimum,
    /// then whether an update is waiting. Each asks; neither blocks.
    fn request_play(&mut self, t: &Title, size_ok: bool) {
        if !size_ok
            && let (Some(m), Ok((w, h))) = (t.min_terminal, ratatui::crossterm::terminal::size())
            && (w < m.cols || h < m.rows)
        {
            self.modal = Some(Modal::Choice {
                title: " terminal size ".into(),
                text: format!(
                    "{} needs a terminal of at least {}x{}. This one is {w}x{h}.\n\nEnlarge the window first, or play anyway.",
                    t.title, m.cols, m.rows
                ),
                options: vec![("Cancel".into(), Then::Cancel), ("Play anyway".into(), Then::Play { id: t.id.clone(), size_ok: true })],
                sel: 0,
            });
            return;
        }
        if !t.is_mud() && self.status_of(t) == Status::Update {
            let have = self.state.installed.get(&t.id).map(|i| i.version.clone()).unwrap_or_default();
            if self.offline {
                // no update to be had now: play what is installed
                self.status = format!("offline: playing {} {have}; {} waits for a connection", t.title, t.version);
                self.launch = Some(t.clone());
                return;
            }
            self.modal = Some(Modal::Choice {
                title: " update available ".into(),
                text: format!("{} {} is out; you have {have}.", t.title, t.version),
                options: vec![
                    ("Update first".into(), Then::UpdateThenPlay(t.id.clone())),
                    (format!("Play {have}"), Then::Launch(t.id.clone())),
                ],
                sel: 0,
            });
            return;
        }
        self.launch = Some(t.clone());
    }

    fn choose(&mut self, then: Then) {
        let find = |app: &App, id: &str| app.titles.iter().find(|t| t.id == id).cloned();
        match then {
            Then::Cancel => {}
            Then::Play { id, size_ok } => {
                if let Some(t) = find(self, &id) {
                    self.request_play(&t, size_ok);
                }
            }
            Then::Launch(id) => self.launch = find(self, &id),
            Then::UpdateThenPlay(id) => {
                if let Some(t) = find(self, &id) {
                    self.play_after = Some(id);
                    self.run(Action::Install, &t);
                }
            }
            Then::Open(url) => {
                self.status = match crate::launch::open_url(&url) {
                    Ok(()) => format!("opened {url} in your browser"),
                    Err(e) => format!("could not open a browser ({e}); the link is {url}"),
                };
            }
        }
    }

    /// Back from a title: record play time, then say how it went.
    pub fn lera(&self) -> Option<std::path::PathBuf> {
        crate::launch::lera(&self.paths)
    }

    pub fn finished(&mut self, t: &Title, o: crate::launch::Outcome) {
        let sessions = self.state.record_play(&t.id, o.seconds);
        self.save();
        let played = crate::launch::duration(o.seconds);
        match &o.status {
            Ok(st) => match crate::launch::failure(st) {
                None => {
                    self.status = format!("back from {}, played {played}", t.title);
                    // now and then, not every time: point at the developer's own donation page
                    if let Some(url) = &t.funding
                        && sessions.is_multiple_of(5)
                    {
                        self.status += &format!(". Enjoying it? Support its developer: {url}");
                    }
                }
                Some(how) => {
                    let mut text = format!("{} {how} after {played}.", t.title);
                    if let Some(log) = &o.log {
                        let tail = crate::launch::tail(log, 12);
                        if !tail.is_empty() {
                            text += &format!("\n\nLast lines it printed:\n\n{tail}");
                        }
                        text += &format!("\n\nFull log: {}", crate::paths::tilde(log));
                    }
                    self.status = format!("{} {how}", t.title);
                    self.modal = Some(Modal::Message(text));
                }
            },
            Err(e) if t.connect.is_some() && e.kind() == std::io::ErrorKind::NotFound => {
                self.status = "Neither Lera nor telnet is installed; install Lera to play MUDs".into();
            }
            Err(e) => {
                self.modal = Some(Modal::Message(format!("Could not start {}.\n\n{e}", t.title)));
            }
        }
        log::info!("{}: {}", t.id, self.status);
    }

    fn on_modal_key(&mut self, m: Modal, k: KeyEvent) {
        match m {
            Modal::Choice { title, text, options, mut sel } => match k.code {
                KeyCode::Left | KeyCode::Up | KeyCode::BackTab => {
                    sel = sel.saturating_sub(1);
                    self.modal = Some(Modal::Choice { title, text, options, sel });
                }
                KeyCode::Right | KeyCode::Down | KeyCode::Tab => {
                    sel = (sel + 1).min(options.len().saturating_sub(1));
                    self.modal = Some(Modal::Choice { title, text, options, sel });
                }
                KeyCode::Enter => {
                    if let Some((_, then)) = options.get(sel).cloned() {
                        self.choose(then);
                    }
                }
                KeyCode::Esc => {}
                _ => self.modal = Some(Modal::Choice { title, text, options, sel }),
            },
            Modal::Quit => match k.code {
                KeyCode::Enter | KeyCode::Char('y') => self.quit = true,
                _ => {}
            },
            // W in help opens the welcome screen again; any other key closes it
            Modal::Help if matches!(k.code, KeyCode::Char('w' | 'W')) => self.open(Modal::Welcome),
            // a note taller than the screen scrolls; any other key closes it
            m @ (Modal::Message(_) | Modal::Help | Modal::Welcome | Modal::AvUpdate)
                if matches!(k.code, KeyCode::Up | KeyCode::Down | KeyCode::PageUp | KeyCode::PageDown | KeyCode::Home | KeyCode::End) =>
            {
                let max = self.modal_scroll_max.get();
                self.modal_scroll = match k.code {
                    KeyCode::Up => self.modal_scroll.saturating_sub(1),
                    KeyCode::Down => self.modal_scroll + 1,
                    KeyCode::PageUp => self.modal_scroll.saturating_sub(10),
                    KeyCode::PageDown => self.modal_scroll + 10,
                    KeyCode::Home => 0,
                    _ => max,
                }
                .min(max);
                self.modal = Some(m);
            }
            Modal::Message(_) | Modal::Help | Modal::Welcome | Modal::Size | Modal::AvUpdate => {}
            Modal::Downloads => self.downloads_key(k),
            Modal::Uninstall { id } => match k.code {
                KeyCode::Left | KeyCode::Right | KeyCode::Up | KeyCode::Down => {
                    self.uninstall_keep_data = !self.uninstall_keep_data;
                    self.modal = Some(Modal::Uninstall { id });
                }
                KeyCode::Enter => {
                    let Some(t) = self.titles.iter().find(|t| t.id == id).cloned() else { return };
                    let dest = self.paths.installed_dir(&t, self.state.installed.get(&t.id));
                    let keep = self.uninstall_keep_data;
                    match install::uninstall(&dest, self.paths.bin.as_deref(), &t.id, &self.paths.saves(&t.id), keep) {
                        Ok(()) => {
                            self.state.installed.remove(&id);
                            self.save();
                            self.status = if keep {
                                format!("uninstalled {id}; its saves and settings were kept for a reinstall")
                            } else {
                                // titles may also keep data in their own config folders;
                                // the manifest does not say where, so av leaves those
                                format!("uninstalled {id} with the saves it kept in its folder; anything it keeps elsewhere is untouched")
                            };
                            self.action = 0;
                        }
                        Err(e) => self.modal = Some(Modal::Message(format!("Uninstall failed.\n\n{e:#}"))),
                    }
                }
                KeyCode::Esc => {}
                _ => self.modal = Some(Modal::Uninstall { id }),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::Installed;

    #[test]
    fn show_filters_by_kind_and_by_mine() {
        let dir = std::env::temp_dir().join(format!("av-show-{}", std::process::id()));
        let mut state = State::load(&dir);
        let title = |id: &str, kind: &str, version: &str| Title { id: id.into(), kind: kind.into(), version: version.into(), ..Default::default() };
        let (old, cur, mud, wish) = (title("old", "game", "2"), title("cur", "game", "1"), title("mud", "mud", ""), title("wish", "game", "1"));
        for (id, v) in [("old", "1"), ("cur", "1")] {
            let i = Installed { id: id.into(), version: v.into(), sha256: String::new(), installed_at: String::new(), kind: "game".into(), target: String::new(), exec: String::new(), args: Vec::new() };
            state.installed.insert(id.into(), i);
        }
        state.wishlist.insert("wish".into());
        let passes = |s: &Show| [&old, &cur, &mud, &wish].iter().filter(|t| s.lets_through(t, &state)).map(|t| t.id.as_str()).collect::<Vec<_>>();

        assert_eq!(passes(&Show::default()), ["old", "cur", "mud", "wish"]);
        let no_games = Show { hidden: ["game".to_string()].into(), ..Default::default() };
        assert_eq!(passes(&no_games), ["mud"]);
        assert_eq!(passes(&Show { updates: true, ..Default::default() }), ["old"]);
        assert_eq!(passes(&Show { installed: true, ..Default::default() }), ["old", "cur"]);
        // several "only mine" boxes: a title passing any of them
        assert_eq!(passes(&Show { updates: true, wishlist: true, ..Default::default() }), ["old", "wish"]);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn offline_installed_is_not_saved() {
        let dir = std::env::temp_dir().join(format!("av-offline-{}", std::process::id()));
        let paths = Paths { install: dir.join("data"), state: dir.join("data"), cache: dir.join("cache"), config: dir.join("config"), bin: None };
        std::fs::create_dir_all(&paths.cache).unwrap();
        let catalog = r#"{"schema_version":1,"titles":{"a":{"id":"a","title":"A","kind":"game"}}}"#;
        let url = "http://127.0.0.1:9/catalog.json";
        let file = catalog::cache_file(&paths.cache, url);
        std::fs::write(&file, catalog).unwrap();
        // older than the cache's hour, so av tries the network first
        let old = std::time::SystemTime::now() - std::time::Duration::from_secs(2 * 3600);
        std::fs::File::options().write(true).open(&file).unwrap().set_modified(old).unwrap();
        let saved = |app: &App| Show::load(&app.paths.config).installed;

        // nothing listens on port 9, so the fetch fails and the cache is used
        let mut app = App::new(paths, url.into());
        while app.is_loading() {
            std::thread::sleep(std::time::Duration::from_millis(10));
            app.tick();
        }
        assert!(app.offline && app.show.installed);
        let all = app.show_rows().iter().position(|r| *r == ShowRow::All).unwrap();
        let kind = app.show_rows().iter().position(|r| matches!(r, ShowRow::Kind(_))).unwrap();
        app.toggle_show(kind);
        assert!(app.show.installed && !saved(&app), "the offline tick leaked into show.json");
        app.toggle_show(all);
        // the player's own choice is saved as usual
        let installed = app.show_rows().iter().position(|r| *r == ShowRow::Installed).unwrap();
        app.toggle_show(installed);
        app.toggle_show(installed);
        assert!(app.show.installed && saved(&app));
        let _ = std::fs::remove_dir_all(&dir);
    }
}
