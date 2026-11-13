//! Game flow: the title menu, a match, the falling disc, the computer
//! thinking in its own thread, undo and rematch.
//!
//! Colours are fixed: 0 is Red, 1 is Yellow. In a game against the
//! computer you are always Red. Who moves first alternates from game to
//! game; on the board, player 0 is whoever started, so a disc of board
//! player p has colour `p ^ starter`.

use crate::audio::{Audio, Sound};
use crate::board::{Board, Variant, VARIANTS};
use crate::engine::{Engine, Level, LEVELS};
use crate::rng::Rng;
use crate::store::Store;
use std::sync::mpsc::{self, Receiver};
use std::time::{Duration, Instant};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mode {
    /// you (Red) against the computer
    Solo(Level),
    /// two people taking turns at one keyboard (or two pads)
    Duo,
    /// the computer against itself
    Watch(Level),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Screen {
    Title,
    Play,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Outcome {
    /// the winning colour and the cells of its line(s), as (col, row)
    Win(u8, Vec<(usize, usize)>),
    Draw,
}

/// A disc on its way down.
#[derive(Clone, Copy, Debug)]
pub struct Fall {
    pub col: usize,
    pub row: usize,
    pub colour: u8,
    /// rows down from the hover line (0) to the landing cell (h - row)
    pub y: f32,
    pub vy: f32,
}

pub struct Think {
    rx: Receiver<usize>,
    started: Instant,
}

pub struct Match {
    pub mode: Mode,
    pub board: Board,
    pub starter: u8,
    pub history: Vec<usize>,
    pub cursor: usize,
    pub fall: Option<Fall>,
    pub over: Option<Outcome>,
    /// seconds since the game ended
    pub over_for: f32,
    pub think: Option<Think>,
    /// Red wins, Yellow wins, draws, this session
    pub score: [u32; 3],
}

impl Match {
    pub fn new(mode: Mode, v: Variant, starter: u8) -> Match {
        Match {
            mode,
            board: Board::new(v),
            starter,
            history: Vec::new(),
            cursor: v.w / 2,
            fall: None,
            over: None,
            over_for: 0.0,
            think: None,
            score: [0; 3],
        }
    }

    /// The colour to move now.
    pub fn to_move(&self) -> u8 {
        self.board.to_move() ^ self.starter
    }

    /// The colour of the disc at (col, row).
    pub fn colour_at(&self, col: usize, row: usize) -> Option<u8> {
        self.board.cell(col, row).map(|p| p ^ self.starter)
    }

    pub fn is_computer(&self, colour: u8) -> bool {
        match self.mode {
            Mode::Solo(_) => colour == 1,
            Mode::Duo => false,
            Mode::Watch(_) => true,
        }
    }

    pub fn level(&self) -> Option<Level> {
        match self.mode {
            Mode::Solo(l) | Mode::Watch(l) => Some(l),
            Mode::Duo => None,
        }
    }

    /// A person may drop a disc now.
    pub fn human_turn(&self) -> bool {
        self.over.is_none() && self.fall.is_none() && !self.is_computer(self.to_move())
    }

    pub fn thinking(&self) -> bool {
        self.think.is_some()
    }

    fn rebuild(&mut self) {
        let mut b = Board::new(self.board.v);
        for &c in &self.history {
            b.play(c);
        }
        self.board = b;
    }
}

/// Fall speed, in rows per second per second.
const GRAVITY: f32 = 90.0;
/// The computer seems to think at least this long, so moves can be followed.
const MIN_THINK: Duration = Duration::from_millis(450);
/// In a demo, the next game starts this long after one ends.
const DEMO_NEXT: f32 = 3.0;

pub const MENU: [&str; 6] = ["Play the computer", "Two players", "Watch the computer", "Computer", "Board", "Quit"];
pub const MENU_LEVEL: usize = 3;
pub const MENU_BOARD: usize = 4;

pub struct App {
    pub screen: Screen,
    pub menu: usize,
    pub level: Level,
    pub variant: usize,
    pub play: Option<Match>,
    pub audio: Audio,
    pub store: Store,
    pub rng: Rng,
    pub quit: bool,
    /// a demo rematches by itself
    pub demo: bool,
    /// seconds since start, for pulsing things
    pub t: f32,
}

impl App {
    pub fn new(audio: Audio, store: Store, seed: u64) -> App {
        let level = store.level.as_deref().and_then(Level::parse).unwrap_or(Level::Normal);
        let variant = store.board.as_deref().and_then(Variant::by_name).unwrap_or(0);
        App { screen: Screen::Title, menu: 0, level, variant, play: None, audio, store, rng: Rng::new(seed), quit: false, demo: false, t: 0.0 }
    }

    pub fn variant(&self) -> Variant {
        VARIANTS[self.variant]
    }

    pub fn sound(&self, s: Sound) {
        self.audio.play(s);
    }

    pub fn start(&mut self, mode: Mode) {
        // the person against the computer goes first in the first game
        let m = Match::new(mode, self.variant(), 0);
        log::info!("start {mode:?} board {:?}", m.board.v);
        self.play = Some(m);
        self.screen = Screen::Play;
    }

    /// The next game of the same match: the other colour starts.
    pub fn rematch(&mut self) {
        let Some(m) = self.play.as_mut() else { return };
        let starter = m.starter ^ 1;
        let mut next = Match::new(m.mode, m.board.v, starter);
        next.score = m.score;
        next.cursor = m.cursor.min(next.board.v.w - 1);
        log::info!("rematch {:?}, colour {starter} starts", next.mode);
        self.play = Some(next);
    }

    pub fn to_title(&mut self) {
        if self.screen == Screen::Play {
            log::info!("back to the title");
        }
        self.play = None;
        self.screen = Screen::Title;
    }

    pub fn menu_move(&mut self, d: i32) {
        self.menu = (self.menu as i32 + d).rem_euclid(MENU.len() as i32) as usize;
        self.sound(Sound::Menu);
    }

    /// Left and right on the Computer and Board lines change them.
    pub fn menu_side(&mut self, d: i32) {
        match self.menu {
            MENU_LEVEL | 0 | 2 => {
                let i = LEVELS.iter().position(|&l| l == self.level).unwrap_or(1) as i32;
                self.level = LEVELS[(i + d).rem_euclid(LEVELS.len() as i32) as usize];
                self.store.level = Some(self.level.name().to_string());
            }
            MENU_BOARD => {
                self.variant = (self.variant as i32 + d).rem_euclid(VARIANTS.len() as i32) as usize;
                self.store.board = Some(self.variant().name.to_string());
            }
            _ => return,
        }
        self.store.save();
        self.sound(Sound::Menu);
    }

    pub fn menu_pick(&mut self) {
        log::info!("menu pick {}", MENU[self.menu]);
        match self.menu {
            0 => self.start(Mode::Solo(self.level)),
            1 => self.start(Mode::Duo),
            2 => self.start(Mode::Watch(self.level)),
            MENU_LEVEL | MENU_BOARD => self.menu_side(1),
            _ => self.quit = true,
        }
    }

    pub fn cursor_move(&mut self, d: i32) {
        let Some(m) = self.play.as_mut() else { return };
        if !m.human_turn() {
            return;
        }
        let w = m.board.v.w as i32;
        let c = (m.cursor as i32 + d).clamp(0, w - 1) as usize;
        if c != m.cursor {
            m.cursor = c;
            self.audio.play(Sound::Step);
        }
    }

    pub fn cursor_to(&mut self, col: usize) {
        let Some(m) = self.play.as_mut() else { return };
        if m.human_turn() && col < m.board.v.w && col != m.cursor {
            m.cursor = col;
            self.audio.play(Sound::Step);
        }
    }

    /// A person drops a disc in the cursor's column.
    pub fn drop_disc(&mut self) {
        let Some(m) = self.play.as_ref() else { return };
        if m.over.is_some() {
            if m.over_for > 0.4 {
                self.rematch();
            }
            return;
        }
        if !m.human_turn() {
            return;
        }
        let col = m.cursor;
        self.begin_fall(col);
    }

    fn begin_fall(&mut self, col: usize) {
        let Some(m) = self.play.as_mut() else { return };
        if !m.board.can_play(col) {
            log::debug!("column {col} is full");
            self.audio.play(Sound::Full);
            return;
        }
        let row = m.board.landing_row(col);
        let colour = m.to_move();
        log::info!("move {} colour {colour} column {col} row {row}", m.history.len() + 1);
        m.cursor = col;
        m.fall = Some(Fall { col, row, colour, y: 0.0, vy: 4.0 });
    }

    /// Take back your last move (and the computer's answer to it).
    pub fn undo(&mut self) {
        let Some(m) = self.play.as_mut() else { return };
        if matches!(m.mode, Mode::Watch(_)) || m.fall.is_some() || m.history.is_empty() {
            return;
        }
        m.think = None;
        // a finished game can be taken back too; its result stays counted
        m.over = None;
        loop {
            let Some(c) = m.history.pop() else { break };
            m.rebuild();
            log::info!("undo column {c}");
            m.cursor = c;
            if !m.is_computer(m.to_move()) {
                break;
            }
        }
        m.over_for = 0.0;
        self.audio.play(Sound::Undo);
    }

    /// Advance animations and the computer by `dt` seconds.
    pub fn tick(&mut self, dt: f32) {
        self.t += dt;
        let demo = self.demo;
        let Some(m) = self.play.as_mut() else { return };
        if m.over.is_some() {
            m.over_for += dt;
            if demo && m.over_for > DEMO_NEXT {
                self.rematch();
            }
            return;
        }
        if let Some(f) = m.fall.as_mut() {
            let land = (m.board.v.h - f.row) as f32;
            f.vy += GRAVITY * dt;
            f.y += f.vy * dt;
            if f.y >= land {
                let f = *f;
                m.fall = None;
                m.board.play(f.col);
                m.history.push(f.col);
                self.audio.play(Sound::Drop(f.row));
                self.after_move();
            }
            return;
        }
        let colour = m.to_move();
        if !m.is_computer(colour) {
            return;
        }
        match &m.think {
            None => {
                let b = m.board;
                let level = m.level().unwrap_or(Level::Normal);
                let seed = self.rng.next();
                let (tx, rx) = mpsc::channel();
                std::thread::spawn(move || {
                    let mut e = Engine::new(&b);
                    let (c, s) = e.choose(&b, level, &mut Rng::new(seed));
                    log::debug!("computer {level:?} picks {c} score {s}");
                    let _ = tx.send(c);
                });
                m.think = Some(Think { rx, started: Instant::now() });
            }
            Some(t) => {
                if t.started.elapsed() < MIN_THINK {
                    return;
                }
                if let Ok(c) = t.rx.try_recv() {
                    m.think = None;
                    self.begin_fall(c);
                }
            }
        }
    }

    fn after_move(&mut self) {
        let Some(m) = self.play.as_mut() else { return };
        let mut outcome = None;
        if let Some(p) = m.board.winner() {
            let colour = p ^ m.starter;
            outcome = Some(Outcome::Win(colour, m.board.winning_cells(p)));
        } else if m.board.full() {
            outcome = Some(Outcome::Draw);
        }
        let Some(o) = outcome else { return };
        log::info!("game over: {o:?} after {} moves", m.history.len());
        let sound = match &o {
            Outcome::Win(c, _) => {
                m.score[*c as usize] += 1;
                if matches!(m.mode, Mode::Solo(_)) && *c == 1 {
                    Sound::Lose
                } else {
                    Sound::Win
                }
            }
            Outcome::Draw => {
                m.score[2] += 1;
                Sound::Draw
            }
        };
        if let Mode::Solo(level) = m.mode {
            let i = LEVELS.iter().position(|&l| l == level).unwrap_or(0);
            match &o {
                Outcome::Win(0, _) => self.store.record[i].0 += 1,
                Outcome::Win(_, _) => self.store.record[i].1 += 1,
                Outcome::Draw => {}
            }
            self.store.save();
        }
        m.over = Some(o);
        m.over_for = 0.0;
        self.audio.play(sound);
    }

    pub fn snapshot(&self) -> String {
        match &self.play {
            None => format!("title menu {} level {:?} board {}", MENU[self.menu], self.level, self.variant().name),
            Some(m) => format!(
                "play {:?} {} moves {:?} to_move {} cursor {} fall {:?} thinking {} over {:?} score {:?}",
                m.mode,
                m.board.v.name,
                m.history,
                m.to_move(),
                m.cursor,
                m.fall.map(|f| (f.col, f.y)),
                m.thinking(),
                m.over,
                m.score
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn app() -> App {
        App::new(Audio::silent(), Store::default(), 1)
    }

    fn settle(a: &mut App) {
        for _ in 0..400 {
            if a.play.as_ref().is_some_and(|m| m.fall.is_none() && m.think.is_none()) {
                break;
            }
            a.tick(1.0 / 60.0);
            if a.play.as_ref().is_some_and(|m| m.think.is_some()) {
                std::thread::sleep(Duration::from_millis(5));
            }
        }
    }

    #[test]
    fn two_players_take_turns_and_win() {
        let mut a = app();
        a.start(Mode::Duo);
        for c in [0, 1, 0, 1, 0, 1, 0] {
            a.play.as_mut().unwrap().cursor = c;
            a.drop_disc();
            settle(&mut a);
        }
        let m = a.play.as_ref().unwrap();
        match &m.over {
            Some(Outcome::Win(0, cells)) => assert_eq!(cells.len(), 4),
            o => panic!("{o:?}"),
        }
        assert_eq!(m.score, [1, 0, 0]);
    }

    #[test]
    fn rematch_swaps_who_starts() {
        let mut a = app();
        a.start(Mode::Duo);
        a.rematch();
        let m = a.play.as_ref().unwrap();
        assert_eq!(m.starter, 1);
        assert_eq!(m.to_move(), 1);
    }

    #[test]
    fn the_computer_answers_and_undo_takes_both_back() {
        let mut a = app();
        a.start(Mode::Solo(Level::Easy));
        a.drop_disc();
        settle(&mut a);
        // our disc fell, then the computer's
        settle(&mut a);
        for _ in 0..200 {
            if a.play.as_ref().unwrap().history.len() == 2 {
                break;
            }
            a.tick(1.0 / 60.0);
            std::thread::sleep(Duration::from_millis(5));
        }
        assert_eq!(a.play.as_ref().unwrap().history.len(), 2);
        a.undo();
        let m = a.play.as_ref().unwrap();
        assert!(m.history.is_empty());
        assert!(m.human_turn());
    }

    #[test]
    fn a_full_column_takes_nothing() {
        let mut a = app();
        a.start(Mode::Duo);
        let h = a.variant().h;
        for _ in 0..h {
            a.play.as_mut().unwrap().cursor = 3;
            a.drop_disc();
            settle(&mut a);
        }
        a.drop_disc();
        let m = a.play.as_ref().unwrap();
        assert!(m.fall.is_none());
        assert_eq!(m.history.len(), h);
    }
}
