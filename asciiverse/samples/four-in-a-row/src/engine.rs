//! The computer: negamax with alpha-beta, a transposition table and
//! iterative deepening, on the bitboards in board.rs. Scores are from the
//! side to move: a win is WIN minus the moves it takes, so nearer wins
//! score higher and farther losses are preferred.

use crate::board::Board;
use crate::rng::Rng;
use std::time::{Duration, Instant};

pub const WIN: i32 = 1_000_000;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Level {
    Easy,
    Normal,
    Hard,
    Master,
}

pub const LEVELS: [Level; 4] = [Level::Easy, Level::Normal, Level::Hard, Level::Master];

impl Level {
    pub fn name(self) -> &'static str {
        match self {
            Level::Easy => "Easy",
            Level::Normal => "Normal",
            Level::Hard => "Hard",
            Level::Master => "Master",
        }
    }

    pub fn parse(s: &str) -> Option<Level> {
        LEVELS.into_iter().find(|l| l.name().eq_ignore_ascii_case(s))
    }

    /// (deepest search, time allowed)
    fn limits(self) -> (u32, Duration) {
        match self {
            Level::Easy => (2, Duration::from_millis(100)),
            Level::Normal => (5, Duration::from_millis(300)),
            Level::Hard => (8, Duration::from_millis(600)),
            Level::Master => (64, Duration::from_millis(2500)),
        }
    }
}

#[derive(Clone, Copy, Default)]
struct Entry {
    cur: u128,
    mask: u128,
    depth: u8,
    flag: u8, // 0 empty, 1 exact, 2 lower bound, 3 upper bound
    best: u8,
    score: i32,
}

const TT_BITS: usize = 18;

pub struct Engine {
    tt: Vec<Entry>,
    windows: Vec<u128>,
    order: Vec<usize>,
    deadline: Option<Instant>,
    stopped: bool,
    pub nodes: u64,
}

fn hash(cur: u128, mask: u128) -> usize {
    let x = (cur ^ mask.rotate_left(17)).wrapping_mul(0x9e37_79b9_7f4a_7c15_f39c_c060_5ced_c835);
    ((x >> 64) as u64 >> (64 - TT_BITS)) as usize
}

impl Engine {
    pub fn new(b: &Board) -> Engine {
        Engine {
            tt: vec![Entry::default(); 1 << TT_BITS],
            windows: b.windows(),
            order: b.v.centre_order(),
            deadline: None,
            stopped: false,
            nodes: 0,
        }
    }

    /// Lines nearly made, for the side to move, minus the other side's.
    fn eval(&self, b: &Board) -> i32 {
        let me = b.cur;
        let them = b.cur ^ b.mask;
        let n = b.v.n as u32;
        let mut s = 0i32;
        for &w in &self.windows {
            let a = (w & me).count_ones();
            let o = (w & them).count_ones();
            if o == 0 && a > 0 {
                s += weight(a, n);
            } else if a == 0 && o > 0 {
                s -= weight(o, n);
            }
        }
        // the middle columns take part in more lines
        s
    }

    fn timed_out(&mut self) -> bool {
        if self.stopped {
            return true;
        }
        if self.nodes & 2047 == 0 {
            if let Some(d) = self.deadline {
                if Instant::now() >= d {
                    self.stopped = true;
                }
            }
        }
        self.stopped
    }

    fn negamax(&mut self, b: &Board, depth: u32, mut alpha: i32, mut beta: i32) -> i32 {
        self.nodes += 1;
        if self.timed_out() {
            return 0;
        }
        let cells = b.v.w * b.v.h;
        if b.moves >= cells {
            return 0;
        }
        // win now if we can
        for c in 0..b.v.w {
            if b.wins_with(c) {
                return WIN - (b.moves as i32 + 1);
            }
        }
        if b.moves + 1 >= cells {
            return 0;
        }
        // where would they win next? we must go there, and two is lost
        let mut them = *b;
        them.cur ^= them.mask;
        let threats: Vec<usize> = (0..b.v.w).filter(|&c| them.wins_with(c)).collect();
        if threats.len() >= 2 {
            return -(WIN - (b.moves as i32 + 2));
        }
        if depth == 0 {
            return self.eval(b);
        }
        // the best we could still get is winning on our next-but-one move
        let max = WIN - (b.moves as i32 + 3);
        if beta > max {
            beta = max;
            if alpha >= beta {
                return beta;
            }
        }
        let alpha0 = alpha;
        let slot = hash(b.cur, b.mask);
        let e = self.tt[slot];
        let mut tt_best = None;
        if e.flag != 0 && e.cur == b.cur && e.mask == b.mask {
            tt_best = Some(e.best as usize);
            if e.depth as u32 >= depth {
                match e.flag {
                    1 => return e.score,
                    2 => alpha = alpha.max(e.score),
                    3 => beta = beta.min(e.score),
                    _ => {}
                }
                if alpha >= beta {
                    return e.score;
                }
            }
        }
        let mut cols: Vec<usize> = if let Some(&t) = threats.first() {
            vec![t]
        } else {
            self.order.iter().copied().filter(|&c| b.can_play(c)).collect()
        };
        if let Some(t) = tt_best {
            if let Some(i) = cols.iter().position(|&c| c == t) {
                let c = cols.remove(i);
                cols.insert(0, c);
            }
        }
        let mut best = i32::MIN + 1;
        let mut best_col = cols[0];
        for c in cols {
            let mut nb = *b;
            nb.play(c);
            let s = -self.negamax(&nb, depth - 1, -beta, -alpha);
            if self.stopped {
                return 0;
            }
            if s > best {
                best = s;
                best_col = c;
            }
            alpha = alpha.max(s);
            if alpha >= beta {
                break;
            }
        }
        let flag = if best <= alpha0 {
            3
        } else if best >= beta {
            2
        } else {
            1
        };
        let old = &self.tt[slot];
        if old.flag == 0 || old.depth as u32 <= depth || old.cur != b.cur || old.mask != b.mask {
            self.tt[slot] = Entry { cur: b.cur, mask: b.mask, depth: depth.min(255) as u8, flag, best: best_col as u8, score: best };
        }
        best
    }

    /// Scores for every playable column at a fixed depth (no clock), in
    /// the order given (`first` is tried first). With `exact` every score
    /// is exact; without, only the best and those tied with it are, and the
    /// rest are upper bounds, which is much quicker.
    pub fn root_scores_from(&mut self, b: &Board, depth: u32, first: Option<usize>, exact: bool) -> Vec<(usize, i32)> {
        let mut order = self.order.clone();
        if let Some(f) = first {
            if let Some(i) = order.iter().position(|&c| c == f) {
                order.remove(i);
                order.insert(0, f);
            }
        }
        let mut out = Vec::new();
        let mut best = -WIN;
        for c in order {
            if !b.can_play(c) {
                continue;
            }
            let s = if b.wins_with(c) {
                WIN - (b.moves as i32 + 1)
            } else {
                let mut nb = *b;
                nb.play(c);
                let alpha = if exact { -WIN } else { (best - 1).max(-WIN) };
                -self.negamax(&nb, depth.saturating_sub(1), -WIN, -alpha)
            };
            if self.stopped {
                return Vec::new();
            }
            best = best.max(s);
            out.push((c, s));
        }
        out
    }

    #[cfg(test)]
    pub fn root_scores(&mut self, b: &Board, depth: u32) -> Vec<(usize, i32)> {
        self.root_scores_from(b, depth, None, true)
    }

    /// The column the computer plays, and its score.
    pub fn choose(&mut self, b: &Board, level: Level, rng: &mut Rng) -> (usize, i32) {
        let (max_depth, time) = level.limits();
        self.deadline = Some(Instant::now() + time);
        self.stopped = false;
        self.nodes = 0;
        let mut scores: Vec<(usize, i32)> = Vec::new();
        let left = (b.v.w * b.v.h - b.moves) as u32;
        let mut reached = 0;
        for depth in 1..=max_depth.min(left) {
            let first = scores.iter().max_by_key(|&&(_, v)| v).map(|&(c, _)| c);
            let s = self.root_scores_from(b, depth, first, level == Level::Easy);
            if s.is_empty() {
                break;
            }
            scores = s;
            reached = depth;
            // a forced result is known: deeper changes nothing
            if scores.iter().all(|&(_, v)| v.abs() > WIN / 2) || scores.iter().any(|&(_, v)| v > WIN / 2) {
                break;
            }
        }
        if scores.is_empty() {
            // out of time before depth 1 finished: any playable column
            let c = self.order.iter().copied().find(|&c| b.can_play(c)).unwrap_or(0);
            return (c, 0);
        }
        if level == Level::Easy {
            // Easy misjudges quiet positions, but still sees a win in one
            // and a loss in one
            for (_, s) in scores.iter_mut() {
                if s.abs() < WIN / 2 {
                    *s += (rng.normal() * 40.0) as i32;
                }
            }
        }
        let best = scores.iter().map(|&(_, s)| s).max().unwrap();
        let ties: Vec<usize> = scores.iter().filter(|&&(_, s)| s == best).map(|&(c, _)| c).collect();
        let c = ties[(rng.next() % ties.len() as u64) as usize];
        log::debug!("engine {level:?} depth {reached} nodes {} scores {scores:?} -> {c}", self.nodes);
        (c, best)
    }
}

fn weight(k: u32, n: u32) -> i32 {
    // one short of a line matters most
    match n - k.min(n) {
        0 => 1000,
        1 => 50,
        2 => 8,
        3 => 2,
        _ => 1,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board::VARIANTS;

    fn board_with(v: usize, moves: &[usize]) -> Board {
        let mut b = Board::new(VARIANTS[v]);
        for &c in moves {
            b.play(c);
        }
        b
    }

    #[test]
    fn takes_a_win_in_one() {
        // player 0 has three on the bottom row at 1, 2, 3
        let b = board_with(0, &[1, 1, 2, 2, 3, 3]);
        for level in LEVELS {
            let (c, s) = Engine::new(&b).choose(&b, level, &mut Rng::new(3));
            assert!(c == 0 || c == 4, "{level:?} played {c}");
            assert!(s > WIN / 2);
        }
    }

    #[test]
    fn blocks_a_win_in_one() {
        // player 0 has 0,1,2 on the bottom; player 1 to move must take 3
        let b = board_with(0, &[0, 6, 1, 6, 2]);
        for level in LEVELS {
            let (c, _) = Engine::new(&b).choose(&b, level, &mut Rng::new(5));
            assert_eq!(c, 3, "{level:?}");
        }
    }

    #[test]
    fn finds_a_forced_win_by_double_threat() {
        // 0 has the bottom of 2 and 3, 1 sits on top of them; 0 to move
        // makes three in a row with both ends open and wins
        let b = board_with(0, &[3, 3, 2, 2]);
        let mut e = Engine::new(&b);
        let scores = e.root_scores(&b, 5);
        let best = scores.iter().map(|&(_, s)| s).max().unwrap();
        assert!(best > WIN / 2, "{scores:?}");
        for (c, s) in scores {
            if s == best {
                assert!(c == 1 || c == 4, "{c}");
            }
        }
        let (c, _) = Engine::new(&b).choose(&b, Level::Normal, &mut Rng::new(1));
        assert!(c == 1 || c == 4);
    }

    #[test]
    fn takes_the_quickest_forced_win() {
        // 0: bottom 1 and 2; 1 high on the edge. 0 plays 3 for an open
        // three and wins on its next move whatever 1 does
        let b = board_with(0, &[1, 6, 2, 6]);
        let mut e = Engine::new(&b);
        let scores = e.root_scores(&b, 7);
        let best = scores.iter().map(|&(_, s)| s).max().unwrap();
        assert_eq!(best, WIN - (b.moves as i32 + 3), "{scores:?}");
        let (c, _) = Engine::new(&b).choose(&b, Level::Hard, &mut Rng::new(4));
        assert_eq!(c, 3);
    }

    #[test]
    fn avoids_a_loss_in_two() {
        // 1 has the bottom of 3 and 4 with room both sides: 0 must spoil
        // the open three now (1, 2 or 5), or lose
        let b = board_with(0, &[6, 3, 6, 4]);
        let scores = Engine::new(&b).root_scores(&b, 6);
        let s0 = scores.iter().find(|&&(c, _)| c == 0).unwrap().1;
        assert!(s0 < -WIN / 2, "{scores:?}");
        for level in [Level::Normal, Level::Hard, Level::Master] {
            let (c, s) = Engine::new(&b).choose(&b, level, &mut Rng::new(2));
            assert!(s > -WIN / 2 && [1, 2, 5].contains(&c), "{level:?} {c} {s}");
        }
    }

    #[test]
    fn finds_wins_on_every_variant() {
        for (i, v) in VARIANTS.iter().enumerate() {
            // player 0 lays n-1 along the bottom from column 1, player 1
            // stacks on the far right
            let mut moves = Vec::new();
            for k in 0..v.n - 1 {
                moves.push(1 + k);
                moves.push(v.w - 1);
            }
            // the far right column might fill up on tall boards: fine for these sizes
            let b = board_with(i, &moves);
            let (c, s) = Engine::new(&b).choose(&b, Level::Hard, &mut Rng::new(9));
            assert!(c == 0 || c == v.n, "{} played {c}", v.name);
            assert!(s > WIN / 2);
        }
    }

    #[test]
    fn plays_whole_games_to_the_end() {
        let mut rng = Rng::new(11);
        for v in 0..VARIANTS.len() {
            let mut b = Board::new(VARIANTS[v]);
            let mut e = Engine::new(&b);
            while b.winner().is_none() && !b.full() {
                let (c, _) = e.choose(&b, Level::Easy, &mut rng);
                assert!(b.can_play(c));
                b.play(c);
            }
        }
    }
}

#[cfg(test)]
mod ladder {
    use super::*;
    use crate::board::VARIANTS;

    /// Each level against the one below, both colours: `cargo test --release ladder -- --ignored --nocapture`
    #[test]
    #[ignore]
    fn each_level_beats_the_one_below() {
        let mut rng = Rng::new(21);
        for pair in LEVELS.windows(2).skip(std::env::var("LADDER_FROM").ok().and_then(|v| v.parse().ok()).unwrap_or(0)) {
            let (lo, hi) = (pair[0], pair[1]);
            let mut tally = [0; 3]; // hi wins, lo wins, draws
            for g in 0..12 {
                let mut b = Board::new(VARIANTS[0]);
                let hi_first = g % 2 == 0;
                while b.winner().is_none() && !b.full() {
                    let hi_turn = (b.moves % 2 == 0) == hi_first;
                    let lvl = if hi_turn { hi } else { lo };
                    let (c, _) = Engine::new(&b).choose(&b, lvl, &mut rng);
                    b.play(c);
                }
                match b.winner() {
                    Some(p) => {
                        let hi_won = (p == 0) == hi_first;
                        tally[if hi_won { 0 } else { 1 }] += 1;
                    }
                    None => tally[2] += 1,
                }
            }
            println!("{hi:?} vs {lo:?}: won {} lost {} drawn {}", tally[0], tally[1], tally[2]);
        }
    }
}
