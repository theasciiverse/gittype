//! The board as two bitboards, for any size up to 10 columns by 8 rows.
//!
//! Each column takes `h + 1` bits, bottom row first; the extra bit on top
//! of every column (the sentinel) is always empty, so lines can't run from
//! the top of one column into the bottom of the next. `cur` holds the
//! discs of the side to move, `mask` every disc.

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Variant {
    pub name: &'static str,
    pub w: usize,
    pub h: usize,
    /// how many in a row win
    pub n: usize,
}

pub const VARIANTS: [Variant; 4] = [
    Variant { name: "Classic", w: 7, h: 6, n: 4 },
    Variant { name: "Wide", w: 8, h: 7, n: 4 },
    Variant { name: "Grand", w: 9, h: 7, n: 4 },
    Variant { name: "Five", w: 10, h: 8, n: 5 },
];

impl Variant {
    pub fn label(&self) -> String {
        let n = match self.n {
            4 => "four",
            5 => "five",
            _ => "?",
        };
        format!("{} {}x{}, {n} in a row", self.name, self.w, self.h)
    }

    pub fn by_name(s: &str) -> Option<usize> {
        VARIANTS.iter().position(|v| v.name.eq_ignore_ascii_case(s))
    }

    /// Columns from the middle outwards: the usual best order to try.
    pub fn centre_order(&self) -> Vec<usize> {
        let mut cols: Vec<usize> = (0..self.w).collect();
        let mid = (self.w as f32 - 1.0) / 2.0;
        cols.sort_by(|a, b| {
            let da = (*a as f32 - mid).abs();
            let db = (*b as f32 - mid).abs();
            da.partial_cmp(&db).unwrap().then(a.cmp(b))
        });
        cols
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Board {
    pub v: Variant,
    pub cur: u128,
    pub mask: u128,
    pub moves: usize,
}

impl Board {
    pub fn new(v: Variant) -> Board {
        assert!(v.w <= 10 && v.h <= 8 && v.n >= 3);
        Board { v, cur: 0, mask: 0, moves: 0 }
    }

    pub fn stride(&self) -> usize {
        self.v.h + 1
    }

    pub fn bit(&self, col: usize, row: usize) -> u128 {
        1u128 << (col * self.stride() + row)
    }

    fn bottom(&self, col: usize) -> u128 {
        self.bit(col, 0)
    }

    fn top(&self, col: usize) -> u128 {
        self.bit(col, self.v.h - 1)
    }

    fn column(&self, col: usize) -> u128 {
        ((1u128 << self.v.h) - 1) << (col * self.stride())
    }

    /// 0 or 1: who moves now (0 started the game).
    pub fn to_move(&self) -> u8 {
        (self.moves % 2) as u8
    }

    pub fn full(&self) -> bool {
        self.moves == self.v.w * self.v.h
    }

    pub fn can_play(&self, col: usize) -> bool {
        col < self.v.w && self.mask & self.top(col) == 0
    }

    /// The row a disc dropped in `col` lands on.
    pub fn landing_row(&self, col: usize) -> usize {
        (self.mask & self.column(col)).count_ones() as usize
    }

    /// The bit a disc dropped in `col` would take.
    pub fn move_bit(&self, col: usize) -> u128 {
        (self.mask + self.bottom(col)) & self.column(col)
    }

    pub fn play(&mut self, col: usize) {
        debug_assert!(self.can_play(col));
        self.cur ^= self.mask;
        self.mask |= self.mask + self.bottom(col);
        self.moves += 1;
    }

    /// The discs of player `p` (0 or 1).
    pub fn discs(&self, p: u8) -> u128 {
        if p == self.to_move() {
            self.cur
        } else {
            self.cur ^ self.mask
        }
    }

    pub fn cell(&self, col: usize, row: usize) -> Option<u8> {
        let b = self.bit(col, row);
        if self.mask & b == 0 {
            None
        } else if self.discs(0) & b != 0 {
            Some(0)
        } else {
            Some(1)
        }
    }

    fn dirs(&self) -> [usize; 4] {
        let s = self.stride();
        [1, s, s - 1, s + 1]
    }

    /// The lowest bit of every run of `n` along each direction.
    fn runs(&self, stones: u128) -> [(usize, u128); 4] {
        let mut out = [(0, 0); 4];
        for (i, d) in self.dirs().into_iter().enumerate() {
            let mut m = stones;
            for _ in 1..self.v.n {
                m &= m >> d;
            }
            out[i] = (d, m);
        }
        out
    }

    pub fn has_line(&self, stones: u128) -> bool {
        self.runs(stones).iter().any(|&(_, m)| m != 0)
    }

    /// Would the side to move win by dropping in `col`?
    pub fn wins_with(&self, col: usize) -> bool {
        self.can_play(col) && self.has_line(self.cur | self.move_bit(col))
    }

    /// The player who has a line, if any.
    pub fn winner(&self) -> Option<u8> {
        (0..2).find(|&p| self.has_line(self.discs(p)))
    }

    /// Every cell in a winning line of player `p`, as (col, row).
    pub fn winning_cells(&self, p: u8) -> Vec<(usize, usize)> {
        let s = self.stride();
        let mut cells = Vec::new();
        for (d, m) in self.runs(self.discs(p)) {
            let mut m = m;
            while m != 0 {
                let b = m.trailing_zeros() as usize;
                m &= m - 1;
                for k in 0..self.v.n {
                    let i = b + k * d;
                    let c = (i / s, i % s);
                    if !cells.contains(&c) {
                        cells.push(c);
                    }
                }
            }
        }
        cells
    }

    /// Every place a line of `n` can lie, as a bit mask.
    pub fn windows(&self) -> Vec<u128> {
        let (w, h, n) = (self.v.w as i32, self.v.h as i32, self.v.n as i32);
        let mut out = Vec::new();
        for c in 0..w {
            for r in 0..h {
                for (dc, dr) in [(1, 0), (0, 1), (1, 1), (1, -1)] {
                    let (ec, er) = (c + dc * (n - 1), r + dr * (n - 1));
                    if ec < 0 || ec >= w || er < 0 || er >= h {
                        continue;
                    }
                    let mut m = 0u128;
                    for k in 0..n {
                        m |= self.bit((c + dc * k) as usize, (r + dr * k) as usize);
                    }
                    out.push(m);
                }
            }
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rng::Rng;

    fn board_with(v: Variant, moves: &[usize]) -> Board {
        let mut b = Board::new(v);
        for &c in moves {
            assert!(b.can_play(c), "column {c} full");
            b.play(c);
        }
        b
    }

    /// The plain way: look at every cell and walk each direction.
    fn naive_winner(b: &Board) -> Option<u8> {
        let (w, h, n) = (b.v.w as i32, b.v.h as i32, b.v.n as i32);
        for p in 0..2u8 {
            for c in 0..w {
                for r in 0..h {
                    for (dc, dr) in [(1, 0), (0, 1), (1, 1), (1, -1)] {
                        let ok = (0..n).all(|k| {
                            let (cc, rr) = (c + dc * k, r + dr * k);
                            cc >= 0 && cc < w && rr >= 0 && rr < h && b.cell(cc as usize, rr as usize) == Some(p)
                        });
                        if ok {
                            return Some(p);
                        }
                    }
                }
            }
        }
        None
    }

    #[test]
    fn drops_stack_and_fill() {
        let v = VARIANTS[0];
        let mut b = Board::new(v);
        for i in 0..v.h {
            assert_eq!(b.landing_row(3), i);
            b.play(3);
        }
        assert!(!b.can_play(3));
        assert_eq!(b.cell(3, 0), Some(0));
        assert_eq!(b.cell(3, 1), Some(1));
        assert_eq!(b.cell(3, 5), Some(1));
    }

    #[test]
    fn every_window_is_a_line_in_every_variant() {
        for v in VARIANTS {
            let b = Board::new(v);
            let windows = b.windows();
            assert!(!windows.is_empty());
            for m in windows {
                assert!(b.has_line(m), "{v:?} window {m:#x}");
                // one short of a line never wins
                let low = m & (m - 1);
                assert!(!b.has_line(low), "{v:?} short window {low:#x}");
            }
        }
    }

    #[test]
    fn lines_at_the_edges() {
        let v = VARIANTS[0]; // 7x6
        // bottom row, right edge: 0 plays 3..6, 1 plays on top
        let b = board_with(v, &[3, 3, 4, 4, 5, 5, 6]);
        assert_eq!(b.winner(), Some(0));
        assert_eq!(b.winning_cells(0), vec![(3, 0), (4, 0), (5, 0), (6, 0)]);
        // left edge, vertical, reaching the top row
        let b = board_with(v, &[0, 0, 1, 0, 1, 0, 2, 0, 1, 0]);
        // column 0: 0,1,1,1,1,1 -> player 1 has rows 1..5 (five)
        assert_eq!(b.winner(), Some(1));
        assert_eq!(b.winner(), naive_winner(&b));
        // rising diagonal from the bottom-left corner
        let b = board_with(v, &[0, 1, 1, 2, 2, 3, 2, 3, 3, 6, 3]);
        assert_eq!(b.winner(), Some(0));
        let mut cells = b.winning_cells(0);
        cells.sort();
        assert_eq!(cells, vec![(0, 0), (1, 1), (2, 2), (3, 3)]);
        // falling diagonal into the bottom-right corner
        let b = board_with(v, &[6, 5, 5, 4, 4, 3, 4, 3, 3, 0, 3]);
        assert_eq!(b.winner(), Some(0));
        let mut cells = b.winning_cells(0);
        cells.sort();
        assert_eq!(cells, vec![(3, 3), (4, 2), (5, 1), (6, 0)]);
    }

    #[test]
    fn no_line_across_the_column_seam() {
        for v in VARIANTS {
            let b = Board::new(v);
            let h = v.h;
            // the top of column 0 and the bottom of column 1: neighbours
            // in memory but for the sentinel, never a vertical line
            let mut m = 0;
            for r in h - (v.n - 1)..h {
                m |= b.bit(0, r);
            }
            for r in 0..v.n {
                m |= b.bit(1, r) * if r == 0 { 1 } else { 0 };
            }
            assert!(!b.has_line(m), "{v:?}");
            // a falling diagonal can't wrap from row 0 into the next column's top
            let mut m = b.bit(0, 0);
            for k in 1..v.n {
                m |= b.bit(k, h - k);
            }
            assert!(!b.has_line(m), "{v:?}");
            // the last column's right edge is the end of the board
            let mut m = 0;
            for k in 0..v.n - 1 {
                m |= b.bit(v.w - 1 - k, 0);
            }
            assert!(!b.has_line(m), "{v:?}");
        }
    }

    #[test]
    fn matches_the_plain_scan_on_random_games() {
        let mut rng = Rng::new(7);
        for v in VARIANTS {
            for _ in 0..400 {
                let mut b = Board::new(v);
                loop {
                    let cols: Vec<usize> = (0..v.w).filter(|&c| b.can_play(c)).collect();
                    if cols.is_empty() {
                        break;
                    }
                    let c = cols[(rng.next() % cols.len() as u64) as usize];
                    let mover = b.to_move();
                    let predicted = b.wins_with(c);
                    b.play(c);
                    let w = b.winner();
                    assert_eq!(w, naive_winner(&b), "{v:?}");
                    assert_eq!(predicted, w == Some(mover), "{v:?}");
                    if w.is_some() {
                        assert!(b.winning_cells(mover).len() >= v.n);
                        break;
                    }
                }
            }
        }
    }
}
