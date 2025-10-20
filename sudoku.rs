// rustc -C opt-level=3 -C target-cpu=native sudoku.rs -o sudoku && ./sudoku
type Board = [[u8; 9]; 9];
type Mask = u16;

const ALL: Mask = 0x1FF;

#[derive(Clone, Copy)]
struct State {
    b: Board,
    row: [Mask; 9],
    col: [Mask; 9],
    bx:  [Mask; 9],
}

#[inline] fn bit(d: u8) -> Mask { 1 << (d - 1) }
#[inline] fn box_idx(i: usize, j: usize) -> usize { (i / 3) * 3 + (j / 3) }

fn init_state(b: Board) -> State {
    let mut s = State { b, row: [0; 9], col: [0; 9], bx: [0; 9] };
    for i in 0..9 {
        for j in 0..9 {
            let v = s.b[i][j];
            if v != 0 {
                let m = bit(v);
                s.row[i] |= m; s.col[j] |= m; s.bx[box_idx(i,j)] |= m;
            }
        }
    }
    s
}

#[inline]
fn cand_mask(s: &State, i: usize, j: usize) -> Mask {
    ALL ^ (s.row[i] | s.col[j] | s.bx[box_idx(i, j)])
}

#[inline]
fn assign(s: &mut State, i: usize, j: usize, v: u8) {
    debug_assert!(s.b[i][j] == 0);
    s.b[i][j] = v;
    let m = bit(v);
    s.row[i] |= m; s.col[j] |= m; s.bx[box_idx(i,j)] |= m;
}

fn propagate(s: &mut State) -> bool {
    loop {
        let mut changed = false;
        for i in 0..9 {
            for j in 0..9 {
                if s.b[i][j] == 0 {
                    let m = cand_mask(s, i, j);
                    if m == 0 { return false; }
                    if m.count_ones() == 1 {
                        let v = 1 + m.trailing_zeros() as u8;
                        assign(s, i, j, v);
                        changed = true;
                    }
                }
            }
        }
        if !changed { return true; }
    }
}

#[inline]
fn solved(s: &State) -> bool {
    s.b.iter().all(|r| r.iter().all(|&v| v != 0))
}

fn find_mrv(s: &State) -> Option<(usize, usize, Mask)> {
    let mut best: Option<(usize, usize, Mask, u32)> = None;
    for i in 0..9 {
        for j in 0..9 {
            if s.b[i][j] == 0 {
                let m = cand_mask(s, i, j);
                let k = m.count_ones();
                if k == 0 { return None; }
                if best.map_or(true, |(_,_,_,bk)| k < bk) {
                    best = Some((i, j, m, k));
                    if k == 1 { break; }
                }
            }
        }
    }
    best.map(|(i,j,m,_)| (i,j,m))
}

fn solve(mut s: State) -> Option<Board> {
    if !propagate(&mut s) { return None; }
    if solved(&s) { return Some(s.b); }
    let (i, j, mut m) = find_mrv(&s)?;
    while m != 0 {
        let lb = m & (!m + 1);
        let v = 1 + lb.trailing_zeros() as u8;
        let mut t = s;
        assign(&mut t, i, j, v);
        if let Some(sol) = solve(t) { return Some(sol); }
        m ^= lb;
    }
    None
}

fn pretty(b: &Board) {
    for i in 0..9 {
        if i > 0 && i % 3 == 0 { println!("{}", "-".repeat(21)); }
        for j in 0..9 {
            if j > 0 && j % 3 == 0 { print!("| "); }
            print!("{}", b[i][j]);
            print!("{}", if j == 8 { '\n' } else { ' ' });
        }
    }
}

fn main() {
    let puzzle: Board = [
        [3,0,6,5,0,8,4,0,0],
        [5,2,0,0,0,0,0,0,0],
        [0,8,7,0,0,0,0,3,1],
        [0,0,3,0,1,0,0,8,0],
        [9,0,0,8,6,3,0,0,5],
        [0,5,0,0,9,0,6,0,0],
        [1,3,0,0,0,0,2,5,0],
        [0,0,0,0,0,0,0,7,4],
        [0,0,5,2,0,6,3,0,0],
    ].map(|row| row.map(|x| x as u8));

    let s = init_state(puzzle);
    match solve(s) {
        Some(b) => pretty(&b),
        None => println!("unsatisfiable"),
    }
}
