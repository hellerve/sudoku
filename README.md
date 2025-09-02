# sudoku

in which I build a series of Sudoku solvers, accompanied by blog posts, written
in various programming languages. All of them implement simple backtracking, but
each of them works a bit differently, leveraging the individual strengths of
the language.

## The solvers

- `sudoku.py`: written in Python. Reference implementation. Using MRV and DFS.
- `sudoku.lisp`: written in SBCL. Leveraging macros, bitsets, and type and
  optimization declarations. **PLANNED**
- `sudoku.pro`: written in SWI Prolog. Declarative, leverages Prolog for core
  algorithmn. **PLANNED**
- `sudoku.hs`: written in Haskell. Leverages types, immutability, and searching
  via `Maybe` and `Alternative`. **PLANNED**
- `sudoku.rs`: written in Rust. Trying to get it as raw and low-level as it gets
  and minimize allocations. **PLANNED**
- `sudoku.ex`: written in Elixir. Utilizes the Beam and concurrency on first
  split. **PLANNED**

Each of them is accompanied by a blog post, so check them out if you want more
information.

<hr/>

Have fun!
