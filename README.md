# sudoku

in which I build a series of Sudoku solvers, accompanied by blog posts, written
in various programming languages. All of them implement simple backtracking, but
each of them works a bit differently, leveraging the individual strengths of
the language.

## The solvers

- [`sudoku.py`: written in Python. Reference implementation. Using MRV and DFS.](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_I:_Python_%28Reference%29.html)
- [`sudoku.lisp`: written in SBCL. Leveraging macros, bitsets, and type and
  optimization declarations.](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_II:_Common_Lisp.html)
- [`sudoku.pro`: written in SWI Prolog. Declarative, leverages Prolog for core
  algorithmn.](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_III:_Prolog.html)
- [`sudoku.hs`: written in Haskell. Leverages types and state using monads.](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_IV:_Haskell.html)
- [`sudoku.rs`: written in Rust. Trying to get it as raw and low-level as it gets
  and minimize allocations.](https://blog.veitheller.de/Six_Simple_Sudoku_Solvers_V:_Rust.html)
- `sudoku.ex`: written in Elixir. Utilizes the Beam and concurrency on first
  split. **PREPARED**

Each of them is accompanied by a blog post, so check them out if you want more
information.

<hr/>

Have fun!
