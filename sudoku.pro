% swipl -q -s sudoku.pro  
:- use_module(library(clpfd)).

solve(Rows) :-
    length(Rows, 9),
    maplist(length_(9), Rows),
    append(Rows, Vars), Vars ins 1..9,

    maplist(all_different, Rows),
    transpose(Rows, Cols),
    maplist(all_different, Cols),
    blocks(Rows),

    labeling([ff], Vars).

blocks([]).
blocks([A,B,C|Rs]) :- blocks3(A,B,C), blocks(Rs).

blocks3([], [], []).
blocks3([A,B,C|R1], [D,E,F|R2], [G,H,I|R3]) :-
    all_different([A,B,C,D,E,F,G,H,I]),
    blocks3(R1, R2, R3).

length_(L, Xs) :- length(Xs, L).

z2v(0, _).
z2v(N, N) :- integer(N), N > 0.

rows_from_zeros(PZero, Rows) :- maplist(maplist(z2v), PZero, Rows).

puzzle_zeroes([
  [3,0,6,5,0,8,4,0,0],
  [5,2,0,0,0,0,0,0,0],
  [0,8,7,0,0,0,0,3,1],
  [0,0,3,0,1,0,0,8,0],
  [9,0,0,8,6,3,0,0,5],
  [0,5,0,0,9,0,6,0,0],
  [1,3,0,0,0,0,2,5,0],
  [0,0,0,0,0,0,0,7,4],
  [0,0,5,2,0,6,3,0,0]
]).

print_board(Rows) :-
    forall(nth1(_, Rows, Row), print_row(Row)).

print_row(Row) :-
    forall(nth1(J, Row, V), (
        write(V), (J =:= 9 -> nl ; write(' '))
      )).

main :-
    puzzle_zeroes(P0),
    rows_from_zeros(P0, Rows),
    (   solve(Rows)
    ->  print_board(Rows), halt(0)
    ;   writeln(unsatisfiable), halt(1)
    ).

:- initialization(main).
