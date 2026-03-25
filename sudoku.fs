\ gforth sudoku.fs

create board  81 allot

: board@  ( idx -- val )  board + c@ ;
: board!  ( val idx -- )  board + c! ;

: load-board  ( c-addr -- )
  81 0 do
    dup i + c@  [char] 0 -
    dup 0 < over 9 > or if drop 0 then
    i board!
  loop drop ;

: row-start  ( idx -- r )  9 /  9 * ;
: col-start  ( idx -- c )  9 mod ;
: box-start  ( idx -- b )  dup 27 / 27 *  swap 9 mod 3 / 3 * + ;


: digit>bit  ( d -- bit )  1-  1 swap lshift ;
511 constant full-mask

: row-used  ( idx -- mask )
  row-start  0
  9 0 do  over i + board@  dup 0> if digit>bit or else drop then  loop
  nip ;

: col-used  ( idx -- mask )
  col-start  0
  9 0 do  over i 9 * + board@  dup 0> if digit>bit or else drop then  loop
  nip ;

: box-used  ( idx -- mask )
  box-start  0
  3 0 do  3 0 do
    over j 9 * i + + board@  dup 0> if digit>bit or else drop then
  loop loop
  nip ;

: candidates  ( idx -- mask )
  dup row-used  over col-used or  over box-used or  nip  full-mask xor ;

: popcount  ( mask -- n )
  0 swap  9 0 do  dup 1 and  rot +  swap  1 rshift  loop  drop ;

: lowest-bit  ( mask -- pos )    \ 0-based position of lowest set bit
  0  begin  over 1 and 0=  while  1+  swap 1 rshift swap  repeat  nip ;

50 constant max-depth
create board-stack  max-depth 81 * allot
variable sp   0 sp !

: push-board  ( -- )
  board  board-stack sp @ 81 * +  81 move  1 sp +! ;
: pop-board   ( -- )
  -1 sp +!  board-stack sp @ 81 * +  board  81 move ;

: solved?  ( -- flag )
  true  81 0 do  i board@ 0= if drop false leave then  loop ;

: contradiction?  ( -- flag )
  false
  81 0 do
    i board@ 0= if  i candidates 0= if drop true leave then  then
  loop ;

: fill-singles  ( -- changed? )
  false
  81 0 do
    i board@ 0= if
      i candidates
      dup popcount 1 = if  lowest-bit 1+  i board!  drop true
                      else  drop  then
    then
  loop ;

: propagate  ( -- ok? )
  begin fill-singles 0= until  contradiction? invert ;

variable mrv-best

: find-mrv  ( -- idx )
  10 mrv-best !
  -1
  81 0 do
    i board@ 0= if
      i candidates popcount
      dup mrv-best @ < if
        mrv-best !  drop i
      else drop then
    then
  loop ;

: solve  ( -- solved? )
  propagate 0= if false exit then
  solved?       if true  exit then
  find-mrv dup 0< if drop false exit then
  dup candidates swap
  9 0 do
    over 1 and if
      push-board
      i 1+  over  board!
      recurse if  drop drop true  unloop exit  then
      pop-board
    then
    swap 1 rshift swap
  loop
  2drop false ;

: .digit  ( n -- )  dup 0= if drop [char] . else [char] 0 + then  emit ;
: .board  ( -- )
  9 0 do  9 0 do  j 9 * i + board@ .digit  loop  cr  loop ;

: solve-puzzle  ( c-addr -- )
  load-board  0 sp !
  solve if .board else ." No solution found." cr then ;

s" 530070000600195000098000060800060003400803001700020006060000280000419005000080079"
drop solve-puzzle

s" 800000000003600000070090200050007000000045700000100030001000068008500010090000400"
drop solve-puzzle

bye
