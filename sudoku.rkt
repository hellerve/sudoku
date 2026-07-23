#lang racket
;; racket sudoku.rkt

;; --- a µKanren core ---

(struct lvar (idx) #:transparent)

(define (walk t s)
  (if (lvar? t)
      (let ([b (hash-ref s t #f)])
        (if b (walk b s) t))
      t))

(define (unify u v s)
  (let ([u (walk u s)] [v (walk v s)])
    (cond
      [(equal? u v) s]
      [(lvar? u) (hash-set s u v)]
      [(lvar? v) (hash-set s v u)]
      [else #f])))

;; a state is a substitution and a fresh-variable counter
(define empty-state (cons (hash) 0))

(define mzero '())
(define (unit st) (cons st mzero))

(define ((== u v) st)
  (let ([s (unify u v (car st))])
    (if s (unit (cons s (cdr st))) mzero)))

(define ((call/fresh f) st)
  (let ([c (cdr st)])
    ((f (lvar c)) (cons (car st) (add1 c)))))

;; note: µKanren proper swaps the arguments in the second case
;; ((mplus $2 ($1))) to interleave streams fairly. our search tree is
;; finite and pruned by propagation, so we search depth-first instead;
;; see the blog post for what fairness does to a near-empty board.
(define (mplus $1 $2)
  (cond
    [(null? $1) $2]
    [(procedure? $1) (λ () (mplus ($1) $2))]
    [else (cons (car $1) (mplus (cdr $1) $2))]))

(define (bind $ g)
  (cond
    [(null? $) mzero]
    [(procedure? $) (λ () (bind ($) g))]
    [else (mplus (g (car $)) (bind (cdr $) g))]))

(define ((disj g1 g2) st) (mplus (g1 st) (g2 st)))
(define ((conj g1 g2) st) (bind (g1 st) g2))

(define (fail st) mzero)
(define (succeed st) (unit st))

;; --- the sugar ---

(define-syntax-rule (Zzz g) (λ (st) (λ () (g st))))

(define-syntax conj*
  (syntax-rules ()
    [(_) succeed]
    [(_ g) g]
    [(_ g gs ...) (conj g (conj* gs ...))]))

(define-syntax disj*
  (syntax-rules ()
    [(_) fail]
    [(_ g) g]
    [(_ g gs ...) (disj g (disj* gs ...))]))

(define-syntax fresh
  (syntax-rules ()
    [(_ () g ...) (conj* g ...)]
    [(_ (x xs ...) g ...)
     (call/fresh (λ (x) (fresh (xs ...) g ...)))]))

(define-syntax-rule (conde (g ...) ...)
  (disj* (conj* g ...) ...))

(define (conj/list gs) (foldr conj succeed gs))
(define (disj/list gs) (foldr disj fail gs))

(define (pull $) (if (procedure? $) (pull ($)) $))
(define (take-1 $)
  (let ([$ (pull $)])
    (if (null? $) #f (car $))))

;; --- sudoku on top ---

(define digits '(1 2 3 4 5 6 7 8 9))

(define (idx i j) (+ (* 9 i) j))

;; for every cell, the indices of the cells it shares a row, column,
;; or box with (itself excluded)
(define peers
  (for*/vector ([i 9] [j 9])
    (define r0 (* 3 (quotient i 3)))
    (define c0 (* 3 (quotient j 3)))
    (remove (idx i j)
            (remove-duplicates
             (append
              (for/list ([k 9]) (idx i k))
              (for/list ([k 9]) (idx k j))
              (for*/list ([r (in-range r0 (+ r0 3))]
                          [c (in-range c0 (+ c0 3))])
                (idx r c)))))))

(define (cands b n)
  (remove* (for/list ([k (vector-ref peers n)]
                      #:unless (zero? (vector-ref b k)))
             (vector-ref b k))
           digits))

;; the whole solver is one goal. it walks the board under the current
;; substitution, picks the most constrained cell, and branches on its
;; candidates, delaying recursion into the stream. a forced cell is
;; just an mrv cell with one candidate, so propagation is the case in
;; which the disj below is deterministic.
(define ((solveo cells) st)
  (define s (car st))
  (define b (for/vector ([c cells])
              (let ([v (walk c s)])
                (if (lvar? v) 0 v))))
  (define empties
    (for/list ([n 81] #:when (zero? (vector-ref b n)))
      (cons n (cands b n))))
  (define g
    (cond
      [(null? empties) succeed]
      [(memf (compose null? cdr) empties) fail]
      [else
       (define mrv (argmin (compose length cdr) empties))
       (disj/list
        (for/list ([d (cdr mrv)])
          (conj (== (vector-ref cells (car mrv)) d)
                (Zzz (solveo cells)))))]))
  (g st))

(define (solve board)
  (define flat (apply append board))
  (define cells
    (for/vector ([v flat] [n (in-naturals)])
      (if (zero? v) (lvar n) v)))
  (define st (cons (hash) 81))
  (define res (take-1 ((solveo cells) st)))
  (and res
       (for/list ([i 9])
         (for/list ([j 9])
           (walk (vector-ref cells (idx i j)) (car res))))))

(define (print-board board)
  (if board
      (for ([row board]) (displayln (string-join (map number->string row))))
      (displayln "unsatisfiable")))

(print-board (solve '((8 0 0 0 0 0 0 0 0)
                      (0 0 3 6 0 0 0 0 0)
                      (0 7 0 0 9 0 2 0 0)
                      (0 5 0 0 0 7 0 0 0)
                      (0 0 0 0 4 5 7 0 0)
                      (0 0 0 1 0 0 0 3 0)
                      (0 0 1 0 0 0 0 6 8)
                      (0 0 8 5 0 0 0 1 0)
                      (0 9 0 0 0 0 4 0 0))))
