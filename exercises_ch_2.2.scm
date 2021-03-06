#lang sicp

#|Exercise 2.17|#

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))


#|

> (last-pair (list 23 72 149 34))
(mcons 34 '())

|#

#|Exercise 2.18|#



#|

;;; Recursive implementation of reverse:

(define (reverse lst)
  (if (null? lst)
      lst
      (append (reverse (cdr lst)) (list (car lst)))))

|#

;;; Iterative implementation of reverse:

(define (reverse lst)
  (define (reverse-iter lst a)
    (if (null? lst)
        a
        (reverse-iter (cdr lst) (cons (car lst) a))))
  (reverse-iter lst '()))

#|

> (reverse (list 1 2 3)))
(reverse-iter (1 2 3) '())
(reverse (2 3) (cons 1 '()))
(reverse (3) (cons 2 (cons 1 '())))
(reverse '() (cons 3 (cons 2 (cons 1 '()))))
(cons 3 (cons 2 (cons 1 '())))
(3 2 1)

> (display (reverse (list 1 4 9 16 25)))
(25 16 9 4 1)

|#

#|Exercise 2.19|#

; (define us-coins
;   (list 50 25 10 5 1))

(define us-coins
  (list 1 5 10 25 50))

(define uk-coins
  (list 100 50 20 10 5 2 0.5))

(define (count-change amount)
  (cc amount 5))

(define (cc amount coin-values)
  (cond ((= amount 0)
         1)
        ((or (< amount 0)
             (no-more? coin-values))
         0)
        (else
         (+ (cc
             amount
             (except-first-denomination
              coin-values))
            (cc
             (- amount
                (first-denomination
                 coin-values))
             coin-values)))))

(define (no-more? lst)
  (if (null? lst)
      #t
      #f))

(define (first-denomination lst)
  (car lst))

(define (except-first-denomination lst)
  (cdr lst))

#|

> (cc 100 '(1 5 10 25 50))
292
> (cc 100 '(25 10 5 1 50))
292

No, the order of list coin-values does not affect the answer provided
by cc because the procedure will compute all the possible combinations.

|#

#|Exercise 2.20|#

(define (same-parity head . tail)
  (define (same-parity-iter lst acc)
    (if (null? lst)
        acc
        (same-parity-iter (cdr lst)
              (if (even? (+ (car lst) head))
                  ;(append acc (list (car lst)))
                  (cons (car lst) acc) ; trying to avoid using append 
                  acc))))              ; requires the use of reverse
  ;(cons head (same-parity-iter tail '())))
  (cons head (reverse (same-parity-iter tail '()))))
    
#|

> (display (same-parity 1 2 3 4 5 6 7))
(1 3 5 7)

> (display (same-parity 2 3 4 5 6 7))
(2 4 6)

|#

#|Exercise 2.21|#

(define (square x) (* x x))

#|
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

;;; definition of square-list using map

(define (square-list items)
  (map square items))

> (display (square-list '(1 2 3 4)))
(1 4 9 16)

|#

#|Exercise 2.22

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

Does the following:

> (square-list '(1 2 3 4))
(iter (1 2 3 4) nil)
(iter (2 3 4) (cons 1 nil))
(iter (2 3) (cons 4 (1))
(iter (3) (cons 9 (4, 1)))
(iter '() (cons 16 (9, 4, 1)))
(cons 16 (9, 4, 1))
(16 9 4 1)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
(iter items nil))

Does the following:
> (display (square-list '(1 2 3 4)))
(iter (1 2 3 4) nil)
(iter (2 3 4) (cons nil 1))
(iter (3 4) (cons (nil 1) 4))
(iter (4) (cons ((nil 1) 4) 9))
(iter '() (cons (((nil 1) 4) 9) 16))
(cons (((nil 1) 4) 9) 16)
((((nil 1) 4) 9) 16)

|#

#|Exercise 2.23|#

;;; recursive solution:

(define (for-each proc lst)
    (cond ((null? lst) #t)       ; if-branches don't work well with 
          (else (proc (car lst)) ; multi-line elses
                (for-each proc (cdr lst)))))

#|

> (for-each (lambda (x) (newline) (display x))
            (list 57 321 88))

57
321
88#t

|#


#|Exercise 2.24

Interpreter print-out:

> (list 1 (list 2 (list 3 4)))
(mcons 1 (mcons (mcons 2 (mcons (mcons 3 (mcons 4 '())) '())) '()))

The box-and-pointer structure I drew on a sheet of paper.

The interpretation of this as a tree:

(1 (2 (3 4))
 |      |
 1   (2 (3 4))
      |   |
      2  (3 4)
          | |
          3 4

|#

#|Exercise 2.25|#

#|

> (car (cdr (car (cdr (cdr
                     '(1 3 (5 7) 9))))))
7

> (car (car '((7))))
7

> (car (cdr
   (car (cdr
    (car (cdr
     (car (cdr
      (car (cdr
       (car (cdr
             '(1 (2 (3 (4 (5 (6 7))))))
              ))))))))))))
7

> 
|#

#|Exercise 2.26

(define x (list 1 2 3))

(define y (list 4 5 6))

> (display (append x y))
(1 2 3 4 5 6)

> (display (cons x y))
((1 2 3) 4 5 6)

> (display (list x y))
((1 2 3) (4 5 6))

|#

#|Exercise 2.27|#

; (define x 
;   (list (list 1 2) (list 3 4)))

;;; Elegant solution using map:

(define (deep-reverse x)
  (if (pair? x)
    (reverse (map deep-reverse x))
    x))

#|

> (display x)
((1 2) (3 4))

> (display (reverse x))
((3 4) (1 2))

> (display (deep-reverse x))
((4 3) (2 1))

;;; alternative implementation:

(define (deep-reverse lst)
  (cond ((null? lst) nil)
        ((pair? (car lst))
         (append (deep-reverse (cdr lst))
                 (list (deep-reverse (car lst)))))
        (else (append
               (deep-reverse (cdr lst))
               (list (car lst))))))

|#

#|Exercise 2.28|#

(define x
  (list (list 1 2) (list 3 4)))

(define (fringe tree)
  (cond ((null? tree) nil)
        ((pair? (car tree))
         (append (fringe (car tree))
                 (fringe (cdr tree))))
        (else (cons (car tree)
                    (fringe (cdr tree))))))

#|

> (fringe '((1 2) (3 4)))
(append (fringe '(1 2)) (fringe '((3 4)))
(append (cons 1 (fringe '(2))) (fringe '((3 4))))
(append (cons 1 (fringe '(2))) (append (fringe '(3 4)) (fringe '())))
(append '(1 2) '(3 4))
'(1 2 3 4)

> (display (fringe x))
(1 2 3 4)

> (display (fringe '((1 2) (3 4) (5 6 (7 8) 9))))
(1 2 3 4 5 6 7 8 9)

|#

#|Exercise 2.29|#

#|
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))
|#

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

#|
(define (right-branch mobile)
  (car (cdr mobile)))
|#

(define (right-branch mobile)
  (cdr mobile))

(define (is-mobile? structure)
  (pair? structure))

(define (branch-length branch)
  (car branch))

#|
(define (branch-structure branch)
  (car (cdr branch)))
|#

(define (branch-structure branch)
  (cdr branch))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((bs (branch-structure branch)))
    (if (is-mobile? bs)
        (total-weight bs)
        bs)))

#|

> (total-weight '((1 2) (3 ((4 5) (6 7)))))
(+ (branch-weight '(1 2)) (branch-weight '(3 ((4 5) (6 7)))))
(+ 2 (total-weight '((4 5) (6 7))))
(+ 2 (+ (branch-weight '(4 5)) (branch-weight '(6 7))))
(+ 2 (+ 5 7))
(+ 2 12)
14

|#

(define (branch-torque branch)
  (let ((bs (branch-structure branch)))
    (if (is-mobile? bs)
        (* (branch-length branch)
           (total-weight bs))
        (* (branch-length branch)
           (branch-weight branch)))))

(define (balanced? mobile)
  (let ((m-brnch-l (left-branch mobile))
        (m-brnch-r (right-branch mobile)))
    (and (= (branch-torque m-brnch-l)
            (branch-torque m-brnch-r))
         (if (is-mobile? (branch-structure m-brnch-l))
             (balanced? (branch-structure m-brnch-l))
             #t)
         (if (is-mobile? (branch-structure m-brnch-r))
             (balanced? (branch-structure m-brnch-r))
             #t))))

(define test-mobile1
  (make-mobile (make-branch 4 6)
	       (make-branch 3 8)))

(define test-mobile2
  (make-mobile (make-branch 13 5)
	       (make-branch 5 test-mobile1)))

(define test-mobile3
  (make-mobile (make-branch 7 6)
	       (make-branch 3 test-mobile1)))

#|

> (total-weight test-mobile1)
14

> (total-weight test-mobile2)
19

> (total-weight test-mobile3)
20

> (balanced? test-mobile1) 
#t

> (balanced? test-mobile2) 
#f

> (balanced? test-mobile3) 
#t

Part 4: If we change the representation of mobiles to use cons
instead of list, we simply need to change right-branch and
branch-structure to invoke only (cdr _) instead of (car (cdr _)).

|#

#|Exercise 2.30|#


(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree)) ; square defined earlier
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

#|


> (square-tree '(1 (2 (3 4) 5) (6 7)))
(cons (square-tree 1) (square-tree '((2 (3 4) 5) (6 7))
(cons 1 (cons (square-tree '(2 (3 4) 5)) (square-tree '((6 7)))))
(cons 1 (cons (cons (square-tree 2) (square-tree '((3 4) 5)))
              (cons (square-tree '(6 7)) (square-tree '()))))
(cons 1 (cons (cons 4
                    (cons (square-tree '(3 4)) (square-tree '(5)))
              (cons (cons (square-tree 6) (square-tree '(7)) nil)))
(cons 1 (cons (cons 4
                    (cons '(9 16) '(25)))
              (cons (cons 36 '(49)) nil)))
(cons 1 (cons (cons 4 '((9 16) 25))
              (cons '(36 49) nil)))
(cons 1 (cons '(4 (9 16) 25)
              '((36 49))))
(cons 1 '((4 (9 16) 25) (36 49)))
(1 (4 (9 16) 25) (36 49))

;;; using map procedure:

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

|#

#|Exercise 2.31|#

(define (square-tree* tree)
  (tree-map square tree))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

#|Exercise 2.32|#

(define (subsets s)
  (define (attach-s-head-to x)
    (cons (car s) x))
    ;(lambda (x) (cons (car s) x)) 
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map attach-s-head-to rest)))))

#|

We need to make sure the subsets procedure works for the simplest
cases - 0 and 1 element lists. We notice from the function template
that it would be helpful to append the head of our input to every
element in our previous/one-step-smaller result:

> (subsets '())
(())

> (subsets '(3))
(append (subsets '()) (map attach-3-to (subsets '()))
(append '(()) (map attach-3-to '(())))
(append '(()) '((3)))
(() (3))

> (subsets '(2 3))
(append (subsets '(3))
        (map attach-2-to (subsets '(3))))
(append '(() (3)) (map attach-2-to '(() (3))))
(append '(() (3)) '((2) (2 3)))
(() (3) (2) (2 3))

> (subsets '(1 2 3))
(append rest (map append-1-to rest))
(append (subsets '(2 3)) (map attach-1-to (subsets '(2 3))))
(append '(() (3) (2) (2 3))
        (map attach-1-to '(() (3) (2) (2 3)))
(append '(() (3) (2) (2 3))
        '((1) (1 3) (1 2) (1 2 3)))
(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

|#

#|Exercise 2.33|#

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

#|

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))

> (map square '(1 2 3))
(accumulate (lambda (x y) (cons (p x) y)) nil '(1 2 3))
(cons (square 1) (accumulate (lambda (x y) (cons (p x) y)) nil '(2 3)))
...
(cons 1 (cons 4 (cons 9 (accumulate (lambda (x y) (cons (p x) y))
                                     nil
                                     nil))))
(cons 1 (cons 4 (cons 9 nil)))
(1 4 9)

|#

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

#|

> (append '(1 2 3) '(4 5 6))
(accumulate cons '(4 5 6) '(1 2 3))
(cons 1 (accumulate cons '(4 5 6) '(2 3)))
...
(cons 1 (cons 2 (cons 3 (accumulate cons '(4 5 6) '()))))
(cons 1 (cons 2 (cons 3 '(4 5 6))))
(1 2 3 4 5 6)

|#

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0
              sequence))

#|

> (length '(1 4 9 16 25))
(accumulate (lambda (x y) (+ 1 y)) 0 '(1 4 9 16 25))
((lambda (x y) (+ 1 y)) 1
                        (accumulate ... 0 '(4 9 16 25)))
(f 1 (f 4 (f 9 (f 16 (f 25 0)))))
(f 1 (f 4 (f 9 (f 16 1))))
(f 1 (f 4 (f 9 2)))
(f 1 (f 4 3))
(f 1 4)
5

|#


#|Exercise 2.34|#

(define
  (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

#|

> (horner-eval 3 '(10 2))  ; 10 + x
(+ 10 (* 3 (+ 2 (* 3 0)))) ; 10 + (3 * (2 + 0))
16

The procedure seems to go all the way to the right and start
accumulating the results of multiplying by x and then adding it
to the next term.

> (horner-eval 2 '(1 3 0 5 0 1))
(accumulate (lambda (this-coeff higher-terms)
             (+ this-coeff (* x higher-terms))
            0
            '(1 3 0 5 0 1))
(+ 1 (* 2 (accumulate f 0 '(3 0 5 0 1))))
(+ 1 (* 2 (+ 3 (* 2 (accumulate f 0 '(0 5 0 1))))))
(+ 1 (* 2 (+ 3 (* 2 (+ 0 (* 2 (+ 5 (* 2 (+ 0 (* 2 (+ 1 (* 2 0))))))))))))
79

|#

#|Exercise 2.35|#

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

#|

;;; without enumerate-tree:

(define (count-leaves tree)
  (accumulate + 0
              (map (lambda (x)
                     (cond ((null? x) 0)
                           ((not (pair? x)) 1)
                           (else (count-leaves x))))
                   tree)))
|#

;;; with enumerate-tree

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (x) 1)
                         (enumerate-tree tree))))

#|

> (display (enumerate-tree '((1 ()) (3 4))))
(1 3 4)

> (count-leaves '())
0

> (count-leaves '(1))
1

> (count-leaves '((1 2) 3))
3

> (count-leaves '((1 2) (3 4)))
4

|#

#|Exercise 2.36|#

(define (sum . lst)
  (display))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

#|

> (accumulate-n + 0 '((1 2 3) (4 5 6)))
(cons (accumulate + 0 '(1 4))
      (accumulate-n + 0 '((2 3) (5 6))))
(cons 5 (cons 7 (cons 9 '())))
(5 7 9)

> (accumulate-n + 0 s)
(22 26 30)

|#

#|Exercise 2.37|#

(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

#|

> (map * '(1 2 3) '(4 5 6))
(4 10 18)

> (dot-product '(1 2 3) '(4 5 6))
32

|#

(define A '((1 -1 2) (0 -3 1)))

(define b '(2 1 0))


(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

#|

> (display (matrix-*-vector A b))
(1 -3)

|#

(define (transpose mat)
  (accumulate-n cons nil mat))

#|

> (display (transpose Y))
((0 1) (1 -1) (2 3))

|#

(define X '((0 4 -2) (-4 -3 0)))

(define Y '((0 1) (1 -1) (2 3)))

#|
(define (matrix-*-matrix m n)
  (map (lambda (v)
         (map (lambda (w)
                (dot-product v w)) m))
       (transpose n)))
|#

;;; Better definition:

(define (matrix-*-matrix m n)
  (let ((n-columns (transpose n)))
    (map (lambda (m-row)
           (matrix-*-vector n-columns m-row))
         m)))

#|

> (display (matrix-*-matrix X Y))
((0 -3) (-10 -1))

|#

#|Exercise 2.38

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

|#

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

#|

> (fold-right / 1 (list 1 2 3))
1.5 ; (/ 1 (/ 2 (/ 3 1)))

> (fold-left  / 1 (list 1 2 3))
1/6 ; (/ (/ (/ 1 1) 2) 3)

> (fold-right list nil (list 1 2 3))
(list 1 (list 2 (list 3 nil)))
(1 (2 (3 ()))

> (fold-left  list nil (list 1 2 3))
(list (list (list nil 1) 2) 3)
(((() 1) 2) 3)

(op result (car rest)) should be equivalent to
(op (car rest) result) implying it must be commutative and
associative:

Using + for op:
fold-right: a + 0
fold-left:  0 + a

Using + for op
fold-right: a + (b + 0)
fold-left: (a + b) + 0

|#

#|Exercise 2.39|#

(define (reverse* sequence)
  (fold-right
   (lambda (x y) (append y (list x))) nil sequence))

(define (reverse** sequence)
  (fold-left
   (lambda (x y) (cons y x)) nil sequence))

#|

> (display (reverse* '(1 2 3)))
(3 2 1)

> (display (reverse** '(1 2 3)))
(3 2 1)

|#

#|Exercise 2.40|#

;;; procedures to find primes:

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (= n 1)
      #f
      (= n (smallest-divisor n))))

;;; filter, enumerate-interval

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else  (filter predicate
                       (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

;;; flatmap, prime-sum?, make-pair-sum:

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

#|

> (prime-sum? '(1 2))
#t

> (display (make-pair-sum '(3 4)))
(3 4 7)

> (display (flatmap identity '((1) (2) (3))))
(1 2 3)

;;; prime-sum-pairs:

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
        prime-sum?
        (flatmap
         (lambda (i)
           (map (lambda (j)
                  (list i j))
                (enumerate-interval
                 1
                 (- i 1))))
         (enumerate-interval 1 n)))))

> (display (prime-sum-pairs 6))
((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

|#

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p)
                        (cons x p))
                      (permutations
                       (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

#|

> (display (permutations '(1 2 3)))
((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))

|#

(define (unique-pairs n)
  (flatmap
   (lambda (x)
    (map (lambda (y) (list x y))
         (enumerate-interval 1 (- x 1))))
   (enumerate-interval 1 n)))

#|

> (display (unique-pairs 3))
((2 1) (3 1) (3 2))

|#

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

#|

> (display (prime-sum-pairs 6))
((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

|#

#|Exercise 2.42|#

(define (triples-to-s s)
  (flatmap
   (lambda (x)
     (flatmap
      (lambda (y)
        (map
         (lambda (z) (list x y z))
         (enumerate-interval 1 (- y 1))))
      (enumerate-interval 1 (- x 1))))
   (enumerate-interval 1 s)))

(define (check-if-sum-to-s t s)
  (if (= s (accumulate + 0 t))
      #t
      #f))

(define (ordered-triples-which-sum-to-s
         n
         s)
  (filter
   (lambda (t) (check-if-sum-to-s t s))
   (triples-to-s n)))

#|

> (display (triples-to-s 2))
((1 1 1) (1 1 2) (1 2 1) (1 2 2) (2 1 1) (2 1 2) (2 2 1) (2 2 2))

> (display (ordered-triples-which-sum-to-s 3 6))
((3 2 1))

> (display (ordered-triples-which-sum-to-s 10 10))
((5 3 2) (5 4 1) (6 3 1) (7 2 1))

|#

#|Exercise 2.42|#

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row
                    k
                    rest-of-queens))
                 (enumerate-interval
                  1
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position row k rest-of-queens)
  (cons (list row k) rest-of-queens))

(define empty-board
  '())

;;; human-readable implementation of safe?:

(define (safe? col-k positions)
  (let ((current-queen (car positions))
        (rest-of-queens (cdr positions)))
    (let ((row-k (car current-queen)))
      (null? (filter
              (lambda (position)
                (let ((row (car position))
                      (col (cadr position)))
                  (or (= row row-k) ; check if queen on same row as kth queen
                      (= col col-k) ; check if queen on same column as kth queen
                      (= (abs (- row row-k)) ; check if queen is on same diagonal
                         (abs (- col col-k)))))) ; as kth queen i.e. (5,4) vs (3,6)
             rest-of-queens)))))



#|

> (display (queens 4))
(((3 4) (1 3) (4 2) (2 1)) ((2 4) (4 3) (1 2) (3 1)))

> (display (length (queens 8)))
92

> (display (car (queens 8)))
((4 8) (2 7) (7 6) (3 5) (6 4) (8 3) (5 2) (1 1))

|#

#|Exercise 2.43

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position
                    new-row
                    k
                    rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

> (display (queens 4))
(((2 4) (4 3) (1 2) (3 1)) ((3 4) (1 3) (4 2) (2 1)))

Switching the order of the nested mappings will cause
queens-cols to be re-evaluated for every element in
(enumerate-interval 1 board-size) so we duplicate our
work N times where N = board-size for each recursion level.

As there are N recursion levels so we will end up dupliating
the work N^N times. So for a board of size 8, and T time to
solve that n-queens problem, it will take (8^8)*T time to run.  

|#

