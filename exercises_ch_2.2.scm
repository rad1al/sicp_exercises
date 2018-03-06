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

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))

#|

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