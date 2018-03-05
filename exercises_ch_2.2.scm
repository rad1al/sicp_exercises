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