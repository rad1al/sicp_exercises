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