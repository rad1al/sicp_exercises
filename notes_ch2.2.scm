#lang sicp


#|Hierarchical Data and the Closure Property|#

(define one-through-four (list 1 2 3 4))

#|

one-through-four
> (mcons 1 (mcons 2 (mcons 3 (mcons 4 '()))))

(car one-through-four)
> 1

(cdr one-through-four)
> (mcons 2 (mcons 3 (mcons 4 '())))

(car (cdr one-through-four))
> 2

> (cons 10 one-through-four)
(mcons 10 (mcons 1 (mcons 2 (mcons 3 (mcons 4 '())))))

> (mcons 5 (mcons 1 (mcons 2 (mcons 3 (mcons 4 '())))))

|#

;;; Obtain the nth item of a list.

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items)
                (- n 1))))

(define squares
  (list 1 4 9 16 25))


#|
> (list-ref squares 3)
16

;;; Recursive implementation of length procedure:

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
|#

(define odds
  (list 1 3 5 7))

#|
> (length odds)
4
|#

;;; Iterative implementation of length procedure

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a)
                     (+ 1 count))))
  (length-iter items 0))

#|

> (display (append squares odds))
(1 4 9 16 25 1 3 5 7)

> (display (append odds squares))
(1 3 5 7 1 4 9 16 25)

|#

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1)
                    list2))))

#|

> (append odds squares)
(append (1 3 5 7) (1 4 9 16 25))
(cons 1 (append (3 5 7) (1 4 9 16 25)))
(cons 1 (cons 3 (append (5 7) (1 4 9 16 25))))
(cons 1 (cons 3 (cons 5 (append (7) (1 4 9 16 25)))))
(cons 1 (cons 3 (cons 5 (cons 7 (append nil (1 4 9 16 25))))))
(cons 1 (cons 3 (cons 5 (cons 7 (1 4 9 16 25)))))
(1 3 5 7 1 4 9 16 25)

|#

#|Mapping over lists|#


#|

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

> (display (scale-list (list 1 2 3 4 5) 10))
(10 20 30 40 50)

|#

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))


#|

> (display (map abs (list -10 2.5 -11.6 17)))
(10 2.5 11.6 17)

> (display (map (lambda (x) (* x x)) (list 1 2 3 4)))
(1 4 9 16)

|#

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))