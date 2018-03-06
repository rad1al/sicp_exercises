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

#|2.2.2 - Hierarchical Structures|#

(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

#|

(length x)
3

(count-leaves x)
4

|#

#|Mapping over trees|#



#|

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* factor tree))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

> (display (scale-tree (list 1
                             (list 2 (list 3 4) 5) 
                             (list 6 7))
                       10))

(10 (20 (30 40) 50) (60 70))

|#

;;; using map procedure:

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* factor sub-tree)))
       tree))

#|Sequences as Conventional Interfaces|#

(define (square x) (* x x))



#|

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares
                  (car tree))
                 (sum-odd-squares
                  (cdr tree))))))

> (sum-odd-squares '(9 ((3 4) 5)))
115

|#

(define (fib n)
  (define (fib-iter a b n)
    (if (= n 0)
        a
        (fib-iter b (+ a b) (- n 1) )))
  (fib-iter 0 1 n))

#|

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

> (display (even-fibs 10))
(0 2 8 34)

|#

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else  (filter predicate
                       (cdr sequence)))))

#|

> (display (filter odd? '(1 2 3 4 5)))
(1 3 5)

|#

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

#|

> (display (accumulate + 0 '(1 2 3 4 5)))
15

> (display (accumulate * 1 '(1 2 3 4 5)))
120

> (display (accumulate cons nil '(1 2 3 4 5)))
(1 2 3 4 5)

|#

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

#|

> (display (enumerate-interval 2 7))
(2 3 4 5 6 7)

|#

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

#|

> (enumerate-tree (list 1 (list 2 (list 3 4)) 5))
(enumerate-tree '(1 (2 (3 4)) 5))
(append (enumerate-tree 1)
        (enumerate-tree '((2 (3 4)) 5))
(append '(1) (append (enumerate-tree '(2 (3 4)))
                     (enumerate-tree 5)))
(append '(1)
 (append
  (append (enumerate-tree 2) (enumerate-tree '(3 4)))
  '(5)))
(append '(1)
 (append
  (append '(2) (append '(3) (enumerate-tree '(4))))
  '(5)))
(append '(1)
 (append
  (append '(2) (append '(3) '(4)))
  '(5)))
(append '(1)
 (append (append '(2) '(3 4)) '(5)))
(append '(1) (append '(2 3 4) '(5)))
(append '(1) '(2 3 4 5))
(1 2 3 4 5)

|#

(define (sum-odd-squares tree)
  (accumulate
   +
   0
   (map square
        (filter odd? (enumerate-tree tree)))))

#|

> (sum-odd-squares '(9 ((3 4) 5)))
115

|#

(define (even-fibs n)
  (accumulate
   cons
   nil
   (filter even?
           (map fib
                (enumerate-interval 0 n)))))

#|

> (display (even-fibs 10))
(0 2 8 34)

|#

(define (list-fibs-squares n)
  (accumulate
   cons
   nil
   (map square
        (map fib
             (enumerate-interval 0 n)))))

#|

> (display (list-fib-squares 10))

|#

(define
  (product-of-squares-of-odd-elements
   sequence)
  (accumulate
   *
   1
   (map square (filter odd? sequence))))

#|

> (display
   (product-of-squares-of-odd-elements
    (list 1 2 3 4 5)))
225

|#

#|Nested Mappings

(accumulate
    append
    nil
    (map (lambda (i)
           (map (lambda (j)
                  (list i j))
                (enumerate-interval* 1 (- i 1))))
         (enumerate-interval* 1 n)))

|#

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

|#

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

#|

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

