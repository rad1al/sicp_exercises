#lang sicp

#|Exercise 2.1|#

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom y) (numer x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

#|

> (car (car z))
1

> (car (cdr z))
3

; Does not reduce rational numbers to lowest terms:

(define (make-rat n d) (cons n d)) 

|#

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

#|
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g)
          (/ d g))))
|#

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (abs (gcd n d)))) ; get positive gcd
    (if (< d 0)
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))

#|

Logic for implementation:

1/1 -> keep both signs
-1/-1 -> flip both signs
1/-1 -> flip both signs
-1/1 -> keep both signs

We can see that we should flip both signs if the denominator
is negative.

|#

;(define one-half (make-rat 1 2))
;(print-rat one-half)

;(define one-third (make-rat 1 3))
;(print-rat (add-rat one-half one-third))

;(print-rat (mul-rat one-half one-third))

;(print-rat
; (add-rat one-third one-third))

#|Tests:

(print-rat (make-rat 8 12)) ; 2/3 
(print-rat (make-rat -8 12)) ; -2/3 
(print-rat (make-rat 8 -12)) ; -2/3 
(print-rat (make-rat -8 -12)) ; 2/3 

|#

#|Exercise 2.2|#

(define (average a b)
  (/ (+ a b) 2.0))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment line-segment)
  (car line-segment))

(define (end-segment line-segment)
  (cdr line-segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

#|

|#

(define (midpoint-segment line-segment)
  (let ((x1 (x-point (start-segment line-segment)))
        (x2 (x-point (end-segment line-segment)))
        (y1 (y-point (start-segment line-segment)))
        (y2 (y-point (end-segment line-segment))))
    (make-point (average x1 x2) (average y1 y2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define test-point-a (make-point 1 3))

(define test-point-b (make-point 5 7))

(define test-line-segment-a-b (make-segment test-point-a
                                             test-point-b))

(define test-point-c (make-point -10 20))

(define test-point-d (make-point 18 -55))

(define test-line-segment-c-d (make-segment test-point-c
                                             test-point-d))

#|

Tests/Sanity checks:

midpoint (1,3), (5,7) should be (3,5)
midpoint (-10,20), (18, -55) should be (4, -17.5)

> (print-point (midpoint-segment test-line-segment-a-b))
(3.0,5.0) ; correct

> (print-point (midpoint-segment test-line-segment-c-d))
(4.0,-17.5) ; correct

|#
