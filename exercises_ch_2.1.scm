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

; - Point -

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; - Segment -

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment line-segment)
  (car line-segment))

(define (end-segment line-segment)
  (cdr line-segment))

(define (midpoint-segment line-segment)
  (let ((x1 (x-point (start-segment line-segment)))
        (x2 (x-point (end-segment line-segment)))
        (y1 (y-point (start-segment line-segment)))
        (y2 (y-point (end-segment line-segment))))
    (make-point (average x1 x2) (average y1 y2))))

; - Testing -

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

#|Exercise 2.3|#

(define (square x) (* x x))

(define (make-rectangle p1 p2 p3 p4)
  (cons (cons p1 p2) (cons p3 p4)))

(define (get-p1 rect)
  (car (car rect)))

(define (get-p2 rect)
  (cdr (car rect)))

(define (get-p3 rect)
  (car (cdr rect)))

(define (get-p4 rect)
  (cdr (cdr rect)))

#|
(define (length line-segment)
  (let ((x1 (x-point (start-segment line-segment)))
        (y1 (y-point (start-segment line-segment)))
        (x2 (x-point (end-segment line-segment)))
        (y2 (y-point (end-segment line-segment))))
    (sqrt (+ (square (- x2 x1)) (square (- y2 y1))))))

|#
(define (length start-point end-point)
  (let ((x1 (x-point start-point))
        (y1 (y-point start-point))
        (x2 (x-point end-point))
        (y2 (y-point end-point)))
    (sqrt (+ (square (- x2 x1)) (square (- y2 y1))))))

(define origin (make-point 0 0))

(define test-point-e (make-point -3 4))

#|

Testing length procedure:

Distance between (0,0) and (-3, 4) -> 5

> (length (make-segment origin test-point-e))
^ Procedure to work with a line-segment.

> (length origin test-point-e)
5

|#

(define (perimeter rectangle)
  (let ((p1 (get-p1 rectangle))
        (p2 (get-p2 rectangle))
        (p3 (get-p3 rectangle))
        (p4 (get-p4 rectangle)))
    (+ (length p1 p2)
       (length p2 p3)
       (length p3 p4)
       (length p4 p1))))

(define (area rectangle)
  (let ((l (length (get-p1 rectangle)
                  (get-p2 rectangle)))
        (w (length (get-p2 rectangle)
                   (get-p3 rectangle))))
    (* l w)))

(define test-rectangle
  (let ((p1 (make-point 1 1))
        (p2 (make-point 6 1))
        (p3 (make-point 6 10))
        (p4 (make-point 1 10)))
  (make-rectangle p1 p2 p3 p4)))


#|

Tests for rectangle procedures:

> (perimeter test-rectangle)
28 ; correct

> (area test-rectangle)
45 ; correct

|#
