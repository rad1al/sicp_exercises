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

#|Exercise 2.3

The following procedures assume the rectangles being constructed are
parallel to the X-axis and Y-axis. 
|#

;;; Distance formula

(define (dist start-point end-point)
  (define (square x) (* x x))
  (let ((x1 (x-point start-point))
        (y1 (y-point start-point))
        (x2 (x-point end-point))
        (y2 (y-point end-point)))
    (sqrt (+ (square (- x2 x1)) (square (- y2 y1))))))

;;; Implement segment-length using distance formula

(define (segment-length line-segment)
  (let ((a (start-segment line-segment))
        (b (end-segment line-segment)))
    (dist a b)))

;> (segment-length (make-segment (make-point 0 0) (make-point 3 4)))
;5

;;; Procedure make-rectangle-via-points makes a rectangle using 
;;; 4 points arranged as:
;;;
;;; p4 p3
;;; p1 p2  


(define (make-rectangle-via-points p1 p2 p3 p4)
  (cons (cons p1 p2) (cons p3 p4)))

(define (get-p1 rect)
  (car (car rect)))

(define (get-p2 rect)
  (cdr (car rect)))

(define (get-p3 rect)
  (car (cdr rect)))

(define (get-p4 rect)
  (cdr (cdr rect)))

;;; Procedure make-rectangle-2-sides makes a rectangle with two 
;;; perpendicular sides using make-rectangle-via-points.
;;; The two sides are a side vertical-segment (d a) and
;;; horizontal-segment (a b) using the following point positions.
;;;
;;; d c
;;; a b

(define (make-rectangle-2-sides vertical-segment horizontal-segment)
  (let ((d (start-segment vertical-segment))
        (a (end-segment vertical-segment))
        (b (end-segment horizontal-segment)))
        ; In the body, derive c = (bx, by + dist(d, a)) 
    (make-rectangle-via-points a
                               b
                               (make-point (x-point b) (+ (y-point b) (dist d a)))
                               d)))

;;; vertical and horizon side selectors for a rectangle:

(define (vertical-segment rectangle)
  (make-segment (get-p4 rectangle) (get-p1 rectangle)))
  
(define (horizontal-segment rectangle)
  (make-segment (get-p1 rectangle) (get-p2 rectangle)))

;;; area and perimeter procedures:

(define (perimeter rectangle)
  (* 2 (+ (rect-width rectangle)
          (rect-height rectangle))))

(define (area rectangle)
  (* (rect-width rectangle)
     (rect-height rectangle)))

;;; procedures to get width and height of a rectangle

(define (rect-width rectangle)
  (segment-length (horizontal-segment rectangle)))

(define (rect-height rectangle)
  (segment-length (vertical-segment rectangle)))

;;; find-missing-rect-corner finds the point p4 of a rectangle 
;;; where p4 is the reflection of p2 over the diagonal created by 
;;; p1 and p3.
;;;
;;; It will allow us to find the area of a rectangle with only 2 sides
;;; known/3 points even if it's rotated.

(define (find-missing-rect-corner p1 p2 p3)
  (let ((m (midpoint-segment (make-segment p1 p3))))
    (make-point (+ (x-point m) (- (x-point m) (x-point p2)))
                (+ (y-point m) (- (y-point m) (y-point p2))))))

(define (make-rectangle-3-corners p1 p2 p3)
  (let ((p4 (find-missing-rect-corner p1 p2 p3)))
    (make-rectangle-via-points p1 p2 p3 p4)))

;;; Tests

(define test-rect-with-points (make-rectangle-via-points
                               (make-point 1 1)
                               (make-point 6 1)
                               (make-point 6 10)
                               (make-point 1 10)))

(define test-rect-with-sides (make-rectangle-2-sides
                              (make-segment (make-point 1 10)
                                            (make-point 1 1))
                              (make-segment (make-point 1 1)
                                            (make-point 6 1))))

(define test-rect-3-corners
  (make-rectangle-3-corners (make-point 1 1)
                            (make-point 1 10)
                            (make-point 6 10)))

(define test-rotated-rect-3-corners
  (make-rectangle-3-corners (make-point 0 4)
                            (make-point 3 0)
                            (make-point 8.6 4.2)))


#|

Running the Tests:

> (area test-rect-with-points)
45

> (area test-rect-with-sides)
45

> (perimeter test-rect-with-points)
28

> (perimeter test-rect-with-sides)
28

> (print-point
 (find-missing-rect-corner (make-point 1 10)
                           (make-point 6 10)
                           (make-point 6 1)))

(1.0,1.0)

> (area test-rect-3-corners)
45.0

> (perimeter test-rect-3-corners)
28.0

> (area test-rotated-rect-3-corners)
34.99999999999999

> (perimeter test-rotated-rect-3-corners)
24.0

|#
