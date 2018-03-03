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

#|Exercise 2.4

(define (cons x y) 
  (lambda (m) (m x y)))

(define (car z) 
  (z (lambda (p q) p)))

> (car (cons 1 2))
(car (lambda (m) (m 1 2))
((lambda (m) (m 1 2)) (lambda (p q) p))
((lambda (p q) p) 1 2)
1

;;; Procedural procedural definition of cdr : 

(define (cdr z) 
  (z (lambda (p q) q)))

> (cdr (cons 1 2))
(cdr (lambda (m) (m 1 2))
((lambda (m) (m 1 2)) (lambda (p q) q))
((lambda (p q) q) 1 2)
2

|#

#|Exercise 2.5

Strategy:

(5,7) can be expressed and stored as 2^5 * 3^7, then to retrieve
the information for the first digit, we can factor out the 3s to get
a power of 2 and then take the log base 2 of that result to get 5.
Do the same thing for the other integer we wish to retrieve.

|#

;; constructor 

(define (pcons a b) (* (expt 2 a) (expt 3 b))) 

;; Procedure which calculates the log of n in base b.

(define (log-in-base base n) (ceiling (/ (log n) (log base))))

;; Procedure to factor out all n's from a number x.

(define (remove-factors x n)
  (if (= 1 (gcd x n))
      x
      (remove-factors (/ x n) n)))

;; Selectors

(define (pcar x) (log-in-base 2 (remove-factors x 3)))

(define (pcdr x) (log-in-base 3 (remove-factors x 2)))

#|

> (pcons 5 7)
69984

> (pcar (pcons 5 7))
5.0

> (pcdr (pcons 5 7))
7.0

|#

#|Exercise 2.6|#

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

#|

One:

(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f x)))

Two:

(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f (f x))))

Three:

(f (f (f x)))

a + b seems to be a function iterated a + b times. Recall the
compose function where compose f g = f (g x):


|#

(define (compose f g) (lambda (x) (f (g x)))) 


(define (add a b)
  (lambda (f) (compose (a f) (b f))))

#|

> ((one inc) 10)
11

> ((two inc) 5)
7

|#

#|Exercise 2.7|#



(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (positive? x) (>= x 0)) 

(define (negative? x) (< x 0)) 

        
#|
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval
                 (/ 1.0 (upper-bound y))
                 (/ 1.0 (lower-bound y)))))

|#



(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))

(define (upper-bound interval) (cdr interval))


#|Exercise 2.8|#


(define (sub-interval x y)
  (let ((p1 (- (lower-bound x)
               (lower-bound y)))
        (p2 (- (lower-bound x)
               (upper-bound y)))
        (p3 (- (upper-bound x)
               (lower-bound y)))
        (p4 (- (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

#|

> (add-interval (make-interval 5 10) (make-interval 1 2))
(mcons 6 12)

> (mul-interval (make-interval 5 10) (make-interval 1 2))
(mcons 5 20)

> (div-interval (make-interval 5 10) (make-interval 1 2))
(mcons 2.5 10.0)

> (sub-interval (make-interval 5 10) (make-interval 1 2))
(mcons 3 9)

> (sub-interval (make-interval 1 2) (make-interval 5 10))
(mcons -9 -3)

|#

#|Exercise 2.9|#

(define (width interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2.0))

(define A (make-interval 3 5))
(define B (make-interval 4 10))

#|

> (+ (width A) (width B))
4

> (width (add-interval A B))
4.0

> (+ (width A) (width B))
4.0

; Multiplication and division is dependent on the input interval bounds.

> (width (mul-interval (make-interval 0 2) (make-interval 0 10)))
10.0

> (width (mul-interval (make-interval 0 2) (make-interval 4 14)))
14.0

> (width (div-interval (make-interval 2 4) (make-interval 2 12)))
0.9166666666666666

> (width (div-interval (make-interval 2 4) (make-interval 4 14)))
0.4285714285714286

|#

#|Exercise 2.10|#

(define (div-interval x y)
  (if (or (= 0 (upper-bound y))
          (= 0 (lower-bound y)))
      (error "Divide by zero error (the interval spans 0)")
      (mul-interval x
                    (make-interval
                    (/ 1.0 (upper-bound y))
                    (/ 1.0 (lower-bound y))))))

#|

> (div-interval (make-interval 0 0) (make-interval 3 5))
|#

#|Exercise 2.11|#

(define (mul-interval x y)
  (let ((x-l (lower-bound x))
        (x-u (upper-bound x))
        (y-l (lower-bound y))
        (y-u (upper-bound y)))
    (cond ((and (positive? x-l) (positive? y-l))
           (make-interval (* x-l y-l) (* x-u y-u))) 
          ((and (positive? x-l) (negative? y-l))
           (make-interval (* x-u y-l) (* (if (negative? y-u) x-l x-u)
                                         y-u))) 
          ((and (negative? x-l) (positive? y-l)) 
           (make-interval (* x-l y-u) (* x-u
                                         (if (negative? x-u) y-l y-u)))) 
          ((and (positive? x-u) (positive? y-u))
           (make-interval (min (* x-l y-u) (* x-u y-l))
                          (max (* x-l y-l) (* x-u y-u)))) 
          ((and (positive? x-u) (negative? y-u)) 
           (make-interval (* x-u y-l) (* x-l y-l))) 
          ((and (negative? x-u) (positive? y-u)) 
           (make-interval (* x-l y-u) (* x-l y-l))) 
          (else 
           (make-interval (* x-u y-u) (* x-l y-l))))))

#|Exercise 2.12|#

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (make-center-percentage c pct-tol)
  (let ((tolerance (/ pct-tol 100.0)))
    (make-interval (- c (abs (* c tolerance)))
                   (+ c (abs (* c tolerance))))))

(define (percent interval)
  (let ((c (center interval))
        (w (width interval)))
    (* (/ w c) 100)))


#|

> (make-center-percentage -10 20)
(mcons -12.0 -8.0)

> (percent (make-interval 2.5 7.5))
50.0

|#



#|Exercise 2.13

; l, u means lower, upper respectively 

p = w/c ; percentage tolerance
w = p*c ; implied by p = w/c
w = (u-l)/2 ; width
c = (u+l)/2 ; center

p = (u-l)/(u+l)

Say we have an operation x*y = z we assume that x > 0 and y > 0,
we can observe that:

[zl, zu] = [xl, xu] * [yl, yu] = [xl*yl, xu*yu]

which means zl = xl * yl and zu = xu * yu

pz = (zu - zl)/(zu + zl) = (xu*yu - xl*yl)/(xu*yu + xl*yl)

Using center and width:

lx = cx - wx = cx - px*cx = cx(1 - px)
ly = cy - wy = cy - py*cy = cy(1 - py)
ux = cx + wx = cx + px*cx = cx(1 + px)
uy = cy + wy = cy + py*cy = cy(1 + py)

xl*yl = cx*cy(1 - px)(1 - py)
xu*yu = cx*cy(1 + px)(1 + py)

Using previous equation for pz, we can obtain:

pz = (px + py)/(1 + px*py)

For sufficiently small values of px*py, the denominator would be
close to 1 and the fraction would be approximately px + py.

|#


(define t1 (make-center-percentage 5 0.02))
(define t2 (make-center-percentage 13 0.01))

(define t1*t2 (mul-interval t1 t2))

#|

> (percent t1)
0.02000000000000668

> (percent t2)
0.010000000000004023

> (percent t1*t2)
0.029999999400012082

|#

#|Exercise 2.14|#

(define (mul-pct-tolerance x y)
  (+ (percent x) (percent y)))

(define (par1 r1 r2)
  (div-interval
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one
     (add-interval
      (div-interval one r1)
      (div-interval one r2)))))

#|

> (par1 (make-interval 5 6) (make-interval 3 7))
(mcons 1.153846153846154 5.25)

> (par2 (make-interval 5 6) (make-interval 3 7))
(mcons 1.875 3.230769230769231)

> (div-interval (make-interval 1 1) (make-interval 3 4))
(mcons 0.25 0.3333333333333333)

> (div-interval (make-interval 3 4) (make-interval 3 4))
(mcons 0.75 1.3333333333333333)

> (div-interval (make-interval 4 4) (make-interval 4 4))
(mcons 1.0 1.0)

> (div-interval (make-interval 4 4) (make-interval 4 4.1))
(mcons 0.9756097560975611 1.0)

|#