#lang sicp

(define (cube x) (* x x x))

#|
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b) 
      0 
      (+ (cube a) 
         (sum-cubes (+ a 1) b))))


(define (pi-sum a b) ; converges to pi/8 slowly.
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))
|#

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

#|

> (sum-cubes 1 2)
(sum cube 1 inc 2)
(+ (cube 1) (sum cube (inc 1) inc 2))
(+ 1 (sum cube 2 inc 2))
(+ 1 (+ (cube 2) (sum cube (inc 2) inc 2)))
(+ 1 (+ 8 (sum cube 3 inc 2)))
(+ 1 (+ 8 (+ 0)))
9

|#

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

#|
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


> (* 8 (pi-sum 1 1000))
3.139592655589783
|#

#| Approximation of the definite integral between the limits a and b

(define (integral f a b dx) 
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))


> (integral cube 0 1 0.01)
0.24998750000000042
|#

#|1.3.2 - Constructing Procedures Using Lambda|#

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))


#|

(define (plus4 x) (+ x 4))

is equivalent to:

(define plus4 (lambda (x) (+ x 4)))

|#

(define plus4 (lambda (x) (+ x 4)))

(define (square x) (* x x))

#|
> (plus4 3)
7

> ((lambda (x y z) (+ x y (square z))) 1 2 3)
12

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))


> (f 10 3)
9542

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

> (f 10 3)
9542

|#

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

#|

> (f 10 3)
9542

(define (f x y)
  (define a 
    (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))

> (f 10 3)
9542

|#

#|1.3.3 Procedures as General Methods|#

(define (average a b)
  (/ (+ a b) 2))

(define (search f neg-point pos-point)
  (let ((midpoint
         (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond
            ((positive? test-value)
             (search f neg-point midpoint))
            ((negative? test-value)
             (search f midpoint pos-point))
            (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value)
                (positive? b-value))
           (search f a b))
          ((and (negative? b-value)
                (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of
                   opposite sign" a b)))))

#|

> (half-interval-method sin 2.0 4.0)
3.14111328125

> (half-interval-method
   (lambda (x) (- (* x x x) (* 2 x) 3))
   1.0
   2.0)
1.89306640625

|#

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

#|

> (fixed-point cos 1.0)
0.7390822985224023

> (cos 0.7390822985224023)
0.7390870426953322

> (fixed-point (lambda (y) (+ (sin y) (cos y)))
               1.0)
1.2587315962971173

(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

> (sqrt 2)
...loops forever

(define (sqrt x)
  (fixed-point
   (lambda (y) (average y (/ x y)))
   1.0))

|#


#|1.3.4 Procedures as Returned Values|#

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

#|
> ((average-damp square) 10)
55

(define (sqrt x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x y)))
   1.0))

(define (cube-root x)
  (fixed-point
   (average-damp
    (lambda (y)
      (/ x (square y))))
  1.0))


> (cube-root 8)
1.9999981824788517

|#

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

#|

> ((deriv square) 3)
6.000009999951316

> ((deriv cube) 5) ; x^3 -> 3x^2
75.00014999664018

|#

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
       ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

#|

Square root procedure defined with Newton's method:

(define (sqrt x)
  (newtons-method
   (lambda (y)
     (- (square y) x))
   1.0))

> (sqrt 2)
1.4142135623822438
|#

(define (fixed-point-of-transform
         g transform guess)
  (fixed-point (transform g) guess))

#|

Using fixed-point-of-transform and average-damp:

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   average-damp
   1.0))

|#

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))

#|
(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))
|#
