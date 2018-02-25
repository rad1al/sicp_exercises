#lang sicp

#|1.1 The Elements of Programming|#

(define size 2)

(define pi 3.14159)

(define radius 10)

(define circumference (* 2 pi radius))

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

#|
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
|#

(define (abs x)
  (if (< x 0)
      (- x)
      x))

;(and #t #t #f) => #f
;(or #f #f #t) => #t
;(not #f) => #t

#|1.1.7 Example: Square Roots by Newton's Method|#

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

#|
> (sqrt 9)
3.00009155413138

> (sqrt (+ 100 37))
11.704699917758145

> (sqrt (+ (sqrt 2) (sqrt 3)))
1.7739279023207892

> (square (sqrt 1000))
1000.000369924366
|#


#| Block structure:

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

> (sqrt 36)
6.000000005333189

|#

#| Lexical Scoping
; Since it is not necessary to pass x explicitly to each of the procedures,
; we can allow x to be a free variable in the internal definitions:

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

> (sqrt 25)
5.000023178253949

|#