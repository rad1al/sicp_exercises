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

;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.001))

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

(define (new-if predicate
                then-clause
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; As new-if is not a special form, it will always evaluate
; the then-clause and else-clause regardless of the value of the predicate.
; Replacing "if" in the definition of sqrt-iter will make it loop forever.

#|Exercise 1.7

> (sqrt 0.000001)
0.031260655525445276
> (sqrt 1000000)
1000.0000000000118
> (square 0.031260655525445276)
0.0009772285838805523
> (good-enough? 0.031260655525445276 0.000001) ;sqrt(0.000001) should be 0.001
#t
|#

(define (good-enough? guess x)
  (= (abs (- (square guess) x)) 0.0))

; For very small inputs of x, the tolerance of 0.001 may allow the approximation
; to end prematurely early and give an inaccurate result.
; For very large numbers, the procedure make take a very long time to evaluate
; or possibly never finish if the input value is greater than the maximum
; floating point precision.
; (sqrt 1e+32) => 1e+16
; (sqrt 1e+64) => runs forever


(define (cube-rt-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-rt-iter (cube-improve guess x) x)))

(define (cube-improve guess x)
  (average guess (/ (+ (/ x (* guess guess))
                       (* 2 guess))
                    3)))

; average is already defined.

(define (cube-rt x)
  (cube-rt-iter 1.0 x))

(define (cube-good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) (* 1.0e-12 x)))

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