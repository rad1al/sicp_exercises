#lang sicp

#|Exercise 1.1|#

10                                 ; 10
(+ 5 3 4)                          ; 12
(- 9 1)                            ; 8
(/ 6 2)                            ; 3
(+ (* 2 4) (- 4 6))                ; 6
(define a 3)                       ; a = 3
(define b (+ a 1))                 ; b = 4
(+ a b (* a b))                    ; 19
(= a b)                            ; #f
(if (and (> b a) (< b (* a b)))    ; 4
    b
    a)
(cond ((= a 4) 6)                  ; 16
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))             ; 6
(* (cond ((> a b) a)               ; 16
         ((< a b) b)
         (else -1))
   (+ a 1))

#|Exercise 1.2|#
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

#|Exercise 1.3|#
(define (square x) (* x x))
(define (sum-of-squares-of-two-largest a b c)
  (cond ((and (>= a c) (>= b c)) (+ (square a) (square b)))
        ((and (>= a b) (>= c b)) (+ (square a) (square c)))
        ((and (>= b a) (>= c a)) (+ (square b) (square c)))))

#|Exercise 1.4|#
(define (a-plus-abs-b a b) ;This procedure adds b to a if b is greater than zero. Otherwise, it will
  ((if (> b 0) + -) a b))  ;subtract b from a, which is essentially adding the absolute value of b to a.

#|Exercise 1.5|#
(define (p) (p))   ;With an applicative-order evaluation, (p) would be evaluated causing an infinite recursive loop, making sure
                   ;the program will never terminate.
(define (test x y) ;With a normal-order evaluation, the expression is expanded and x is substituted with 0, causing the predicate expression 
  (if (= x 0)      ;to evaluate to #t making the conditional expression evaluate to 0.
      0            
      y))

#|Exercise 1.6|#
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

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (= (abs (- (square guess) x)) 0.0)) ;Attempt to make epsilon/tolerance equal to 0.0

(define (sqrt x)
  (sqrt-iter 1.0 x))


; For very small inputs of x, the tolerance of 0.001 may allow the approximation
; to end prematurely early and give an inaccurate result.
; For very large numbers, the procedure make take a very long time to evaluate
; or possibly never finish if the input value is greater than the maximum
; floating point precision.
; (sqrt 1e+32) => 1e+16
; (sqrt 1e+64) => runs forever

#|Exercise 1.8|#

(define (cube-rt-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-rt-iter (cube-improve guess x) x)))

(define (cube-improve guess x)
  (average guess (/ (+ (/ x (* guess guess))
                       (* 2 guess))
                    3)))

;(define (average x y)
;  (/ (+ x y) 2))

(define (cube-rt x)
  (cube-rt-iter 1.0 x))

(define (cube-good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) (* 1.0e-12 x)))