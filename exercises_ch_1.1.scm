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
(cond ((= a 4) 6)		   ; 16
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))	           ; 6
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
(define (p) (p))   ;With an applicative-order evaluation, the x is substituted with 0, causing the predicate expression
                   ;to evaluate to #t making the conditional expression evaluates to 0.
(define (test x y) ;With a normal-order evaluation, (p) would be evaluated causing an infinite recursive loop, making sure
  (if (= x 0)      ;the program will never terminate.
      0            
      y))