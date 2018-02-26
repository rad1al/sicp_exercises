#lang sicp

#|Exercise 1.9

(define (+ a b)
  (if (= a 0) 
      b 
      (inc (+ (dec a) b))))

this process is recursive:
> (+ 2 3)
(inc (+ 1 3))
(inc (inc (+ 0 3)))
(inc (inc 3))
(inc 4)
5

(define (+ a b)
  (if (= a 0) 
      b 
      (+ (dec a) (inc b))))

this process is iterative:
> (+ 2 3)
(+ 1 4)
(+ 0 5)
5

|#

#|Exercise 1.10|#

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

#|
> (A 1 0)
(A 0 (A 0 (A 0 ... (A 0 (A 1 1))...)))
(A 0 (A 0 (A 0 ... (A 0 2)...)))
(A 0 (A 0 (A 0 ... 4)))
1024

> (A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 1 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 4))
(A 1 16) ;2^16
65536

> (A 3 3)
(A 2 (A 3 2)
(A 2 (A 2 (A 3 1))
(A 2 (A 2 2))
(A 2 4)
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 4))
(A 1 16) ;2^16
65536

(define (f n) (A 0 n))   ;2n
(define (g n) (A 1 n))   ;2^n
(define (h n) (A 2 n))   ;2^2^2..^2 (n is the number of 2s)
(define (k n) (* 5 n n)) ;5n^2

|#

#|Exercise 1.11|#

; Recursion implementation:

;(define (f n)
;  (if (< n 3)
;        n
;  (+ (f (- n 1))
;     (* 2 (f (- n 2)))
;     (* 3 (f (- n 3))))))

;> (f 4)
;11
;> (f 25)
;812934961

;Iterative process/solution

(define (f n)
  (f-iter 0 1 2 n))

(define (f-iter a b c n)
  (if (= n 0)
      a
      (f-iter b
              c
              (+ c
                 (* 2 b)
                 (* 3 a))
              (- n 1))))

;> (f 4)
;(f-iter 0 1 2 4)  ;0,1,2 are the base cases for (f 0), (f 1), and (f 2)
;(f-iter 1 2 4 3)  ;move values to the left as n is decremented.
;(f-iter 2 4 11 2)
;(f-iter 4 11 25 1)
;(f-iter 11 25 59 0)
;11

#|Exercise 1.12|#

(define (pascal n x)
  (if (or (= x 1) (= x n))
      1
      (+ (pascal (- n 1) (- x 1))
         (pascal (- n 1) x))))

;> (pascal 4 2)
;(pascal 3 1) + (pascal 3 2)
;1 + (pascal 2 1) + (pascal 2 2)
;1 + 1 + 1
;3


#|Exercise 1.13

Prove that Fib(n) is the closest integer to phi^n/sqrt(5) where
phi = (1 + sqrt(5))/2. Hint let psi = (1 - sqrt(5))/2 to prove
Fib(n) = (phi^n - psi^n)/sqrt(5).

|#

#|Exercises 1.14

Draw the tree illustrating the process generated by the count-change
procedure of 1.2.2 in making change for 11 cents.

What are the orders of growth of the space and number of steps used by
this process as the amount to be changed increases?

|#

#|Exercise 1.15

The sine of an angle (specified in radians) can be computed by making
use of the approximation sinx ~ x if x is sufficiently small, and the
trigonometric identity sin(x) = 3sin(x/3) - 4sin^3(x/3) to reduce the
size of the argument of sin.

(For purposes of this exercise an angle is considered “sufficiently
small” if its magnitude is not greater than 0.1 radians.) These ideas
are incorporated in the following procedures:

|#

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;1. How many times is the procedure p applied when (sine 12.15) is evaluated?

;2. What is the order of growth in space and number of steps (as a function
;of a) used by the process generated by the sine procedure when (sine a)
;is evaluated?


#|Exercise 1.16|#

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b counter product)
  (cond ((= counter 0)
         product)
        ((even? counter)
         (fast-expt-iter (square b) (/ counter 2) product))
        (else
         (fast-expt-iter b (- counter 1) (* b product)))))
                      
(define (square x)
  (* x x))
  
#|Exercise 1.17|#

#|
(define (fast-multiply a b)
  (cond ((= b 0)
         0)
        ((even? b)
         (fast-multiply (double a) (halve b)))
        (else
         (+ a (fast-multiply a (- b 1))))))

|#

(define (double x)
  (+ x x))
  
(define (even? n)
  (= (remainder n 2) 0))

(define (halve x)
  (/ x 2))

#|Exercise 1.18|#

(define (fast-multiply a b)
  (fast-mult-iter a b 0))

(define (fast-mult-iter a b n)
  (cond ((= b 0)
         n)
        ((even? b)
         (fast-mult-iter (double a) (halve b) n))
        (else
         (fast-mult-iter a (- b 1) (+ a n)))))

#|Exercise 1.20|#

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

#|
; Applicative-order evaluation:


> (gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
2 ;There are 4 remainder operations.

; Normal-order evaluation:

> (gcd 206 40)

(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))

(if (= (remainder 206 40) 0) ; <- remainders are evaluated
    40
    (gcd (remainder 206 40)
         (remainder 40 (remainder 206 40))))

(if (= (remainder 40 (remainder 206 40)) 0) ; <- remainders are evaluated
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40))
         (remainder (remainder 206 40)
                    (remainder 40 (remainder 206 40)))))

(if (= (remainder (remainder 206 40) ; <- remainders are evaluated
                  (remainder 40 (remainder 206 40))) ; <- remainders are evaluated
       0)
    (gcd (remainder 40 (remainder 206 40) (remainder 40 (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))))

(if (= (remainder (remainder 40 (remainder 206 40)) ; <- remainders are evaluated
                  (remainder (remainder 206 40) ; <- remainders are evaluated
                             (remainder 40 (remainder 206 40)))) ; <- remainders are evaluated
       0)
    (remainder (remainder 206 40) ; <- remainders are evaluated
               (remainder 40 (remainder 206 40))) ; <- remainders are evaluated
    (gcd (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40)))
                    (remainder (remainder 40 (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206)))))))

; The remainder procedure is applied 18 times.
|#

#|Exercise 1.21|#

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
  (= n (smallest-divisor n)))

#|

> (smallest-divisor 199)
199
> (smallest-divisor 1999)
1999
> (smallest-divisor 19999)
7

|#

#|Exercise 1.22|#

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime)
                       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (cond ((even? start) (search-for-primes (+ start 1) end))
        ((<= start end)
         (timed-prime-test start)
         (search-for-primes (+ start 2) end))))

#|

> (search-for-primes 1000 1100)
1001
1003
1005
1007
1009 *** 2
1011
1013 *** 3
1015
1017
1019 *** 3

> (search-for-primes 10000 10100)
10001
10003
10005
10007 *** 6
10009 *** 6
10011
...
10035
10037 *** 6

> (search-for-primes 100000 100100)

100001
100003 *** 18
100005
...
100017
100019 *** 18
100021
...
100041
100043 *** 18


> (search-for-primes 1000000 1000100)
1000001
1000003 *** 58
1000005
...
1000031
1000033 *** 57
1000035
1000037 *** 57

; We can see that each time we multiply the input start number by
; 10, the average time it takes to check if the number in that vicinity
; increase by 3 to 3.2 times. Which is roughly the sqrt(10) confirming
; that this program runs in time proportional to the number of steps
; required for the computation. (Testing algorithm has order of growth of
; O(n))

|#

#|Exercise 1.25

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

; No, this program will generate a massive number '(fast-expt base exp)'
; and try to apply the remainder procedure to it, which would be very slow.

|#

#|Exercise 1.26

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (* (expmod base (/ exp 2) m)
             (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base 
             (expmod base (- exp 1) m))
          m))))

; The process is changed from O(log n) to O(n) because by using
; explicit multiplication, Louis will evaluate expmod twice (which
; will branch out into a binary tree) which removes the advantage we
; had with the original O(log n) algorithm, since 2^(log n) = n.

|#


#|Exercise 1.27|#

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
         m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))


(define (test-fermat n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (test-fermat-iter a)
    (if (< a n)
        (if (try-it a)
            (test-fermat-iter (+ a 1))
            #f)
        #t))
  (test-fermat-iter 1))

#|

> (test-fermat 17)
#t
> (test-fermat 561)
#t

|#

#|Exercise 1.28|#

(define (nontrivial-test x n)
  (if (and (not (or (= x 1)
                    (= x (- n 1))))
           (= 1 (remainder (square x) n)))
      0
      x))

#|
> (nontrivial-test 3 3)
3
> (nontrivial-test 25 3)
0
> (nontrivial-test 24 3)
24
> (nontrivial-test 23 3)
0
|#

(define (modified-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (nontrivial-test (modified-expmod base (/ exp 2) m) m))
          m))
        (else
         (remainder
          (* base (modified-expmod base (- exp 1) m))
          m))))

(define (miller-rabin-test n)
  (define (rand-a n)
    (+ 1 (random (- n 1))))
  (define (try-it a)
    (= (modified-expmod a (- n 1) n) 1))
  (define (iter a)
    (cond ((= 0 a)
           #t)
          ((try-it (rand-a n))
           (iter (- a 1)))
          (else
           #f)))
  (iter 20))

#|

> (miller-rabin-test 7)
#t
> (miller-rabin-test 13)
#t
> (miller-rabin-test 561)
#f ;This time it's correct.
> (miller-rabin-test 1105)
#f ;This is correct.
|#