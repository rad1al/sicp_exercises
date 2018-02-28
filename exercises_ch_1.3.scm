#lang sicp

#|Exercise 1.29 - Simpson's Rule|#

#|
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
|#

(define (inc n) (+ n 1))

(define (cube x) (* x x x))

(define (simpson f a b n)
  (define h
    (/ (- b a) n))
  (define (yk k)
    (f (+ a (* k h))))
  (define (simpson-term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             (else 2))
       (yk k)))
  (* (/ h 3.0) (sum simpson-term 0 inc n)))

#|
> (simpson cube 0 1 100)
0.25
> (simpson cube 0 1 1000)
0.25
|#

#|Exercise 1.30|#

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))


#|Exercise 1.31|#

#|
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
|#

(define (identity x) x)

(define (factorial n)    
  (product identity 1 inc n))

(define (square x)
  (* x x))

(define (pi-product n)
  (define (pi-product-term k)
    (/ (* (* 2.0 k) (* 2.0 (+ k 1.0)))
       (square (+ (* 2.0 k) 1.0))))
  (product pi-product-term 1 inc n))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (inc a) (* (term a) result))))
  (iter a 1))

#|
> (pi-product 1000)
0.7855943412734714
|#

#|Exercise 1.32

; Recursive Version:

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
|#

; Iterative Version:

(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter a result)
    (if (> a b)
        result
        (accumulate-iter (next a) (combiner (term a) result))))
  (accumulate-iter a null-value))


(define (acc-sum a b)
  (accumulate + 0 identity a inc b))

(define (acc-product a b)
  (accumulate * 1 identity a inc b))

#|Exercise 1.33

; Recursive Version:

(define (filtered-accumulate combiner filter null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a)
                    (term a)
                    null-value)
                (filtered-accumulate combiner filter null-value term (next a) next b))))

; Iterative Version:

(define (filtered-accumulate combiner filter null-value term a next b)
  (define (filtered-accumulate-iter a result)
    (if (> a b)
        result
        (if (filter a)
            (filtered-accumulate-iter (next a) (combiner (term a) result))
            (filtered-accumulate-iter (next a) (combiner null-value result)))))
  (filtered-accumulate-iter a null-value))

|#

; Better Iterative Version:

(define (filtered-accumulate combiner filter null-value term a next b)
  (define (filtered-accumulate-iter a result)
    (if (> a b)
        result
        (filtered-accumulate-iter (next a)
                                  (combiner (if (filter a)
                                                 (term a)
                                                 null-value)
                                            result))))
  (filtered-accumulate-iter a null-value))


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
  (if (= n 1)
      #f
      (= n (smallest-divisor n))))

(define (sum-prime-squares a b)
  (filtered-accumulate + prime? 0 square a inc b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Product of numbers from 1 to n that are relatively prime to n:

(define (product-rel-prime n)
  (define (relatively-prime? a)
    (= 1 (gcd a n)))
  (filtered-accumulate * relatively-prime? 1 identity 1 inc (- n 1)))

#|
> (product-rel-prime 10)
189
|#

(define (f g) (g 2))

#|

> (f square)
4
> (f (lambda (z) (* z (+ z 1))))
6

|#

#|Exercise 1.34

If we ask the interpreter to evaluate the combination (f f), there
will be an error because f is supposed to take in a procedure as an
argument but we will end up applying f to 2 which is a number instead.

The following situation happens:

> (f f)
(f 2) ; 2 is not a procedure.
(2 2) ; the first 2 is not a procedure.

|#

#|Exercise 1.35

(define golden-ratio
  (fixed-point
   (lambda (x) (+ 1 (/ 1.0 x)))
   1.0))

; Using average damping:

(define golden-ratio
  (fixed-point
   (lambda (x) (average x (+ 1 (/ 1.0 x))))
   1.0))

> golden-ratio
1.6180311591702674

x -> 1 + 1/x
x = 1 + 1/x
x^2 = x + 1
x = +- (1 + sqrt(5))/2
x = (1 + sqrt(5))/2 ; Ignore negative root
|#

#|Exercise 1.36|#

(define (average a b)
  (/ (+ a b) 2))

(define tolerance 0.00001)

#|

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      ; (display guess)
      ; (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))




; Without average damping:

(define (x-to-the-x y)
  (fixed-point
   (lambda (x) (/ (log y) (log x)))
   10.0))

> (x-to-the-x 1000)
10.0
2.9999999999999996
6.2877098228681545
3.7570797902002955
5.218748919675316
4.1807977460633134
4.828902657081293
4.386936895811029
4.671722808746095
4.481109436117821
4.605567315585735
4.522955348093164
4.577201597629606
4.541325786357399
4.564940905198754
4.549347961475409
4.5596228442307565
4.552843114094703
4.55731263660315
4.554364381825887
4.556308401465587
4.555026226620339
4.55587174038325
4.555314115211184
4.555681847896976
4.555439330395129
4.555599264136406
4.555493789937456
4.555563347820309
4.555517475527901
4.555547727376273
4.555527776815261
4.555540933824255
4.555532257016376

This converges after 33 iterations.

|#

; With average damping:

(define (x-to-the-x y)
  (fixed-point
   (lambda (x) (average x (/ (log y) (log x))))
   10.0))

#|
> (x-to-the-x 1000)
10.0
6.5
5.095215099176933
4.668760681281611
4.57585730576714
4.559030116711325
4.55613168520593
4.555637206157649
4.55555298754564
4.555538647701617

This converges after 10 iterations.

|#

#|Exercise 1.37|#

#|

; Iterative solution where n and d are numbers

(define (cont-frac n d k)
  (define (cont-frac-iter term result)
    (if (= term 0)
        result
        (cont-frac-iter (dec term) (/ n (+ d result)))))
  (cont-frac-iter k 0.0))

> (cont-frac 1 2 2)
2/5

> (cont-frac 1 1 100)
0.6180339887498948

; Iterative solution where n and d are procedures

(define (cont-frac n d k)
  (define (cont-frac-iter term result)
    (if (= term 0)
        result
        (cont-frac-iter (dec term) (/ (n term) (+ (d term) result)))))
  (cont-frac-iter k 0.0))  

We need 11 iterations to get 4 correct decimal places:

> (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)
0.6179775280898876

> (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)
0.6180555555555556

> (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)
0.6180339887498948



; Recursive solution where n and d are procedures:

|#

(define (cont-frac n d k)
  (define (recur term)
    (if (= term k)
        (/ (n term) (d term))
        (/ (n term) (+ (d term) (recur (inc term))))))
  (recur 1))


#|Exercise 1.38|#

(define (euler k)
  (+ 2
     (cont-frac (lambda (i) 1)
                (lambda (i) (if (= (remainder i 3) 2)
                                (- i (floor (/ i 3.0)))
                                1))
                k)))

#|

> (euler 5)
0.271875
> (euler 100)
0.27182818284590453

|#


#|Exercise 1.39|#

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (- (square x))))
             (lambda (i) (- (* 2.0 i) 1))
             k))

#|

> (tan-cf 5 100)
-3.3805150062465867
> (tan 5)
-3.380515006246585

|#

#|Exercise 1.40|#

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

#|Exercise 1.41|#

(define (double f)
  (lambda (x) (f (f x))))

#|

> ((double square) 3)
81

> (((double (double double)) inc) 5)
(((double (lambda (x) (double (double x)))) inc) 5)
((double (double (double (double inc)))) 5)
((double (double (double (lambda (x) (inc (inc x)))))) 5)
(inc (inc (inc (inc (inc
(inc (inc (inc (inc (inc
(inc (inc (inc (inc (inc (inc 5)))))))))))))))) 
21

|#


#|Exercise 1.42|#

(define (compose f g)
  (lambda (x) (f (g x))))

#|
> ((compose square inc) 6)
49

|#

#|Exercise 1.43|#

(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

#|
> ((repeated square 2) 5)
625
> ((repeated inc 10) 5)
15
|#

#|Exercise 1.44|#

(define (average-of-three a b c)
  (/ (+ a b c) 3))


(define dx 0.00001)

(define (smooth f)
  (lambda (x) (average-of-three (f (- x dx))
                                (f x)
                                (f (+ x dx)))))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))

#|
> ((n-fold-smooth sin 10) (/ 3.1415926 6))
0.3748135547313751

> ((n-fold-smooth sin 20) (/ 3.1415926 6))
0.3081515797761997
|#

#|Exercise 1.45|#

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx))) ; dx defined earlier to be 0.00001.

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
       ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (fixed-point-of-transform
         g transform guess)
  (fixed-point (transform g) guess))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

#|

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))

(define (cube-root x)
  (fixed-point
   (average-damp
    (lambda (y)
      (/ x (square y))))
  1.0))


(define (nth-root n)
  (fixed-point-of-transform
   (lambda (y) (- (repeated (lambda (x) (* y x)) n) y x 2)))
|#

(define (inc-by n)
  (lambda (y) (+ y n)))

(define (mult-by n)
  (lambda (y) (* y n)))

#|

> ((inc-by 2) 10)
12

> ((mult-by 6) 10)
60

> ((repeated (mult-by 2) 3) 1)
8

> ((repeated (mult-by 3) 4) 1)
81

Using built-in expt procedure:

(define (nth-root x n)
  (fixed-point
   ((repeated average-damp (ceiling (/ (log n) (log 2))))
    (lambda (y) (/ x (expt y (- n 1))))) ; using expt
   1.0))

|#

; Using repeated to replicate the power function.

(define (nth-root x n)
  (fixed-point
   ((repeated average-damp (ceiling (/ (log n) (log 2))))
    (lambda (y) (/ x
                   ((repeated (mult-by y) (- n 1)) 1)))) 
   1.0))

#|

It seems to require the ceiling(log(n)/log(2)) of dampings (where n
is the nth-root we want to find) to avoid being stuck in a situation
where it does not terminate.

> (nth-root 81 4)
3.000000000000033

> (nth-root 100000 5)
10.000003715932646

> (nth-root 298023223876953125 25)
5.000000724021547

> (expt 5 25)
298023223876953125

|#


#|Exercise 1.46|#

(define (close-enough? v1 v2)
  (define tolerance 0.0000001)
  (< (/ (abs (- v1 v2)) v2) tolerance))

(define (iterative-improve good-enough? improve)
  (lambda (x) (let ((x-better (improve x)))
                (if (good-enough? x x-better)
                    x-better
                    ((iterative-improve close-enough? improve) x-better)))))

(define (sqrt x)
  ((iterative-improve
    close-enough?
    (lambda (y) (average (/ x y) y)))
   1.0))

(define (fixed-point f first-guess)
  ((iterative-improve
    close-enough?
    f)
   first-guess))

#|

> (x-to-the-x 1000)
4.55553558182498

> (fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)
1.258728219883364

> (fixed-point cos 1.0)
0.7390851084737986

|#

