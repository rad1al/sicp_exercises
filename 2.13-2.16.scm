#lang sicp

(define (make-interval a b) (cons a b))

(define (upper-bound z)
  (max (car z)
       (cdr z)))

(define (lower-bound z)
  (min (car z)
       (cdr z)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (lower-bound y))
                               (- (upper-bound y)))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (= (upper-bound y)
         (lower-bound y))
      (error "Division by zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(define (make-center-percent c p)
  (let ((w (* c p 0.01)))
    (make-center-width c w)))


(define (percent i)
  (let ((w (width i))
        (c (center i)))
    (* 100.0 (/ w c))))

;; Depends on some preceding exercises

;; From the book
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))
;; ----------------

(define res3 (make-center-percent 39 0.05))
(define res4 (make-center-percent 56 0.05))

(center  (par1 res3 res4)) ; 23.22
(percent (par1 res3 res4)) ; 0.149 

(center  (par2 res3 res4)) ; 22.99 (center is closer to actual value)
(percent (par2 res3 res4)) ; 0.05  (par2 indeed makes tighter tolerances)

(center  (div-interval res3 res3)) ; 1.005  (should be exactly 1)
(percent (div-interval res3 res3)) ; 0.0998 (should be 0)

(center  (div-interval res3 res4)) ; 0.6999
(percent (div-interval res3 res4)) ; 0.0998 (dividing adds the uncertainties)

;; She is right. We saw in 2.14 that different operations propagate relative
;; tolerances differently - multiplication and division adds them, 
;; addition averages them and reciprocal preserves them. Subtraction usually
;; scales them up.

;; par1 has one multiplication, addition and division. We took the case where
;; both resistances have the same percentage tolerance, 5%. Multiplication
;; in numerator doubles the tolerance to 10%. Addition in denominator 
;; preserves tolerance at 5%. Dividing numerator by denominator adds the
;; corresponding tolerances, producing 15%. Evaluating par1 confirms this:
(percent (par1 res3 res4)) ; 0.149

;; par2 has 1 addition and 3 reciprocals that all preserve the tolerance at 5%:
(percent (par2 res3 res4)) ; 0.05

;; Thus, we should prefer additions and reciprocals to multiplications and 
;; divisions.

#|

22.989496673689956
0.1499999000000721
22.989473684210523
0.04999999999999694
1.0000005000001249
0.09999997500000216
0.6964289196429443
0.09999997500000014
0.1499999000000721
0.04999999999999694


|#