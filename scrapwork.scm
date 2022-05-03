#lang sicp

(define (add a b)
  (+ a b))

; (display (add 5 7))

#|

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

|#

(define one-through-four (list 1 2 3 4))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

; 2.17
(define (last-pair list)
  (if (< (length list) 2)
      list
      (last-pair (cdr list))))

; (display (last-pair (list 23 72 149 34)))

#|
(define (reverse list)
  (if (< (length list) 2)
      list
      (append (reverse (cdr list)) (cons (car list) '()))))

|#

(define (reverse list)
  (if (null? list)
      list
      (append (reverse (cdr list)) (cons (car list) '()))))

; (display (reverse (list 1 4 9 16 25)))

; (display (reverse (list 1)))


; Exercise 2.20
(define (same-parity first . rest)
  (define (find-parity x y)
    (if (null? y)
        '()
        (if (even? (+ x (car y)))
            (cons (car y) (find-parity x (cdr y)))
            (find-parity x (cdr y)))))
  (cons first (find-parity first rest)))


; (display (same-parity 1 2 3 4 5 6 7))

; Exercise 2.21

(define (square x)
  (* x x))

#|
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))


(define (square-list items)
  (map square items))

|#

#|  
(display (square-list (list 1 2 3 4)))

(define (display-ln x)
  (display x)
  (newline))
|#




; Exercise 2.22

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer (list (square (car things)))))))
  (iter items nil))

(display (square-list (list 1 2 3 4)))
(newline)

; Exercise 2.23


(define (for-each proc items)
  (cond ((null? items) #t)
        (else (proc (car items))
              (for-each proc (cdr items)))))

#|

(for-each 
 (lambda (x) (display (* 2 (+ 5 x)))
             (newline))
 (list 1 10 100))

|#

; Exercise 2.27

(define x
  (list (list 1 2) (list 3 4)))


;(display (reverse x))
;(newline)

#|

(define (deep-reverse items)
  (cond ((null? items) nil)
        ((pair? (car items))
         (append (deep-reverse (cdr items))
                 (list (deep-reverse (car items)))))
        (else (append (deep-reverse (cdr items))
                      (list (car items))))))

|#


(define (deep-reverse t) 
   (if (pair? t) 
       (reverse (map deep-reverse t)) 
       t))

;(display (deep-reverse x))
;(newline)

; Exercise 2.28
(define (fringe tree)
  (cond ((null? tree) nil)
        ((pair? (car tree))
         (append (fringe (car tree)) (fringe (cdr tree))))
        (else (cons (car tree)
                    (fringe (cdr tree))))))


(display (fringe x))
(newline)

(display (fringe '((1 2) (3 4) (5 6 (7 8) 9))))