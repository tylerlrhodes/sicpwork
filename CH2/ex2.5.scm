#lang sicp

; 2.5

; http://www.billthelizard.com/2010/10/sicp-25-representing-pairs-as-product.html

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (num-divs n d)
  (define (iter x result)
    (if (= 0 (remainder x d))
        (iter (/ x d) (+ 1 result))
        result))
  (iter n 0))

(define (car z)
  (num-divs z 2))

(define (cdr z)
  (num-divs z 3))

(car (cons 4 7))

(cdr (cons 4 7))

