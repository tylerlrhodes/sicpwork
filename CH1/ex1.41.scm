#lang sicp

; 1.41

(define (inc x) (+ 1 x))

(define (double f) (lambda (x) (f (f x))))

((double inc) 1)

; double of the double doubled
; 2 * 2 * 2 = double applied 8 times
; 
(((double (double double)) inc) 5)
