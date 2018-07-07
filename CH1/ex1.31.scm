#lang sicp

(#%require "../Common/lib.scm")

; 1.31
; a

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; Wallis approximation of pie / wallis product
; https://en.wikipedia.org/wiki/Wallis_product

(define (wallis-product n i)
  (define (term x)
    (* (/ (* 2 x) (- (* 2 x) 1))
       (/ (* 2 x) (+ (* 2 x) 1))))
  (exact->inexact
   (* (product term n inc i)
     2)))

; b

(define (product-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))



