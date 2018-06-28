#lang sicp

(#%require "../Common/lib.scm")

; http://www.billthelizard.com/2010/04/sicp-exercise-129-integration-using.html
;

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (inc x) (+ x 1))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((odd? k) 4)
             ((or (= k 0) (= k n)) 1)
             ((even? k) 2))
       (y k)))
  (/ (* h (sum term 0 inc n)) 3))