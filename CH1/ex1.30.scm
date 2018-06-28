#lang sicp


(#%require "../Common/lib.scm")


(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (identity x) x)

(define (inc x) (+ 1 x))

(sum identity 1 inc 10)


(define (cube x) (* x x x))

(sum cube 1 inc 10)

