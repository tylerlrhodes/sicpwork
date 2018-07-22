#lang sicp

(#%require "../Common/lib.scm")

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

;(horner-eval 1 (list 1 3 4))


; The recursion looks like this:

;(define x 1)

;(+ (* (+ (* (+ (* x 0) 4) x) 3) x) 1)


(horner-eval 1 (list 1 3 0 5 0 1))
