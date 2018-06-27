#lang sicp

;(#%require "../Common/lib.scm")

; http://www.billthelizard.com/2010/01/sicp-exercise-119-computing-fibonacci.html


(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))     ; compute p'
                   (+ (* 2 p q) (* q q))   ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
