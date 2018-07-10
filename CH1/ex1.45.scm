#lang sicp

(#%require "../Common/lib.scm")

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (lambda (x)
    (define (iter i result)
      (if (= i n)
          (result x)
          (iter (+ 1 i) (compose f result))))
    (iter 1 f)))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))


;(define (nth-root x n)
;  (fixed-point
;     (average-damp
;        (lambda (y) (/ x (expt y (- n 1)))))
;     1.0))

; http://www.billthelizard.com/2010/08/sicp-145-computing-nth-roots.html
; I was too lazy to do the experimenting, he describes nicely how to figure out how many
; times to repeat the average dampening
;

(define (log2 x)
  (/ (log x) (log 2)))

(define (nth-root x n)
  (fixed-point
      ((repeated average-damp (floor (log2 n)))
          (lambda (y) (/ x (expt y (- n 1)))))
      1.0))

