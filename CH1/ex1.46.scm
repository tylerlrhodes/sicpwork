#lang sicp


(#%require "../Common/lib.scm")

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (iterative-improve good-enough? improve)
  (lambda (x)
    (define (try guess)
      (let ((next (improve guess)))
        (display next)(newline)
        (if (good-enough? next)
            next
            (try next))))
    (try x)))


(define (sqrt x)
  ((iterative-improve
   (lambda (a) (< (abs (- (square a) x)) 0.0001))
   (lambda (guess) (average guess (/ x guess) ))) 1.0))

(define (fixed-point f guess)
  ((iterative-improve
    (lambda (guess) (< (abs (- (f guess) guess)) 0.00001))
    (lambda (guess) (f guess)))
   guess))



