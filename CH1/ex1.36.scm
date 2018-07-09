#lang sicp

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess num-guesses)
    (display num-guesses) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ 1 num-guesses)))))
  (try first-guess 1))

; without average dampening
; 34 guesses
;
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

; with average dampening
; 9 guesses
;
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)



