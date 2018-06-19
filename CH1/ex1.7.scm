#lang sicp

;; ex 1.8

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;
; implement this so that it checks if the change is a very small fraction of the guess
; then the guess is "good enough"
;
(define (good-enough? currentGuess nextGuess)
  (< (/ (abs (- currentGuess nextGuess)) nextGuess)
     1.0e-20))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

