#lang sicp

;; ex 1.8
;; (x/y^2 + 2y) / 3


(define (cube-iter guess x)
  (display guess)
  (newline)
  (if (good-enough? guess (improve guess x))
      guess
      (cube-iter (improve guess x)
                 x)))

(define (improve guess x)
  (/
   (+ (/ x (square guess)) (* 2 guess))
   3))


;
; implement this so that it checks if the change is a very small fraction of the guess
; then the guess is "good enough"
;
(define (good-enough? currentGuess nextGuess)
  (< (/ (abs (- currentGuess nextGuess)) nextGuess)
     (* 1.0e-10 currentGuess)))

(define (square x)
  (* x x))

(define (cube-root x)
  (cube-iter 1.0 x))

