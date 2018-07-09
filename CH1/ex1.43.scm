#lang sicp

; 1.43

(define (inc x) (+ 1 x))
(define (square x) (* x x))

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (lambda (x)
    (define (iter i result)
      (if (= i n)
          (result x)
          (iter (+ 1 i) (compose f result))))
    (iter 1 f)))

((repeated square 2) 5)

((repeated inc 10) 10)
