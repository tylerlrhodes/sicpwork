#lang sicp


; 1.44

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

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/
     (+ (f x)
        (f (- x dx))
        (f (+ x dx)))
     3)))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))

