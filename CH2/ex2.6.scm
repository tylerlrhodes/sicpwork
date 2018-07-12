#lang sicp

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


(add-1 zero)

(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))

(lambda (f) (lambda (x) (f x)))

(define one
  (lambda (f) (lambda (x) (f x))))

(add-1 one)

(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (add-church m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(add-church one one)

(define (inc x) (+ 1 x))

(((add-church one one) inc) 0)





