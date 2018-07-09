#lang sicp

(define (cont-frac n d k)
  (define (frac i)
    (if (< i k)
        (/ (n i) (+ (d i) (frac (+ i 1))))
        (/ (n i) (d i))))
  (frac 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)

(define (cont-frac-iter n d k)
  (define (frac-iter i result)
    (if (= 0 i)
        result
        (frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
  (frac-iter (- k 1) (/ (n k) (d k))))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)
