#lang sicp

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (even? x)
  (= (remainder x 2) 0))

(define (double x)
  (* 2 x))

(define (halve x)
  (/ x 2))


(define (*fi a b)
  (define (*f-iter product a b)
    (cond ((= b 0) product)
          ((even? b) (*f-iter product (double a) (halve b)))
          (else (*f-iter (+ product a) a (- b 1)))))
  (*f-iter 0 a b))

