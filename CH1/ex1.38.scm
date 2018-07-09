#lang sicp

; 1.38

(define (cont-frac-iter n d k)
  (define (frac-iter i result)
    (if (= 0 i)
        result
        (frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
  (frac-iter (- k 1) (/ (n k) (d k))))

(define (d i)
  (if (not (= 0 (remainder (+ i 1) 3)))
      1
      (* 2 (/ (+ i 1) 3))))

(define e
  (+ 2 (cont-frac-iter (lambda (i) 1.0) d 10)))


; 1.39

(define (square x) (* x x))

(define (tan-cf x k)
   (define (n k)
       (if (= k 1)
           x
           (- (square x))))
   (define (d k)
       (- (* 2 k) 1))
   (cont-frac-iter n d k))

; http://www.billthelizard.com/2010/07/sicp-137-138-and-139-continued.html
; for a good explanation


