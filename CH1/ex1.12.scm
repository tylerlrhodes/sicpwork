#lang sicp

;; ex 1.12

#|
think of it this way with rows and columns
1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
|#

(define (pascal x y)
  (cond ((= y 1) 1)
        ((= y x) 1)
        (else (+ (pascal (- x 1) (- y 1)) (pascal (- x 1) y)))))




