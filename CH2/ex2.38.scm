#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;(fold-right / 1 (list 1 2 3))
;(fold-left / 1 (list 1 2 3))
;(fold-right list nil (list 1 2 3))
;(fold-left list nil (list 1 2 3))


(fold-right * 2 (list 1 2 3))
(fold-left * 2 (list 1 2 3))

(fold-right + 2 (list 1 2 3))
(fold-left + 2 (list 1 2 3))


; commutative property of op should be that changing the order of the factors does not change the result
; associative property of op should be that changing the grouping of the factors does not change the result

