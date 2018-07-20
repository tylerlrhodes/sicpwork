#lang sicp

(#%require "../Common/lib.scm")

; ex 2.21

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-m items)
  (map square (list 1 2 3 4)))
