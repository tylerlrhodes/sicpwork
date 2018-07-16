#lang sicp

; 2.17

(define (last-pair l)
  (cond ((null? l) nil)
        ((null? (cdr l)) (car l))
        (else (last-pair (cdr l)))))

