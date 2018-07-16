#lang sicp


; 2.18


(define (reverse l)
  (cond ((null? l) nil)
        (else (cons (reverse (cdr l)) (car l)))))

;(reverse (list 1 3 4 5))
;(cons (reverse (list 3 4 5)) (1)))
;(cons (cons (reverse (list 4 5)) (3)) (1)))
;(cons (cons (cons (reverse (list 5)) (4)) (3)) (1)))
;(cons (cons (cons (cons (reverse nil) (5)) (4)) (3)) (1)))
