#lang sicp
(#%require "../Common/lib.scm")

(define (fringe l)
  (define (iter l)
    (cond ((null? l) nil)
          ((pair? (car l)) (cons (fringe (car l)) (fringe (cdr l))))
          (else (cons (car l) (fringe (cdr l))))))
  (iter l))

(define x (list (list 1 2) (list 3 4)))

(fringe x)

(fringe (list x x))
