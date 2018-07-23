#lang sicp

(#%require "../Common/lib.scm")

(define (reverse-fr sequence)
  (fold-right (lambda (x y)
                (display y)(newline)
                (display "x: ")(display x)(newline)
                (append y (list x))) nil sequence))

(define (reverse-fl sequence)
  (fold-left (lambda (x y)
               (display y)(newline)
               (display "x: ")(display x)(newline)
               (cons y x)) nil sequence))