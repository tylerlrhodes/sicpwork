#lang sicp

(#%require "../Common/lib.scm")

(define (reverse-fr sequence)
  (fold-right (lambda (x y)
                (display "y: ")(display y)(newline)
                (display "x: ")(display x)(newline)
                (append y (list x))) nil sequence))

(define (reverse-fl sequence)
  (fold-left (lambda (x y)
               (display "y: ")(display y)(newline)
               (display "x: ")(display x)(newline)
               (cons y x)) nil sequence))

(display "reverse-fr")(newline)
(reverse-fr (list 1 2 3))

(display "reverse-fl")(newline)
(reverse-fl (list 1 2 3))