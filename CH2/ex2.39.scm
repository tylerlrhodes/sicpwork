#lang sicp

(#%require "../Common/lib.scm")

(define (reverse-fr sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-fl sequence)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))