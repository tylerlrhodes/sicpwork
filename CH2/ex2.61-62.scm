#lang sicp

(#%require "../Common/lib.scm")

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;Exercise 2.61.  Give an implementation of adjoin-set using the ordered representation.
;By analogy with element-of-set? show how to take advantage of the ordering to produce a procedure that
;requires on the average about half as many steps as with the unordered representation.

; adjoin-set
; if the set is null, cons x to it
; rebuild the set inserting x to the left of the first element it's less then

(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((< x (car set))
         (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))


; Exercise 2.62.  Give a O(n) implementation of union-set for sets represented as ordered lists.

; we have two ordered sets that we want to get the union of
; if one of the sets is null return the other set
; if (car set1) is less then (car set2) cons (car set1) to the union of (cdr set1) and set2
; if (car set1) is greater then (car set2) cons (car set2) to the union of set1 and (cdr set2)
; if (car set1) ='s (car set2) cons (car set1) to the union of (cdr set1) and (cdr set2)

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 ((> x1 x2)
                  (cons x2 (union-set set1 (cdr set2))))
                 ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2)))))))))


  