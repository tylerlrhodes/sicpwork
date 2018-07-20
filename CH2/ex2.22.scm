#lang sicp

(define (square x)
  (* x x))

; ex 2.22


; This attempt at a fix doesn't work becuase
; nil is the first element of the "list" which
; is displayed like:
; {{{() . 1} . 4} . 9}
; because it is really represented in Scheme as pairs since
; a proper list wasn't constructed
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))


; This doesn't work because the result is
; cons'd to the beginning of the list each time
; resulting in a reversed list -- it does produce a
; 'list' in Scheme, because it is cons'd in the correct
; order with nil at the end (a null list in effect)

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

