#lang sicp

(#%require "../Common/lib.scm")

; ex2.35

(define (count-leaves t)
  (accumulate
   (lambda (x y) (+ x y))
   0
   (map
    (lambda (sub-tree)
      (cond ((null? sub-tree) 0)
            ((pair? sub-tree)
             (+ (count-leaves (list (car sub-tree)))
                (count-leaves (list (cdr sub-tree)))))
            (else 1)))
    t)))

(count-leaves (list 3 4))

(count-leaves (list 3 4 5))

(count-leaves (list (list 3 4) (list 2 5)))

(count-leaves (list 1 2))

(count-leaves (list (list 1 2 3) (list 4 5 6 (list 7 8))))

(count-leaves (list (list (list 1)) (list 2)))


; So the above solution works, but as usual searching the internet has revealed
; more elegant solutions


; http://www.billthelizard.com/2011/04/sicp-235-counting-leaves-of-tree.html
; Bill's solution is the simplest to understand, and arguably the one that makes the most sense
; as it uses the patterns described immediately before this exercise
;
; There was also another solution, shown below from the comments on Bill's blog by Mike Campo
;

(define (count-leaves-2 t)
  (accumulate + 0 (map (lambda (x)
                         (if (pair? x)
                             (count-leaves x)
                             1))
                       t)))

(count-leaves-2 (list (list (list 1)) (list 2 3)))