#lang sicp

(#%require sicp-pict)

;(define wave2 (beside einstein (flip-vert einstein)))


(#%require "../Common/lib.scm")

(define (permutations s)
  (if (null? s)                    ; empty set?
      (list nil)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(permutations (list 1 2))

(cons 1 (permutations (list 2)))
(cons 1 (cons 2 (permutations '())))

(cons 2 (permutations (list 1)))
(cons 2 (cons 1 (permutations nil)))


(permutations (list 1 2 3))
(cons 1 (permutations (list 2 3)))
(cons 1 (cons 2 (permutations (list 3))))
(cons 1 (cons 2 (cons 3 (permutations '()))))
(cons 1 (cons 3 (permutations (list 2))))
(cons 1 (cons 3 (cons 2 (permutations '()))))

(cons 2 (permutations (list 1 3)))
(cons 2 (cons 1 (permutations (list 3))))
(cons 2 (cons 1 (cons 3 (permutations '()))))
(cons 2 (cons 3 (permutations (list 1))))
(cons 2 (cons 3 (cons 2 (permutations '()))))

(cons 3 (permutations (list 1 2)))
(cons 3 (cons 1 (permutations (list 2))))
(cons 3 (cons 1 (cons 2 (permutations '()))))
(cons 3 (cons 2 (permutations (list 1))))
(cons 3 (cons 2 (cons 1 (permutations '()))))



