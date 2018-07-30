#lang sicp

(#%require "../Common/lib.scm")

(define (ordered-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (triple-sum? t s)
  (= (+ (car t) (cadr t) (caddr t)) s))

(define (sum-triple-pairs? n s)
  (filter (lambda (x) (triple-sum? x s))
          (ordered-triples n)))