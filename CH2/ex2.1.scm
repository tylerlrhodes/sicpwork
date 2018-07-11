#lang sicp

; 2.1

(#%require "../Common/lib.scm")

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))   
    (if (or (< n 0) (< d 0))
        (if (and (< n 0) (< d 0))
            (cons (/ (abs n) g) (/ (abs d) g))         
            (let
                ((num
                  (if (> n 0)
                      (* n -1)
                      n))
                 (den
                  (if (< d 0)
                      (* d -1)
                      d)))
              (cons (/ num g) (/ den g))))
        (cons (/ n g) (/ d g)))))
