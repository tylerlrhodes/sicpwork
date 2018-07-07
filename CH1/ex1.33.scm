#lang sicp

(#%require "../Common/lib.scm")

; 1.33
; a

(define (filtered-accumulate combiner null-value term a next b filter)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a)
                              (filtered-accumulate combiner null-value term (next a) next b filter)))
        (else (filtered-accumulate combiner null-value term (next a) next b filter))))

(filtered-accumulate + 0 square 2 inc 10 prime?)

; b
;
; http://www.billthelizard.com/2010/05/sicp-exercise-133-filtered-accumulator.html
; checked here to see how he handled the less then n part of the exercies, just bundled it up with (- n 1)
; when kicking it off -- easy way works
;

(define (product-of-coprimes n)
  (define (coprime? i)
    (= 1 (gcd i n)))
  (filtered-accumulate * 1 identity 1 inc (- n 1) coprime?))

