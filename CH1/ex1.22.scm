#lang sicp

(#%require "../Common/lib.scm")

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


; unfortunately runtime doens't give a good representation of the time required when I run it
;

(define (search-for-primes start stop)
  (if (even? start)
      (search-for-primes (+ start 1) stop))
  (if (< start stop)
      (begin
        (timed-prime-test start)
        (search-for-primes (+ 2 start) stop))))




