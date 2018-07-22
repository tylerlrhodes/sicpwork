
#lang sicp

(#%provide (all-defined))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (even? x)
  (= (remainder x 2) 0))

(define (double x)
  (* 2 x))

(define (halve x)
  (/ x 2))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (define (next test-divisor)
    (if (= 2 test-divisor)
        (+ 1 test-divisor)
        (+ 2 test-divisor)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
; (expmod 2 4 3)
; (remainder (square (expmod 2 2 3)) 3)
; (remainder (square (square (expmod 2 1 3))) 3)
; (remainder (square (square (remainder (* 2 (expmod 2 0 3)) 3))) 3)
; (remainder (square (square (remainder (* 2 (1)) 3))) 3)
; (remainder (square (square (remainder 2 3))) 3)
; (remainder (square (square 2)) 3)
; (remainder (square (4)) 3)
; (remainder 16 3)
; (1)


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (identity x) x)

(define (inc x) (+ 1 x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (average a b)
  (/ (+ a b)
     2))

(define (mapc proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse items)
 (if (null? items)
     items
     (append (reverse (cdr items)) (list (car items)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


