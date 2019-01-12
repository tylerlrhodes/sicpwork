#lang racket
(require rnrs/mutable-pairs-6)


;(define (parallel-execute . procs)
;  (map thread-wait
;       (map (lambda (proc) (thread proc))
;            procs)))

(define (parallel-execute . thunks)
  (for-each thread thunks))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)
                 ('()))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (set-car! cell false))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))


; 3.38

; a
; 100 - 110 - 90 - 45
; 100 - 50 - 30 - 40
; 100 - 80 - 90 - 45

; b

; seet https://wizardbook.wordpress.com/2010/12/18/exercise-3-38/ for a good diagram of interleaving


; 3.39

;121 p2 -> p1
;101 p1 -> p2
;11 - p1 evaluates serialized (* x x) where x is 10, p2 evaluates setting getting value 11, before setting x, p1 sets x to 100, then p2 sets x to 11
;100 - p1 evalutes serizlized (* x x) where x is 10, p2 evaluates setting getting value 11 and sets x to 11, p1 sets x to 100

; 3.40

; a
; 100, 1000
; 1000 * 10
; 100 * 10 * 10
; 100 * 100 * 10
; 100 * 100 * 100
; 10 * 1000

; b
; 1,000,000


; 3.41

; It would depend upon the requirements of the system.  Without serializing access to the balance,
; it would be possible to see the balance while a transaction is being processed which may or may not be correct



; 3.42
; this should be fine and doesn't appear to introduce any differences

; 3.43

; 3.44

; this method doesn't appear to have a problem.  The exchange problem is different because the account balances
; can interleave when they aren't serialized.  Here, the amount will be withdran and then depositied and the operations
; don't share the account balances between them like the exchange problem

; 3.45

; deadlock


; 3.46
; this is basically the same as the other serialization problems

; 3.47




(define (test)
  (define (iter n)
    (let ((x 10))
      (parallel-execute (lambda () (set! x (+ x 1)))
                        (lambda () (set! x (* x x))))
      (sleep 1)
      (display x)
      (newline)
      (if (> n 0)
          (iter (- n 1))
          '())))
  (iter 10))



