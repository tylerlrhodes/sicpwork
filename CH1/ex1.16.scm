#lang sicp

#|

ex 1.16

b^n = (b^(n/2))^2    -- if n is even
b^n = b * b^(n-1)    -- if n is odd

|#


(define (square x)
  (* x x))

; slow implementation recursive

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

; slow implementation iterative

(define (expt-i b n)
  (define (expt-iter b n product)
    (cond ((= n 0) product)
          (else (expt-iter b (- n 1) (* product b)))))
  (expt-iter b n 1))

; fast O(log n) recursive implementation

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


; fast O(log n) iterative
; This way actually works but i should not have increased the value of the accumlator
; when n was even (this way I don't need to keep an extra b variable for the base)

(define (fast-expt-i b n)
  (define (fast-expt-iter #|b|# bn n a)
    (cond ((= n 0) a)
          ;((= n 1) (* a b))
          ((even? n) (fast-expt-iter #|b|# (square bn) (/ n 2) a));(* a bn)))
          (else (fast-expt-iter #|b|# bn (- n 1) (* a bn)))))
  (fast-expt-iter #|b|# b n 1))


(define f fast-expt-i)



;; an easier to read implementation
;; from https://github.com/abdulapopoola/SICPBook/blob/master/Chapter%201/1.2/Ex1.16.scm

(define (fast-expt2 b n)
	(define (fast-expt-helper a b n)
		(cond ((= n 0) a)
			   ((even? n) (fast-expt-helper a (square b) (/ n 2)))
			   ((odd? n)  (fast-expt-helper (* a b) b (- n 1)))))
	(fast-expt-helper 1 b n))






  