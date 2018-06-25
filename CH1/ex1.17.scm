#lang sicp

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (even? x)
  (= (remainder x 2) 0))

(define (double x)
  (* 2 x))

(define (halve x)
  (/ x 2))

(define (*f a b)
  (cond ((= b 0) 0)
        ((even? b) (*f (double a) (halve b)))
        (else (+ a (*f a (- b 1))))))


#|

(*f 2 5)
(+ 2 (*f 2 4))
(+ 2 (*f 4 2))
(+ 2 (*f 8 1))
(+ 2 (+ 8 (*f 8 0)))
(+ 2 (+ 8 (+ 0)))

10

(*f 2 6)
(*f 4 3)
(+ 4 (*f 4 2))
(+ 4 (*f 8 1))
(+ 4 (+ 8 (*f 8 0)))
(+ 4 (+ 8 (+ 0)))

12

|#