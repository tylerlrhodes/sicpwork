#lang sicp

(#%require "../Common/lib.scm")

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point pt)
  (car pt))

(define (y-point pt)
  (cdr pt))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment segment)
  (let ((start-point (start-segment segment))
        (end-point (end-segment segment)))
    (make-point
     (average (x-point start-point) (x-point end-point))
     (average (y-point start-point) (y-point end-point)))))

(print-point
 (midpoint-segment
  (make-segment (make-point 1 1) (make-point 10 10))))


        