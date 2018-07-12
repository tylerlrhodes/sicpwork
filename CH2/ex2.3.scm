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

; point with width and height
(define (make-rect pt width height)
  (cons pt (cons width height)))

; two points
(define (make-rect2 a b)
  (cons a
        (cons 
         (abs (- (x-point a) (x-point b)))
         (abs (- (y-point a) (y-point b))))))

(define (height-rect rect)
  (cdr (cdr rect)))

(define (width-rect rect)
  (car (cdr rect)))

(define (area-rect rect)
  (* (height-rect rect) (width-rect rect)))

(define (perimeter-rect rect)
  (+
   (* 2 (height-rect rect))
   (* 2 (width-rect rect))))