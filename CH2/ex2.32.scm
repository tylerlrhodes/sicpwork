#lang sicp

; 2.32


; This one was tricky, and I had trouble noticing what happened with the base case
; and the use of "let" threw me off a little
; However, with some judicious print statements I was able to see how this works
; There are a number of answers to this on the internet, but it requires
; some stepping through the solutions, because just staring at it takes too long sometimes
; 
(define (subsets s)
  (display s) (newline)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (display "rest:")(display rest)(newline)
        (append rest (map (lambda (x)
                            (display "car s:")(display (car s))(newline)
                            ;(display "x:")(display x)(newline)
                            (cons (car s) x)) rest)))))



(subsets (list 1 2))

;(1 2)
;(2)
;()
;rest:(())
;car s:2
;rest:(() (2))
;car s:1
;car s:1
;{() {2} {1} {1 2}}