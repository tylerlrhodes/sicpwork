#lang sicp

; 2.25

(define l (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr l)))))

(define l2 (list (list 7)))
(car (car l2))

(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))
