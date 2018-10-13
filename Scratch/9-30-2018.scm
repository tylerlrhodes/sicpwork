#lang sicp

(define (identity x) x)

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(flatmap
 (lambda (vals)
   (map
    (lambda (x)
      (append vals (list (list x))))
    (enumerate-interval 9 12)))
(flatmap
 (lambda (vals)
   (map
    (lambda (x)
      (append vals (list (list x))))
    (enumerate-interval 5 8)))
(flatmap
 (lambda (vals)
   (map
    (lambda (x)
      (append vals (list (list x))))
    (enumerate-interval 1 4)))
 (list '()))))


