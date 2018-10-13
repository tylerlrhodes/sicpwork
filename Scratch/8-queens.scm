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


(flatmap identity (list (list 1 2 3) (list 4 5 6)))

;' 1 2 3 4 5 6

; (flatmap identity (list 1 2 3))
; results in an error - flatmap expects a sequence of sequences

(flatmap (lambda (i) (list i)) '(1 2 3))

; ' 1 2 3

(flatmap
 (lambda (i)
   (map (lambda (j) (list i j))
        (enumerate-interval 1 (- i 1))))
 (enumerate-interval 1 4))

; {{2 1} {3 1} {3 2} {4 1} {4 2} {4 3}}

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
(else (filter predicate (cdr sequence)))))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) 
                (- n 1))))

(define (safe k pos)
  (let ((kth-queen (list-ref pos (- k 1)))
        (others (filter (lambda (q) (not (= (cadr q) k))) pos)))
  (display kth-queen)
   (display others)
    (display pos)
    (newline)))

(filter
 (lambda (x) (safe 2 x))
 (flatmap
  (lambda (rest)
    (map
     (lambda (new-row)
       (append rest (list (list new-row 2))))
     (enumerate-interval 1 4)))
  
(filter
 (lambda (x) (safe 1 x))
(flatmap
 (lambda (rest)
   (map
    (lambda (new-row)
      (append rest (list (list new-row 1))))
      (enumerate-interval 1 4)))
 (list '())))))

 

