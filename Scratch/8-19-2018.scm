#lang sicp

(define (append0 list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append0 (cdr list1) list2))))

(append0 '(1 2 3) '(4 5 6))
; {1 2 3 4 5 6}

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (append list1 list2)
  (accumulate cons list2 list1))

(append '(1 2 3) '(4 5 6))
; {1 2 3 4 5 6}

(define (map p seq)
  (accumulate (lambda (x y) (cons (p x) y)) '() seq))

(define (length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

(define fold-right accumulate)

(fold-right / 1 '(2))
; 2

(fold-right (lambda (x y) (append y (list x))) '() '(1 2 3))
; 3 2 1

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-left / 1 '(2))
; 1/2

(fold-left (lambda (x y) (cons y x)) '() '(1 2 3))
; 3 2 1

