#lang sicp

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (deep-reverse l)
  (define (iter l)
    (cond ((null? l) nil)
          ((pair? (car l))
           (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))
          (else (append (deep-reverse (cdr l)) (list (car l))))))
  (iter l))


