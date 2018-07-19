#lang sicp

; 2.20

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (same-parity parity . nums)
  (define (check-vals vals nums)
    (cond ((null? nums) vals)
          ((even? parity)
           (if (even? (car nums)) (check-vals (append vals (list (car nums))) (cdr nums))))
          ((odd? parity)
           (if (odd? (car nums)) (check-vals (append vals (list (car nums))) (cdr nums))
               (check-vals vals (cdr nums))))))
  (check-vals (list parity) nums))


; another way
; http://www.billthelizard.com/2011/01/sicp-220-dotted-tail-notation.html
(define (same-parity2 a . z)
   (define (iter items answer)
     (if (null? items)
         answer
         (iter (cdr items)
               (if (= (remainder (car items) 2)
                      (remainder a 2))
                   (append answer (list (car items)))
                   answer))))
   (iter z (list a)))

; see https://github.com/abdulapopoola/SICPBook/blob/master/Chapter%202/2.2/Ex2.20.scm for another way too

