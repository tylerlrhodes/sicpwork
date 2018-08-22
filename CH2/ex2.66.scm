#lang sicp

; 2.66

(define (lookup given-key records)
  (cond ((null? records) #f)
        ((equal? given-key (key (entry records)))
         (entry records))
        ((< x (key (entry records)))
         (lookup given-key (left-branch records)))
        ((> x (key (entry records)))
         (lookup given-key (right-branch records)))))