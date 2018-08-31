#lang sicp

(#%require "../Common/ch2support.scm")

;; 2.74

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

; a

(define (get-record emp file)
  ((get 'get-record (type-tag file)) emp (contents file)))

; b

(define (get-salary record)
  ((get 'get-salary (type-tag record)) (contents record)))

; the record should be tagged with the type


; c

(define (find-employee-record emp files)
  (cond ((null? files) #f)
        (else (let ((rec (get-record emp (car files))))
                (if rec
                    rec
                    (find-employee-record emp (cdr files))))))))


; d

; their system must install a package which appropriatley types their records and files
; and defines the appropriate procedures







