#lang sicp

;; 3.12

;; https://wizardbook.wordpress.com/2010/12/15/exercise-3-12/
;; this page shows a good picture

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

;z

;(cdr x)

(define w (append! x y))

;w

;(cdr x)

;; 3.13

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z2 (make-cycle (list 'a 'b 'c)))

; calling this with z2 will show it loops endlessly

(define (loop l)
  (if (null? l)
      (display "found the end...")
      (loop (cdr l))))


;; 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

; (mystery (list 'a 'b))
; (loop '(a b) '())
; (loop '(b) '(a ()))
; (loop '() '(b (a ())))
; '(b a)

; Mystery returns a list which is the reverse of x
; The result of mystery has a side-effect on x where it is a list containing only the first item

(define v (list 'a 'b 'c 'd))
(define w2 (mystery v))

;; 3.15


;; 3.16

(define (count-pairs-bad x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define l41 (list 'b 'c))
(define l42 (list 'a))
(set-car! l41 l42)
(set-car! (cdr l41) l42)


;; 3.17

;; As the book says, we need a data structure to keep track of the pairs
;; which have already been counted...

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;(define (count-pairs l)
;  (let ((seen '()))
;    (cond ((not (pair? l)) 0)
;          ((memq x seen) 0)
;          (else (set! seen (cons l seen))
;                (+ (count-pairs (car l))
;                   (count-pairs (cdr l))
;                   1)))))

; I messed this up originally -- it has to do with the environment model
; the version above creates a new seen variable on each iteration
; by using a lambda and a let it creates an environment with seen which
; keeps track of the seen pairs
; note its also possible to do this without using set! passing seen as an argument to count-pairs

(define count-pairs
  (let ((seen '()))
    (lambda (x)
      (cond ((not (pair? x)) 0)
            ((memq x seen) 0)
            (else (set! seen (cons x seen))
                  (+ (count-pairs (car x))
                     (count-pairs (cdr x))
                     1))))))

;; 3.18

(define l3 (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr l3) (cdr l3))

(define contains-cycle?
  (let ((seen '()))
    (lambda (x)
      (cond ((not (pair? x)) #f)
            ((memq x seen) #t)
            (else
             (set! seen (cons x seen))
             (contains-cycle? (cdr x)))))))


;; 3.19 -- redo 3.18 so it only requires constant space
;; turtle and rabbit algorithm

(define (contains-cycle2? l)
  (define (iter turtle hare)
    (cond ((or (null? turtle) (null? hare) (null? (cdr hare))) #f)
          ((eq? turtle hare) #t)
          (else
           (iter (cdr turtle) (cddr hare)))))
  (if (or (null? l) (null? (cdr l)))
      #f
      (iter l (cdr l))))





