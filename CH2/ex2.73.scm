#lang sicp


(#%require "../Common/ch2support.scm")

;; ex 2.73

;a
; Deriv was written to make use of a genric package for applying operators
; number? cannot be used in the data directed dispatch because if 'exp' is in fact a number, how
; would we dispatch on it?  We would need another general dispatching abstraction of some kind since
; it can be any number
; same-variable? similarly cannot be dispatched on because it could be any symbol

;b

(define (install-deriv-package)
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  
  (define (make-sum a1 a2) (list '+ a1 a2))

  (define (make-product m1 m2) (list '* m1 m2))

  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))

  (define (addend s) (car s))
  (define (augend s) (cadr s))

  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))

  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))

  (define (make-exponentiation b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e)) (expt b e))
          (else (list '** b e))))

  (define (base e) (car e))
  (define (exponent e) (cadr e))
  
  (put 'deriv '+ (lambda (exp var) (make-sum (deriv (addend exp) var) (deriv (augend exp) var))))
  (put 'deriv '* (lambda (exp var) (make-sum
                                    (make-product (multiplier exp)
                                                  (deriv (multiplicand exp) var))
                                    (make-product (deriv (multiplier exp) var)
                                                  (multiplicand exp)))))
  (put 'deriv '** (lambda (exp var) (make-product
                                     (exponent exp)
                                     (make-product
                                      (make-exponentiation 
                                       (base exp) 
                                       (- (exponent exp) 1))
                                      (deriv (base exp) var))))))

(install-deriv-package)
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


;c
; integrated above

;d

; the first two arguments to put need to be exchanged

