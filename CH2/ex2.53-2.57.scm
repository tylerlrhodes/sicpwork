#lang sicp


(#%require "../Common/lib.scm")


(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))


; 2.53
; what does the following print?

(list 'a 'b 'c)

; (a b c)

(list (list 'george))

;((george))


(cdr '((x1 x2) (y1 y2)))

; ((y1 y2))

(cadr '((x1 x2) (y1 y2)))

; (y1 y2)


(pair? (car '(a short list)))

; #f

(memq 'red '((red shoes) (blue socks)))

; #f

(memq 'red '(red shoes blue socks))

; (red shoes blue socks)


; 2.54

(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((and (and (pair? a) (pair? b))
              (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))) #t)
        ((or (pair? a) (pair? b)) #f)
        (else (eq? a b))))

(equal? '(a b) '(c d))
(equal? '(a b) '(a b))
(equal? '(a (b c)) '(a (b c)))
(equal? '(a (b c)) '(a (b d)))

; 2.55, 2.56, 2.57

(car ''abracadabra)

; this prints out 'quote' because the ' is the car of the list

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))
             

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
           (exponent exp)
           (make-product
             (make-exponentiation 
               (base exp) 
               (- (exponent exp) 1))
             (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))


(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(* x y (+ x 3)) 'x)

; 2.58 - not doing this one, would require something like pratt parsing


