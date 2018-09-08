#lang sicp

(#%require "../Common/lib.scm")
(#%require "../Common/ch2support.scm")

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

;  (if (pair? datum)
;      (car datum)
;      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

;  (if (pair? datum)
;      (cdr datum)
;      (error "Bad tagged datum -- CONTENTS" datum)))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)
(install-polar-package)
;;footnote
;: (apply + (list 1 2 3 4))


;; Generic selectors

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


;; Constructors for complex numbers

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (install-integer-number-package)
  (define (tag x)
    (attach-tag 'integer x))

  (put 'raise '(integer)
       (lambda (x) (make-rational x 1)))

  (put 'exp '(integer integer)
       (lambda (x y) (tag (expt x y))))

  (put '=zero? '(integer)
       (lambda (x) (= 0 x)))

  (put 'equ '(integer integer)
       (lambda (x y) (eq? x y)))
  
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))

  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'integer
       (lambda (x) (tag x)))
  'done)

(install-integer-number-package)

(define (make-integer n)
  ((get 'make 'integer) (inexact->exact (round n))))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (put 'raise '(scheme-number)
       (lambda (x) (make-complex-from-real-imag x 0)))
  
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y)))) ; using primitive expt
  
  (put '=zero? '(scheme-number)
       (lambda (x) (= 0 x)))
  (put 'equ '(scheme-number scheme-number)
       (lambda (x y) (eq? x y)))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))

  (put 'project '(scheme-number)
       (lambda (x)
         (let ((rational (inexact->exact x)))
           (make-rational (numerator rational)
                          (denominator rational)))))
  'done)

(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))

  (put 'raise '(rational)
       (lambda (x) (make-scheme-number (/ (numer x) (denom x)))))
  
  (put '=zero? '(rational)
       (lambda (x) (= 0 (numer x))))
  (put 'equ '(rational rational)
       (lambda (x y)
         (and (eq? (numer x) (numer y))
              (eq? (denom x) (denom y)))))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  (put 'project '(rational)
       (lambda (x)
         (make-integer (round (/ (numer x) (denom x))))))
  
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put '=zero? '(complex)
       (lambda (z1) (and (= (real-part z1) 0) (= (imag-part z1) 0))))
  (put 'equ '(complex complex)
       (lambda (z1 z2)
         (and (= (inexact->exact (real-part z1)) (inexact->exact (real-part z2)))
              (= (inexact->exact (imag-part z1)) (inexact->exact (imag-part z2))))))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add3 '(complex complex complex)
       (lambda (z1 z2 z3)
         (add-complex z1 (add-complex z2 z3))))

  (put 'project '(complex)
       (lambda (x) (make-scheme-number (real-part x))))
  
  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (equ x y) (apply-generic 'equ x y))

(define (=zero? x) (apply-generic '=zero? x))
(define (exp x y) (apply-generic 'exp x y))

(define (add3 a b c) (apply-generic 'add3 a b c))

;(define (scheme-number->scheme-number n) n)
;(define (complex->complex z) z)
;(put-coercion 'scheme-number 'scheme-number
;              scheme-number->scheme-number)
;(put-coercion 'complex 'complex complex->complex)

;; 2.77
; This works because apply generic first resolves magnitude to the complex package,
; then, the corresponding function there, e.g., magnitude, receives as its argument
; without the leading 'complex type tag.  That argument is tagged with its corresponding
; type, either 'rectangular or 'polar.  Then, magnitude is called again, which once again
; goes through apply-generic.  However, this time it is resolved to either the rectangular
; or polar package to get the result.  Apply-generic is called twice

;;2.78
; implemented above with modifications to appropriate procedures

;; 2.79
;; done, but hard to do with the rounding issues of "inexact" numbers in scheme ...


;; 2.80
; implemented above

; 2.81
; a - this results in apply-generic hangining by calling itself repeatedly coercing the types
;     because proc isn't found, however it finds the coercion method and continues trying

; b - No, apply-generic works as is

; c - see above

; 2.82
;

(define (coerce type args result)
  (cond ((null? args) result)
        ((eq? (type-tag (car args)) type)
         (coerce type (cdr args) (append result (list (car args)))))
        (else
         (let ((ta->tb (get-coercion (type-tag (car args)) type)))
           (if ta->tb
               (coerce type (cdr args) (append (list (ta->tb (car args))) result)) 
               #f)))))

; commented out for 2.84

;(define (apply-generic op . args)
;  (define (iter coercion-types)
;    (if (null? coercion-types)
;        (error "No method for these types, and could not coerce"
;               (list op (map type-tag args)))
;        (let ((coerced-args (coerce (car coercion-types) args '())))
;          (if coerced-args
;              (let ((proc (get op (map type-tag coerced-args))))
;                (if proc
;                    (apply proc (map contents coerced-args))
;                    (iter (cdr coercion-types))))
;              (iter (cdr coercion-types))))))
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (iter type-tags)))))

; 2.83

; this asks us to desing a 'raise procedure for each type, but the book lists types we don't
; have packages for ...

; 

(define (raise x) (apply-generic 'raise x))


; 2.84

; this asks us to modify apply-generic so that it coerces the arguments to have the same type
; by successive raising.

(define hierarchy (list (list 10 'integer) (list 20 'rational) (list 30 'scheme-number) (list 40 'complex)))

(define (successive-raise args)
  (define (raise-arg x t)
    ; if x's index is less then t's index riase it
    (if (lower-than-type? (type-tag x) t)
        (raise-arg (raise x) t)
        x))
  (define (get-type-index t types)
    (if (eq? t (cadar types))
        (caar types)
        (get-type-index t (cdr types))))
  (define (lower-than-type? t1 t2)
    (if (< (get-type-index t1 hierarchy) (get-type-index t2 hierarchy))
        #t
        #f))
  (define (lower-or-equal-to-type? t1 t2)
    (cond ((<= (get-type-index t1 hierarchy) (get-type-index t2 hierarchy)) #t)
          (else #f)))
  (define (find-top-type remaining top-type)
    (cond ((null? remaining) top-type)
          ;; if the type is less then or equal to top type, top type remains
          ((lower-or-equal-to-type? (type-tag (car remaining)) top-type) (find-top-type (cdr remaining) top-type))
          (else (find-top-type (cdr remaining) (type-tag (car remaining))))))
  (define (raise-args args top-type)
    (cond ((null? args) '())
          (else
           (append (list (raise-arg (car args) top-type)) (raise-args (cdr args) top-type)))))
  (let ((top-type (find-top-type args (cadar hierarchy))))
    (raise-args args top-type)))

;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (let ((raised-args (successive-raise args)))
;            (let ((proc (get op (map type-tag raised-args))))
;              (if proc
;                  (apply proc (map contents raised-args))
;                  (error
;                   "No method for these types -- APPLY-GENERIC"
;                   (list op type-tags)))))))))

; 2.85
; I've added in an integer package for more examples here

; This solution seems inelegant, but it is necessary to avoid
; dropping all operations



(define (drop x)
  (let ((project (get 'project (list (type-tag x)))))
    (if project
        (let ((projected (project (contents x))))
          (if (equ projected x)
              (drop projected)
              x))
        x)))

(define drop-list (list 'add 'sub 'mul 'div '=zero? 'exp 'add3))

(define (should-not-drop? op)
  (define (should-not-drop-iter list)
    (if (null? list)
        #t
        (if (eq? op (car list))
            #f
            (should-not-drop-iter (cdr list)))))
  (should-not-drop-iter drop-list))

(define (apply-generic op . args)
  (let ((proc (get op (map type-tag args))))
    (if proc
        (let ((result (apply proc (map contents args))))
          (if (should-not-drop? op)
              result
              (drop result)))
        (apply apply-generic
               op
               (successive-raise args)))))

; 2.86
; We would need to define generic operations for scheme-numbers, integers, and rationals
; and use them in the polar and rectangular packages
; not going to implement this

