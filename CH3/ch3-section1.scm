#lang sicp

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;; 3.1

(define (make-accumulator sum)
  (lambda (amount)
    (begin (set! sum (+ sum amount))
           sum)))

;; 3.2

(define (make-monitored f)
  (let ((calls 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) calls)
            ((eq? arg 'reset-count)
             (set! calls 0))
            (else
             (begin (set! calls (+ 1 calls))
                    (f arg)))))))

;; 3.3

(define (make-account2 balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m supplied-password)
    (if (eq? supplied-password password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (error "Incorrect Password")))
  dispatch)

;; 3.4

(define (make-account3 balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficent funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (define (call-the-cops)
    (error "The Police have been called!!"))
  (define dispatch
    (let ((tries 0))
      (lambda (m supplied-password)
        (if (eq? supplied-password password)
          (cond ((eq? m 'withdraw)
                 (set! tries 0)
                 withdraw)
                ((eq? m 'deposit)
                 (set! tries 0)
                 deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m)))
          (if (>= tries 2)
              (call-the-cops)
              (begin (set! tries (+ 1 tries))
                     (error "Incorrect Password")))))))
  dispatch)


        
    
