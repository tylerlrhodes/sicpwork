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
    (cond ((or (null? turtle)
               (null? hare)
               (null? (cdr hare)))
           #f)
          ((eq? turtle hare) #t)
          (else
           (iter (cdr turtle) (cddr hare)))))
  (if (or (null? l) (null? (cdr l)))
      #f
      (iter l (cdr l))))


;; 3.22

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT alled with an empty queue" front-ptr)
          (car front-ptr)))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" front-ptr))
            (else
             (set! front-ptr (cdr front-ptr))
             front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr)
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr))))
    (define (dispatch m)
      (cond ((eq? m 'insert)
             insert-queue!)
            ((eq? m 'delete)
             delete-queue!)
            ((eq? m 'front)
             front-queue)))
    dispatch))


;; 3.23


(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-deque)
      (if (empty-queue?)
          (error "error..")
          (caar front-ptr)))
    (define (rear-deque)
      (if (empty-queue?)
          (error "error...")
          (caar rear-ptr)))
    (define (front-delete-deque!)
      (if (empty-queue?)
          (error "error...")
          (begin
            (set! front-ptr (cdr front-ptr))
            (if (null? front-ptr)
                (set! rear-ptr '())
                (set-cdr! (car front-ptr) '()))
            front-ptr)))
    (define (rear-delete-deque!)
      (if (empty-queue?)
          (error "error...")
          (begin
            (set! rear-ptr (cdar rear-ptr))
            (if (null? rear-ptr)
                (set! front-ptr '())
                (set-cdr! rear-ptr '()))
            front-ptr)))
    (define (front-insert-deque! item)
      (let ((new-pair (cons (cons item '()) front-ptr)))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr)
              (else
               (set-cdr! (car front-ptr) new-pair)
               (set! front-ptr new-pair)
               front-ptr))))
    (define (rear-insert-deque! item)
      (let ((new-pair (cons (cons item rear-ptr) '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr)
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr))))
    (define (dispatch m)
      (cond ((eq? m 'front-insert) front-insert-deque!)
            ((eq? m 'rear-insert) rear-insert-deque!)
            ((eq? m 'front) front-deque)
            ((eq? m 'rear) rear-deque)
            ((eq? m 'rear-delete) rear-delete-deque!)
            ((eq? m 'front-delete) front-delete-deque!)))
    dispatch))



;; 3.24

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (list key-1
                                        (cons key-2 value)
                                        (cdr subtable))))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "unknown operation -- TABLE" m))))
    dispatch))

(define op-table (make-table equal?))
(define get (op-table 'lookup-proc))
(define put (op-table 'insert-proc))


;; 3.25


(define (make-table-v)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((or (null? records) (not (pair? records))) #f)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (get-val record)
      (cond ((null? record) #f)
            ((not (pair? record)) record)
            (else (get-val (cdr record)))))
    (define (lookup keylist table)
      (if (null? keylist)
          '()
          (if (null? (cdr keylist))
              (let ((record 
                     (assoc (car keylist) (cdr table))))
                (if record (get-val (cdr record)) #f))
              (let ((subtable 
                     (assoc (car keylist) (cdr table))))
                (if subtable
                    (lookup (cdr keylist) subtable)
                    #f)))))
    (define (lookup-helper . keys)
      (lookup keys local-table))
    (define (insert! val . keys)
      (define (iter keys subtable)
        (if (null? keys)
            #f
            (if (null? (cdr keys))
                (let ((record (assoc (car keys) (cdr subtable))))
                  (if record
                      (set-cdr! record val)
                      (set-cdr! subtable
                                (cons (cons (car keys) val)
                                      (cdr subtable)))))
                (let ((sub (assoc (car keys) (cdr subtable))))
                  (if sub
                      (iter (cdr keys) sub)
                      (begin
                        (set-cdr! subtable
                                  (cons (list (car keys))
                                        (cdr subtable)))
                        (iter keys subtable)))))))
      (iter keys local-table)
      local-table)
    (define (dispatch m)
      (cond ((eq? m 'insert) insert!)
            ((eq? m 'lookup) lookup-helper)
            ))
    dispatch))


;; 3.26

(define (make-table-bst)
  (list '*table*))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cadr record)
        false)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((eq? key (car records)) car records)
        ((< key (car records)) (assoc key (left-branch records)))
        (else (assoc key (right-branch records)))))

(define (insert! key value table)
  (set-cdr! table (insert-record! key value (cdr table))))

(define (insert-record! key value node)
  (let ((record (assoc key node)))
    (if record
        (begin (set-car! (cdr record) value) node)      
        (cond ((null? node)
               (make-tree key value '() '()))
              ((< key (node-key node))
               (make-tree (node-key node) (node-val node) (insert-record! key value (left-branch node)) (right-branch node)))
              (else
               (make-tree (node-key node) (node-val node) (left-branch node) (insert-record! key value (right-branch node))))))))

(define (make-tree node val left right)
  (list node val left right))
(define (node-key node)
  (car node))
(define (node-val node)
  (cadr node))
(define (left-branch node)
  (caddr node))
(define (right-branch node)
  (cadddr node))



