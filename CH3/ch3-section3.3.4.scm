#lang sicp

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
             front-queue)
            ((eq? m 'empty)
             empty-queue?)))
    dispatch))

(define (insert-queue! q value)
  ((q 'insert) value))

(define (delete-queue! q)
  ((q 'delete)))

(define (empty-queue? q)
  ((q 'empty)))

(define (front-queue q)
  ((q 'front)))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (valid-signal s)
  (if (not (or (= s 1) (= s 0)))
      #f
      #t))

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((and (valid-signal s1) (valid-signal s2)) 0)
        (else (error "Invalid signal" s1 s2))))

(define (logical-or s1 s2)
  (cond ((and (or (= s1 1) (= s2 1))
              (valid-signal s1)
              (valid-signal s2))
         1)
        ((and (valid-signal s1) (valid-signal s2)) 0)
        (else (error "Invalid signal" s1 s2))))

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))


(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))



(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (inverter input output)
  (define (inverter-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input inverter-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;; 3.28

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)


(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (probe name wire)
  (add-action! wire
               (lambda ()        
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire))
                 (newline))))


(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
(probe 'carry carry)

;; 3.29

(define (or-gate-2 a1 a2 output)
  (let ((a3 (make-wire))
        (a4 (make-wire))
        (a5 (make-wire)))
    (inverter a1 a3)
    (inverter a2 a4)
    (and-gate a3 a4 a5)
    (inverter a5 output)
    'ok))

;; 3.30

; the delay for is n * full-adder  which is n * (2 * half-adder + or)
; which is (2 * (or + and + inv + and) + or
; so n * (2 * (or + and + inv + and) + or)

(define (ripple-adder la lb ls c-in)
  (define (iter la lb ls c-in)
    (if (null? la)
        'ok
        (let ((c-out (make-wire)))
          (full-adder
           (car la)
           (car lb)
           c-in
           (car ls)
           c-out)
          (iter (cdr la)
                (cdr lb)
                (cdr ls)
                c-out))))
  (if (not (=
            (length la)
            (length lb)
            (length ls)))
      (error "Invalid input")
      (iter la lb ls c-in)))

; 3.31
; the original signals will not propagate through they system, due to the fact
; that after-delay which is called within the procedures which are added by add-action (accept-action-procedure!)_
; will not be executed before the signals are set
; the basic logic gates will work if you set the signals by hand, however, the adder's which make use
; of wires internal to the procedures will not work, due to the fact they aren't "connected"

; 3.32
; It's importante for the items to be run FIFO because the correct output is dependent upon the order
; that the operations are executed.
; In the given example, using a FIFO queue and a probe, you can actually see that the output is temporarily
; set to 1, going from 0,1 to 1,0
; if it were a LIFO stack in this case, the output would not temporarily go to 1

;(define (and-gate a1 a2 output)
;  (define (and-action-procedure)
;    (let ((new-value
;           (logical-and (get-signal a1) (get-signal a2))))
;      (after-delay and-gate-delay
;                   (lambda ()
;                     (set-signal! output new-value)))))
;  (add-action! a1 and-action-procedure)
;  (add-action! a2 and-action-procedure)
;  'ok)
