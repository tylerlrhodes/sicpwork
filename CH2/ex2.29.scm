#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((pair? mobile) (+ (total-weight (branch-structure (left-branch mobile)))
                           (total-weight (branch-structure (right-branch mobile)))))
        (else mobile)))


(define (branch-torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))


(define (balanced? mobile)
  (if (not (pair? mobile))
      true
      (and (= (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))

(define mob1 (make-mobile
              (make-branch 1 90)
              (make-branch 10
                           (make-mobile
                            (make-branch 1 6)
                            (make-branch 2 3)))))

(define a (make-mobile (make-branch 1 10) (make-branch 2 5)))

(total-weight mob1)
(branch-torque (left-branch mob1))

(balanced? mob1)

(balanced? (make-mobile (make-branch 1 mob1) (make-branch 1 mob1)))

(balanced? (make-mobile (make-branch 1 a) (make-branch 1 a)))

