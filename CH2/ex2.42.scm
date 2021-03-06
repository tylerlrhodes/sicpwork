#lang sicp

(#%require "../Common/lib.scm")


; http://www.billthelizard.com/2011/06/sicp-242-243-n-queens-problem.html
;

(define (make-position row col)
  (cons row col))
(define (position-row position)
  (car position))
(define (position-col position)
  (cdr position))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (make-position new-row k))))

(define (safe? col positions)
  (let ((kth-queen (list-ref positions (- col 1)))
        (other-queens (filter (lambda (q)
                                (not (= col (position-col q))))
                              positions)))
    (define (attacks? q1 q2)
      (or (= (position-row q1) (position-row q2))
          (= (abs (- (position-row q1) (position-row q2)))
             (abs (- (position-col q1) (position-col q2))))))

    (define (iter q board)
      (or (null? board)
          (and (not (attacks? q (car board)))
               (iter q (cdr board)))))

    (iter kth-queen other-queens)))
        

  
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define l '())
(flatmap (lambda (r) (map (lambda (new-row) (append (list new-row) r)) (enumerate-interval 1 3))) (list 1 2))
