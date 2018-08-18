#lang sicp

(#%require "../Common/lib.scm")

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

; 2.63

(define (tree-list-1 tree)
  (if (null? tree)
      '()
      (append (tree-list-1 (left-branch tree))
              (cons (entry tree)
                    (tree-list-1 (right-branch tree))))))

; {4 {3 () ()} {5 () ()}}

;(tree-list-1 '(4 (3 () ()) (5 () ())))
;(append (tree-list-1 '(3 () ())) (cons 4 (tree-list-1 '(5 () ()))))
;(append (append '() (cons 3 '())) (cons 4 (tree-list-1 '(5 () ()))))
;(append (append '() (cons 3 '())) (cons 4 (append '() (cons 5 '()))))
;; 


(define (tree-list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


; {4 {3 () ()} {5 () ()}}

; copy to list '(3 () ()) (cons 4 (copy-to-list '(5 () ()) '()))

; (copy-to-list '(3 () ()) (cons 4 (copy-to-list '() (cons 5  (copy-to-list '() '()))))
; (copy-to-list '(3 () ()) (cons 4 ( cons 5 '())))
; (copy-to-list '(3 () ()) '(4 5))
; (copy-to-list '() (cons 3 (copy-to-list '() '(4 5)))
; (copy-to-list '() (cons 3 '(4 5)))
; (3 4 5)



(define t0 (adjoin-set 5 (adjoin-set 3 (adjoin-set 4 '()))))

(define t1 (adjoin-set 11 (adjoin-set 1 (adjoin-set 5 (adjoin-set 3 (adjoin-set 9 (adjoin-set 7 '())))))))
(define t2 (adjoin-set 11 (adjoin-set 9 (adjoin-set 5 (adjoin-set 7 (adjoin-set 1 (adjoin-set 3 '())))))))

(equal? (tree-list-1 t1) (tree-list-2 t1))
(equal? (tree-list-1 t2) (tree-list-2 t2))

; 2.63 - a - yes both procedures produce the same in-order list

; 2.63 - b - tree-list-2 grows more slowly then tree-list-1, O(N) vs O(NlogN) respectively due to
;            tree-list-1 using append which is O(N)



; 2.64

(define (list-tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


; 2.64 a
; partial-tree works by dividing the list into 3 parts, the left tree, middle, and right tree
; First it calls itself recursively to build the left half of the tree, then from the remaining
; elements it takes "this-entry", and then again calls itself recursively to build the right tree
; before putting them together.  The remaining elements construct is used to build the right tree and
; obtain "this-entry", calling partial-tree withan n less then the length of the list will leave the
; extra elements in remaining-elements for use later in the procedure

; 2.64 b
; O(N) - it visits each element once

;(partial-tree '(1 2 3 4 5) 5)
;
;	      left-size = 2
;	      left-result = (partial-tree '(1 2 3 4 5) 2)
;	      		  left-size = 0
;			  left-result = (partial-tree '(1 2 3 4 5) 0)
;			  left-result = '(() (1 2 3 4 5))
;			    left-tree = '()
;			    non-left-elts = '(1 2 3 4 5)
;			    right-size = 1
;			      this-entry = 1
;			      right-result = (partial-tree '(2 3 4 5) 1)
;			      		   left-size = 0
;					   left-result = '(() (2 3 4 5))
;					   left-tree = ()
;					   non-left-elts = (2 3 4 5)
;					   right-size = 0
;					     this-entry = 2
;					     right-result = (partial-tree '(3 4 5) 0)
;					     right-result = '(() (3 4 5))
;					       right-tree = ()
;					       remaining-elts = '(3 4 5)
;					       '((2 () ()) (3 4 5))
;                              right-result = '((2 () ()) (3 4 5))
;			        right-tree = (2 () ())
;				remaining-elts = '(3 4 5)
;				'((1 () (2 () ()))(3 4 5))
;	      left-result = '((1 () (2 () ()))(3 4 5))
;	        left-tree = (1 () (2 () ()))
;		non-left-elts = '(3 4 5)
;		right-size = 2
;		  this-entry = (3)
;		  right-result = (partial-tree '(4 5) 2)
;		  	       left-size = 0
;			       left-result = ('() (4 5))
;			       left-tree = '()
;			       non-left-elts = (4 5)
;			       right-size = 1
;			         this-entry = (4)
;				 right-result = (partial-tree '(5) 1)
;				 	      left-size = 0
;					      left-result = '(() (5))
;					        left-tree = '()
;						non-left-elts = (5)
;						right-size = 0
;						  this-entry = 5
;						  right-result = (partial-tree '() 0)
;						  right-result = '(())
;						    right-tree = '()
;						    remaining-elts = '()
;						    '(5 () ())
;			         right-result = '((5 () ()))
;				   right-tree = '(5 () ())
;				   remaining-elts = '()
;				   '((4 () (5 () ())))
;	          right-result = '((4 () (5 () ())))
;		    right-tree = (4 () (5 () ()))
;		    remaining-elts = '()
;		    '(3 (1 () (2 () ())) (4 () (5 () ())))

;(partial-tree '(1 2 3) 3)
;left-size = 1
;left-result = (partial-tree '(1 2 3) 1)
;                 left-size = 0
;                 left-result = (partial-tree '(1 2 3) 0)
;                 left-result = '('() '(1 2 3))
;                   left-tree = '()
;                   non-left-elts = '( 1 2 3)
;                   right-size = 0
;                     this-entry = 1
;                     right-result = (partial-tree '(2 3) 0)
;                     right-result = '('() '(2 3))
;                     right-tree = '()
;                     remaining-elts = '(2 3)
;                   '((1 () ()) '(2 3))
;left-tree = '(1 () ())
;non-left-elts = '(2 3)
;right-size = 1
;  this-entry = 2
;  right-result = (partial-tree '(3) 1)
;                  left-size = 0
;                  left-result = '(() '(3))
;                    left-tree = '()
;                    non-left-elts = '(3)
;                    right-size = 0
;                      this-entry = 3
;                      right-result = (partial-tree '() 0)
;                      right-result = '()
;                        right-tree = '()
;                        remaining-elts = '()
;                        '(((3 () ()) ())
;  right-result = '((3 () ()) '())
;    right-tree = '(3)
;    remaining-elts = '(() ())
;    ( '(2) (1 () ()) (3 () ()))


; 2.65

(define (intersection-set set1 set2)
  (define (intersection-set-list set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2)
                 (cons x1
                       (intersection-set-list (cdr set1)
                                         (cdr set2))))
                ((< x1 x2)
                 (intersection-set-list (cdr set1) set2))
                ((< x2 x1)
                 (intersection-set-list set1 (cdr set2)))))))
  (list-tree (intersection-set-list (tree-list-2 set1) (tree-list-2 set2))))


(define (union-set set1 set2)
  (define (union-set-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((< x1 x2)
                  (cons x1 (union-set-list (cdr set1) set2)))
                 ((> x1 x2)
                  (cons x2 (union-set-list set1 (cdr set2))))
                 ((= x1 x2)
                  (cons x1 (union-set-list (cdr set1) (cdr set2)))))))))
  (list-tree (union-set-list (tree-list-2 set1) (tree-list-2 set2))))