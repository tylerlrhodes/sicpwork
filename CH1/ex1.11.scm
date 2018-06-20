#lang sicp


(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))


;; (f 5)  // if we don't multiply by 2 and 3 anyway the results
; (+ (f 4) (f 3) (f 2))
; (+ (f 4) 3 2)
; (+ (+ (f 3) (f 2) (f 1)) 3 2)
; (+ (+ 3 2 1) 3 2)
; (+ 6 5)
; (11)

;; iterative process
#|
if n < 3, return n
a = 2 // n - 1
b = 1 // n - 2
c = 0 // n - 3
for _ in 3..n
  a = a + 2 * b + 3 * c           // 2 + 2 + 0, 4 + 4 + 3, 11 + 8 + 6
  b = olda                        // 2, 4, 11
  c = oldb                        // 1, 2, 4
return a
|#




(define (f-iter a b c count n)
  (display a) (newline)
    (if (> count n)
        a
        (f-iter (+ a (* 2 b) (* 3 c)) a b (+ count 1) n)))

(define (f2 n)
  (if (< n 3)
      n
      (f-iter 2 1 0 3 n)))




