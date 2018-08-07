#lang sicp

; 2.44, 2.45, 2.46, 2.47, 2.48, 2.49, 2.50
;

(#%require sicp-pict)

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


; 2.45

(define (split t1 t2)
  (define (transform painter n)
    (if (= n 0)
        painter
        (let ((smaller (transform painter (- n 1))))
          (t1 painter (t2 smaller smaller)))))
  transform)


; 2.46

;(define (make-vect x y)
;  (cons x y))
;
;(define (xcor-vect v)
;  (car v))
;
;(define (ycor-vect v)
;  (cdr v))
;
;(define (add-vect v1 v2)
;  (make-vect
;   (+ (xcor-vect v1) (xcor-vect v2))
;   (+ (ycor-vect v1) (ycor-vect v2))))
;
;(define (sub-vect v1 v2)
;  (make-vect
;   (- (xcor-vect v1) (xcor-vect v2))
;   (- (ycor-vect v1) (ycor-vect v2))))
;
;(define (scale-vect v s)
;  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

; 2.47

;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))
;
;(define (origin-frame frame)
;  (car frame))
;
;(define (edge1-frame frame)
;  (cadr frame))
;
;(define (edge2-frame frame)
;  (caddr frame))

;(define (make-frame2 origin edge1 edge2)
;  (cons origin (cons edge1 edge2)))
;
;(define (origin-frame2 frame)
;  (car frame))
;
;(define (edge1-frame2 frame)
;  (car (cdr frame)))
;
;(define (edge2-frame2 frame)
;  (cdr (cdr frame)))


(define (frame-coord-map frame)
  (lambda (v)
    (vector-add
     (frame-origin frame)
     (vector-add (vector-scale (vector-xcor v)
                               (frame-edge1 frame))
                 (vector-scale (vector-ycor v)
                               (frame-edge2 frame))))))


; 2.48

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))


; 2.49

(define outline-segments
  (list
   (make-segment
    (make-vect 0.0 0.0)
    (make-vect 0.0 0.99))
   (make-segment
    (make-vect 0.0 0.0)
    (make-vect .99 0.0))
   (make-segment
    (make-vect .99 0.0)
    (make-vect .99 .99))
   (make-segment
    (make-vect 0.0 .99)
    (make-vect .99 .99))))

(define outline (segments->painter outline-segments))

(define x-segments
  (list
   (make-segment
    (make-vect 0.0 0.0)
    (make-vect .99 .99))
   (make-segment
    (make-vect 0.0 .99)
    (make-vect .99 0.0))))

(define x-painter (segments->painter x-segments))

(define diamond-segments
  (list
   (make-segment
    (make-vect 0.0 0.5)
    (make-vect 0.5 0.0))
   (make-segment
    (make-vect 0.0 0.5)
    (make-vect 0.5 0.999))
   (make-segment
    (make-vect 0.5 0.999)
    (make-vect 0.999 0.5))
   (make-segment
    (make-vect 0.999 0.5)
    (make-vect 0.5 0.0))))

(define diamond (segments->painter diamond-segments))

; shamelessly copied from http://www.billthelizard.com/2011/10/sicp-249-defining-primitive-painters.html

(define wave-segments
  (list
   (make-segment
    (make-vect 0.006 0.840)
    (make-vect 0.155 0.591))
   (make-segment
    (make-vect 0.006 0.635)
    (make-vect 0.155 0.392))
   (make-segment
    (make-vect 0.304 0.646)
    (make-vect 0.155 0.591))
   (make-segment
    (make-vect 0.298 0.591)
    (make-vect 0.155 0.392))
   (make-segment
    (make-vect 0.304 0.646)
    (make-vect 0.403 0.646))
   (make-segment
    (make-vect 0.298 0.591)
    (make-vect 0.354 0.492))
   (make-segment
    (make-vect 0.403 0.646)
    (make-vect 0.348 0.845))
   (make-segment
    (make-vect 0.354 0.492)
    (make-vect 0.249 0.000))
   (make-segment
    (make-vect 0.403 0.000)
    (make-vect 0.502 0.293))
   (make-segment
    (make-vect 0.502 0.293)
    (make-vect 0.602 0.000))
   (make-segment
    (make-vect 0.348 0.845)
    (make-vect 0.403 0.999))
   (make-segment
    (make-vect 0.602 0.999)
    (make-vect 0.652 0.845))
   (make-segment
    (make-vect 0.652 0.845)
    (make-vect 0.602 0.646))
   (make-segment
    (make-vect 0.602 0.646)
    (make-vect 0.751 0.646))
   (make-segment
    (make-vect 0.751 0.646)
    (make-vect 0.999 0.343))
   (make-segment
    (make-vect 0.751 0.000)
    (make-vect 0.597 0.442))
   (make-segment
    (make-vect 0.597 0.442)
    (make-vect 0.999 0.144))))

(define wave (segments->painter wave-segments))


; 2.50

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (vector-sub (m corner1) new-origin)
                     (vector-sub (m corner2) new-origin)))))))


(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate-180 painter)
   ((transform-painter (make-vect 1.0 1.0)
                       (make-vect 0.0 1.0)
                       (make-vect 1.0 0.0))
    painter))

(define (rotate-270 painter)
   ((transform-painter (make-vect 0.0 1.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0))
    painter))




