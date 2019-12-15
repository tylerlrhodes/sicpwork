(ns clj-sicp.ch3)

(defn test
  []
  (println "Test works..."))





(defn sicp-cons
  [x y]
  (fn [m] (m x y)))

(defn sicp-car
  [z]
  (z (fn [p q] p)))

(defn sicp-cdr
  [z]
  (z (fn [p q] q)))


(def s (sicp-cons 1 2))

(sicp-car s)
(sicp-cdr s)

(defn cons-stream
  [a b]
  (sicp-cons a
             (delay b)))

(defn car-stream
  [z]
  (sicp-car z))

(defn cdr-stream
  [z]
  (force (sicp-cdr z)))

(defn stream-enumerate-interval
  [low high]
  (if (> low high)
    nil
    (cons-stream
     low
     (stream-enumerate-interval (+ 1 low) high))))

(defn stream-for-each
  [stream]
  (if (= nil stream)
    nil
    (do
      (println (car-stream stream))
      (recur (cdr-stream stream)))))

(defn stream-ref
  [stream n]
  (if (= n 0)
    (car-stream stream)
    (recur (cdr-stream stream) (- n 1))))

(defn stream-map
  [proc stream]
  (if (= nil stream)
    nil
    (cons-stream
     (proc (car-stream stream))
     (stream-map (cdr-stream stream) proc))))

(defn stream-filter
  [pred stream]
  (if (= nil stream)
    nil
    (if (pred (car-stream stream))
      (cons-stream
       (car-stream stream)
       (stream-filter pred (cdr-stream stream)))
      (stream-filter
       pred
       (cdr-stream stream)))))

;; Implement stream-map to accept multiple streams (ie work as Clojure's map does)

;; do 3.50 - 3.52


