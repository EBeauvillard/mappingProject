#lang racket
(require "graph_interface.rkt")
(require racket/trace)
(provide optimal-distance)

;;;computes the distance beetween 2 vertices v1, v2 and with R radius of the earth (in km) with the Haversine formula
(define (optimal-distance R v1 v2)
  (* R (c v1 v2)))

;;;computes a part of the Haversine formula (c)
(define (c v1 v2)
  (*
   2
   (atan
    (sqrt (a v1 v2))
    (sqrt (- 1 (a v1 v2))))))
  
;;; computes a part of the Haversine formula (a)
(define (a v1 v2)
  (+ (a1 v1 v2) (a2 v1 v2)))

(define (a1 v1 v2)
  (sqr
   (sin
    (/
     (delta
      (get-lat-vertex v1)
      (get-lat-vertex v2))
     2))))

;;; computes a part of a (a2)
(define (a2 v1 v2)
  (*
   (cos (degrees->radians (get-lat-vertex v1)))
   (cos (degrees->radians (get-lat-vertex v2)))
   (sqr
    (sin
     (/
      (delta
       (get-long-vertex v1)
       (get-long-vertex v2))
      2)))))  

;;; computes the delta of el1 and el2 (the difference)
(define (delta el1 el2)
  (degrees->radians (- el2 el1)))

;;; computes an example distance beetween 2 specific points
(define (test)
  (optimal-distance 6371 (vertex-new 1 40.7486 -73.9864) (vertex-new 2 45.0 0)))

;(trace a1 a2 a c optimal-distance delta)
;distance between around New York (40.7486;-73.9864) and around Bordeaux (45.0;0)
;should be around 5833 km according to google maps
;(test)