#lang racket
(require "xml_open.rkt" "graph_interface.rkt" "optimal_distance.rkt")
(require racket/runtime-path)
(provide get-lid-vertex get-llon-vertex get-llat-vertex get-lid-edge
         get-lref-edge-src get-lref-edge-dest get-lref-edge cons-graph
         get-bounds)


;;;;;;;;;;;;;;;Parsing part;;;;;;;;;;;;;;;;;

;;; returns the list of all the vertices of the map
(define (get-vertex data)
  (remove* '(way) (cdddr data) (lambda (x y) (and (list? y) (equal? x (car y))))))

;;; returns the list of all the edges of the map
(define (get-edge data)
  (remove* '(node) (cdddr data) (lambda (x y) (and (list? y) (equal? x (car y))))))

;;; returns the list of all the id of the vertices of the map
(define (get-lid-vertex data)
  (map string->number (map car (map cdaadr (get-vertex data)))))

;;; returns the list of all the longitudes of the vertices of the map
(define (get-llon-vertex data)
  (map string->number (map cadar (map cddadr (get-vertex data)))))

;;; returns the list of all the latitudes of the vertices of the map
(define (get-llat-vertex data)
  (map string->number (map cadr (map cadadr (get-vertex data)))))

;;; returns the list of all the id of the edges of the map
(define (get-lid-edge data)
  (map string->number (map car (map cdaadr (get-edge data)))))

;;; returns a list of all the sources of ref of the edge
(define (get-lref-edge-src data)
  (map string->number (map cadr (map caadar (map cddr (get-edge data))))))

;;; returns a list of all the destinations of ref of the edges
(define (get-lref-edge-dest data)
  (map string->number (map car (map cdaadr (map cadddr (get-edge data))))))

;;; returns a list of all the sources and destinations of the edges as dotted pairs
(define (get-lref-edge data)
  (map cons (get-lref-edge-src data) (get-lref-edge-dest data)))

;;; returns a list of bounds
(define (get-bounds-l data)
  (caddr data))

;;; returns the maximum latitude 
(define (get-maxlat data)
  (string->number (car (cdaadr (get-bounds-l data)))))

;;; returns the maximum longitude
(define (get-maxlon data)
  (string->number (cadr (cadadr (get-bounds-l data)))))

;;; returns the minimum latitude
(define (get-minlat data)
  (string->number (cadar (cddadr (get-bounds-l data)))))

;;; returns the minimum longitude
(define (get-minlon data)
  (string->number (cadadr (cddadr (get-bounds-l data)))))

;;; returns the list of bounds : max lat, max lon, min lat, min lon
(define (get-bounds str)
  (list (get-maxlat (data-argument str)) (get-maxlon (data-argument str)) (get-minlat (data-argument str)) (get-minlon (data-argument str))))


;;;;;;;;;;;;;;;;;Graph Part;;;;;;;;;;;;;;;;;;;

;;; returns the list of vertices corresponding to the list of IDs
(define (fvbIl lid lv)
  (if (null? lid)
      '()
      (cons
       (find-vertex-by-ID (car lid) lv)
       (fvbIl (cdr lid) lv))))

;;; returns the weight beetween 2 vertices (for Dijkstra, the distance) 

(define (get-weight v1 v2)
  (optimal-distance 6371 v1 v2))

;;; returns the list of vertices for the graph
(define (list-vertex sgraph)
  (map vertex-new
       (get-lid-vertex sgraph)
       (get-llat-vertex sgraph)
       (get-llon-vertex sgraph)))

;;; returns the list of edges for the graph
(define (list-edge sgraph lv)
  (let ([lsrc (fvbIl (get-lref-edge-src sgraph) lv)]
        [ldest (fvbIl (get-lref-edge-dest sgraph) lv)])
    (map edge-new
         (get-lid-edge sgraph)
         lsrc
         ldest
         (map get-weight lsrc ldest))))

;;; construct the graph of the .osm file located with the path "str"
(define (cons-graph str)
  (let ([sgraph (data-argument str)])
    (let ([lv (list-vertex sgraph)])
      (let ([le (list-edge sgraph lv)])
        (foldl add-edge (foldl add-vertex (graph-new) lv) le)))))
