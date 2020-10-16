#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "../src/graph_interface.rkt" "../src/xml_open.rkt")
(provide graph-interface-tests)

;;(***********************************)
;;(****** TESTS GRAPH-INTERFACE ******)
;;(***********************************)
(define v0 (vertex-new 0 0 0))
(define v1 (vertex-new 1 1 1))
(define v2 (vertex-new 2 2 2))
(define v3 (vertex-new 3 3 3))
(define v4 (vertex-new 4 4 4))
(define e0 (edge-new 100 v0 v1 10))
(define e1 (edge-new 101 v1 v3 1))
(define e2 (edge-new 102 v2 v3 5))
(define e3 (edge-new 103 v3 v2 8))

(define g0 (add-edge e2
            (add-edge e1
             (add-edge e0
              (add-vertex v3
               (add-vertex v2
                (add-vertex v1
                 (add-vertex v0
                  (graph-new)))))))))

(define v5 (vertex-new 5 5 5))
(define v6 (vertex-new 6 6 6))
(define v7 (vertex-new 7 7 7))
(define v8 (vertex-new 8 8 8))
(define e4 (edge-new 104 v5 v7 1))
(define e5 (edge-new 105 v7 v5 1))
(define e6 (edge-new 106 v7 v8 1))
(define e7 (edge-new 107 v8 v7 1))
(define e8 (edge-new 108 v8 v6 1))
(define e9 (edge-new 109 v5 v6 1))
(define g1 (add-edge e9
            (add-edge e8
             (add-edge e7
              (add-edge e6
               (add-edge e5
                (add-edge e4
                  (add-vertex v8
                   (add-vertex v7
                    (add-vertex v6
                     (add-vertex v5
                      (graph-new))))))))))))

(define e10 (edge-new 110 v2 v1 1))
(define e11 (edge-new 111 v1 v3 1))
(define e12 (edge-new 112 v3 v4 1))
(define e13 (edge-new 113 v3 v5 1))
(define e14 (edge-new 114 v4 v5 1))
(define e15 (edge-new 115 v5 v4 1))
(define e16 (edge-new 116 v4 v6 1))
(define e17 (edge-new 117 v6 v4 1))
(define e18 (edge-new 118 v5 v6 1))
(define e19 (edge-new 119 v6 v5 1))
(define e20 (edge-new 120 v6 v7 1))
(define e21 (edge-new 121 v5 v7 1))
(define e22 (edge-new 122 v1 v2 1))
(define g2 (add-edge e22
            (add-edge e21
             (add-edge e20
              (add-edge e19
               (add-edge e18
                (add-edge e17
                 (add-edge e16
                  (add-edge e15
                   (add-edge e14
                    (add-edge e13
                     (add-edge e12
                      (add-edge e11
                       (add-edge e10
                        (add-vertex v7
                         (add-vertex v6
                          (add-vertex v5
                           (add-vertex v4
                            (add-vertex v3
                             (add-vertex v2
                              (add-vertex v1
                               (graph-new))))))))))))))))))))))

(define e23 (edge-new 123 v5 v7 3))
(define e24 (edge-new 124 v5 v6 2))
(define g3 (add-edge e24
            (add-edge e20
             (add-edge e19
              (add-edge e23
               (add-edge e17
                (add-edge e16
                 (add-edge e15
                  (add-edge e14
                   (add-edge e13
                    (add-edge e12
                     (add-edge e11
                      (add-edge e10
                       (add-edge e9
                        (add-vertex v7
                         (add-vertex v6
                          (add-vertex v5
                           (add-vertex v4
                            (add-vertex v3
                             (add-vertex v2
                              (add-vertex v1
                               (graph-new))))))))))))))))))))))

(define list-v0 (get-list-vert g0))
(define list-e0 (get-list-edge g0))
(define list-v1 (get-list-vert g1))
(define list-e1 (get-list-edge g1))

(define (graph-interface-tests)
  (test-case                                                          
    "get-ID-vertex: Returns the ID of the first vertex"                                          
    (check-equal? (get-ID-vertex (car list-v0)) 0))

   (test-case                                                          
    "get-ID-vertex: Returns the ID of the second vertex"                                          
    (check-equal? (get-ID-vertex (cadr list-v0)) 1))
   
   (test-case                                                          
    "get-ID-edge: Returns the ID of the first edge"                                          
    (check-equal? (get-ID-edge (car list-e0)) 100))

   (test-case                                                                                                   
    "get-ID-edge: Returns the ID of the second edge"                                          
    (check-equal? (get-ID-edge (cadr list-e0)) 101))

   (test-case                                                          
    "get-vertex-degree-out: Returns degree of the first outter edge of the first vertex"                                          
    (check-equal? (get-vertex-degree-out list-e0 v0) 1))

   (test-case                                                          
    "get-vertex-degree-out: Returns degree of the first outter edge of the second vertex"                                          
    (check-equal? (get-vertex-degree-out list-e0 v1) 1))

   (test-case                                                          
    "get-vertex-degree-out: Returns degree of the first outter edge of the third vertex"                                          
    (check-equal? (get-vertex-degree-out list-e0 v2) 1)) 

   (test-case                                                          
    "get-vertex-degree-in: Returns degree of the first inner edge of the first vertex"                                          
    (check-equal? (get-vertex-degree-in list-e0 v0) 0))

   (test-case                                                          
    "get-vertex-degree-in: Returns degree of the first inner edge of the second vertex"                                          
    (check-equal? (get-vertex-degree-in list-e0 v1) 1))

   (test-case                                                          
    "get-vertex-degree-in: Returns degree of the first inner edge of the third vertex"                                          
    (check-equal? (get-vertex-degree-in list-e0 v2) 0))

   (test-case
    "get-vertex-degree: Returns degree of the first edge of the first vertex"
    (check-equal? (get-vertex-degree list-e0 v0) 1))

   (test-case
    "get-vertex-degree: Returns degree of the first edge of the second vertex"
    (check-equal? (get-vertex-degree list-e0 v1) 2))

   (test-case
    "get-vertex-degree: Returns degree of the first edge of the third vertex"
    (check-equal? (get-vertex-degree list-e0 v2) 1))

   (test-case
    "add-vertex: Add a vertex to a graph"
    (check-equal? (length (get-list-vert (add-vertex v4 g0))) 5))

   (test-case
    "add-edge: Add an edge to a graph"
    (check-equal? (length (get-list-vert (add-edge e3 g0))) 4))

   (test-case
    "find-vertex-by-ID: Find a vertex in a graph by its ID"
    (check-equal? (get-ID-vertex (find-vertex-by-ID 1 list-v0)) 1))
  
   (test-case
    "find-vertex-by-ID: Find a vertex in a graph by its ID"
    (check-equal? (find-vertex-by-ID 100 list-v0) #f))

   (test-case
    "find-edges-by-vertices: Find an edge in a graph by its source and destination vertices"
    (check-equal? (get-ID-edge (car (find-edges-by-vertices v0 v1 list-e0))) 100))

   (test-case
    "find-edges-by-vertices: Find an edge in a graph by its source and destination vertices"
    (check-equal? (find-edges-by-vertices v1 v0 list-e0) '()))

   (test-case
    "is-simplifiable: Tells if a vertex is simplifiable"
    (check-equal? (is-simplifiable list-e1 v5) #f))

   (test-case
    "is-simplifiable: Tells if a vertex is simplifiable"
    (check-equal? (is-simplifiable list-e1 v6) #f))

   (test-case
    "is-simplifiable: Tells if a vertex is simplifiable"
    (check-equal? (is-simplifiable list-e1 v7) #t))
  
   (test-case
    "is-simplifiable: Tells if a vertex is simplifiable"
    (check-equal? (is-simplifiable list-e1 v8) #f))

   (test-case
    "simplify-graph: Simplify a graph by removing the useless vertices"
    (check-equal? (and (equal? (length (get-list-vert (simplify-graph g1))) 3)
                       (equal? (length (get-list-edge (simplify-graph g1))) 3))
                  #t))

   (test-case
    "simplify-graph: Simplify a graph by removing the useless vertices"
    (check-equal? (and (equal? (length (get-list-vert (simplify-graph g0))) 4)
                       (equal? (length (get-list-edge (simplify-graph g0))) 3))
                  #t))

   (test-case
    "remove-vertex: Remove the vertex and all its inner and outer edges"
    (check-equal? (and (equal? (length (get-list-vert (remove-vertex v7 g0))) 4)
                       (equal? (length (get-list-edge (remove-vertex v7 g0))) 3))
                  #t))

   (test-case
    "remove-vertex: Remove the vertex and all its inner and outer edges"
    (check-equal? (and (equal? (length (get-list-vert (remove-vertex v7 g0))) 4)
                       (equal? (length (get-list-edge (remove-vertex v7 g0))) 3))
                  #t))

   (test-case
    "remove-vertex: Remove the vertex and all its inner and outer edges"
    (check-equal? (dijkstra-complete g2 v1 v7) '(1 3 5 7)))
  
   (test-case
    "remove-vertex: Remove the vertex and all its inner and outer edges"
    (check-equal? (dijkstra-complete g3 v1 v7) '(1 3 4 6 7)))
  )
