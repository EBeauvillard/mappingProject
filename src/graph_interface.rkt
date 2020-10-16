#lang racket
(require racket/trace)

(provide add-vertex add-edge graph-new find-vertex-by-ID get-list-vert find-edges-by-vertices
         get-list-edge get-ID-edge get-src-edge get-dest-edge get-ID-vertex
         get-lat-vertex get-long-vertex get-vertex-degree-out get-vertex-degree-in
         get-vertex-degree is-simplifiable is-simplifiable
         find-edges-by-vertex graph-l_vertices graph-l_edges edge-dest edge-src
         remove-vertex simplify-graph dijkstra-complete get-weight-edge)

;;(****************************)
;;(******** CONSTANTS *********)
;;(****************************)

(define infinity 8888888888888888)

;;(****************************)
;;(********** STRUCT **********)
;;(****************************)

(struct vertex (ID lat long))
(struct edge (ID src dest weight))
(struct graph (l_vertices l_edges))

;;(****************************)
;;(******* CONSTRUCTORS *******)
;;(****************************)

#| Create a new vertex
   @param: ID   -> Identificator
   @param: lat  -> Latitude
   @param: long -> Longitude

   @return: a vertex
|#
(define (vertex-new ID lat long) (vertex ID lat long))

#| Create a new oriented edge
   @param: ID     -> Identificator
   @param: v_src  -> Source vertex
   @param: v_dest -> Destination vertex

   @return: an oriented edge
|#
(define (edge-new ID v_src v_dest weight) (edge ID v_src v_dest weight))

#| Create a new empty graph

   @return: an empty graph
|#
(define (graph-new) (graph '() '()))

;;(*****************************)
;;(********* FUNCTIONS *********)
;;(*****************************)

#| Add a vertex to a graph
   @param: v  -> Vertex
   @param: g  -> Graph

   @return: a graph
|#
(define (add-vertex v g)
  (graph (append (graph-l_vertices g) (list v)) (graph-l_edges g)))

#| Add an edge to a graph
   @param: e  -> Edge
   @param: g  -> Graph

   @return: a graph
|#
(define (add-edge e g)
  (graph (graph-l_vertices g) (append (graph-l_edges g) (list e))))

#| Find a vertex in a graph by its ID
   @param: id -> Identificator of the vertex
   @param: lv -> List of vertices

   @return: a vertex
|#
(define (find-vertex-by-ID id lv)
  (findf (lambda (v) (= (vertex-ID v) id)) lv))

#| Find an edge in a graph by its source and destination vertices
   @param: src     -> Source vertex
   @param: dest    -> Destination vertex
   @param: l_edges -> Edges list

   @return: an edge
|#
(define (find-edges-by-vertices src dest l_edges)
  (sort
   (foldr (lambda (e le)
          (if (and (= (vertex-ID (edge-src e)) (vertex-ID src))
                   (= (vertex-ID (edge-dest e)) (vertex-ID dest)))
              (cons e le)
              le))
          '()
          l_edges)
   <
   #:key edge-ID))

#| Count the number of elements in the list that respects a certain predicate
   @param: list  -> List
   @param: v     -> Vertex
   @param: pred  -> Predicate function (with one parameter)

   @return: the number of elements of list that respects pred
|#
(define (get-count-filter list v pred)
   (foldl + 0
          (map (lambda (x)
                 (if (pred x)
                     1
                     0))
               list)))

#| Count the outer-degree of a vertex
   @param: l_edges -> List of edges
   @param: v       -> Vertex

   @return: the outer-degree of v
|#
(define (get-vertex-degree-out l_edges v)
  (let ([pred (lambda (x) (= (vertex-ID (edge-src x)) (vertex-ID v)))])
    (get-count-filter l_edges v pred)))

#| Count the inner-degree of a vertex
   @param: l_edges -> List of edges
   @param: v       -> Vertex

   @return: the inner-degree of v
|#
(define (get-vertex-degree-in l_edges v)
  (let ([pred (lambda (x) (= (vertex-ID (edge-dest x)) (vertex-ID v)))])
    (get-count-filter l_edges v pred)))

#| Count the degree of a vertex
   @param: l_edges -> List of edges
   @param: v       -> Vertex

   @return: the degree of v
|#
(define (get-vertex-degree l_edges v)
  (+ (get-vertex-degree-out l_edges v) (get-vertex-degree-in l_edges v)))

#| Tells if a vertex is simplifiable
   @param: l_edges -> List of edges
   @param: v       -> Vertex

   @return: boolean
|#
(define (is-simplifiable edges v)
  (and (and (= (get-vertex-degree-in edges v) 2)    ; inner-degree = 2
            (= (get-vertex-degree-out edges v) 2))  ; outer-degree = 2
       (andmap (lambda (dest)
                 (not (null? (find-edges-by-vertices (edge-dest dest) v edges))))
               (find-edges-by-vertex edge-src v edges))))

#| Find the list of edges whose source or destination is v, based on the function given
   @param: side    -> Function edge-src or function edge-dest
   @param: v       -> Linked vertex
   @param: l_edges -> List of edges

   @return: list of edges sorted by ID
|#
(define (find-edges-by-vertex side v l_edges)
  (sort
   (foldr (lambda (e le)
          (if (= (vertex-ID (side e)) (vertex-ID v))
              (cons e le)
              le))
          '()
          l_edges)
   <
   #:key edge-ID))

#| Simplify a graph by removing the useless vertices
   @param: g -> Graph

   @return: a graph
|#
(define (simplify-graph g)
  (if (and (eq? (graph-l_vertices g) '())
           (eq? (graph-l_edges g) '()))
      g
      (let ([s (flatten
            (map (lambda (v)
                   (if (is-simplifiable (graph-l_edges g) v)
                       (let ([edges-out (find-edges-by-vertex edge-src v (graph-l_edges g))])
                         (let ([edges-in (find-edges-by-vertex edge-dest v (graph-l_edges g))])
                           (add-edge (edge-new (edge-ID (car edges-out))
                                               (car edges-out)
                                               (car edges-in)
                                               (+ (edge-weight (car edges-out)) (edge-weight (car edges-in))))
                                     (remove-vertex v g))))
                       '()))
                   (graph-l_vertices g)))])
        (if (null? s)
            g
            (car s)))))


#| Remove a vertex from a graph
   @param: v -> Vertex
   @param: g -> Graph

   @return g : a graph
|#
(define (remove-vertex v g)
    (graph
     (remove v (graph-l_vertices g))
     (filter (lambda (e) (and (not (eq? (edge-src e)  v))
                             (not (eq? (edge-dest e) v))))
             (graph-l_edges g))))

#| Return the length of a list
   @param: lst -> List

   @return: a real 
|#
(define (length lst)
  (cond
    [(empty? lst)  0]
    [(cons? lst)   (+ 1 (length (rest lst)))]))

#| Return the list of neighbours of a vertex
   @param: v       -> Vertex
   @param: l_edges -> List of edges

   @return: a list of vertices
|#
(define (get-neighbours v l_edges)
  (foldl (lambda (e lv)
           (cons (edge-dest e) lv))
         '()
         (find-edges-by-vertex edge-src v l_edges)))

#| Return the list of vertices constitutive the shortest path from the source vertex to the destination vertex
   @param: g    -> Graph
   @param: src  -> Vertex
   @param: dest -> Vertex

   @return: a list of vertices
|#
(define (dijkstra-complete g src dest)

  (let* ([distances (make-hash)]
         [parents (make-hash)]
         [isTreated (make-hash)])

    (letrec ([compute-dist (lambda (src dest edges)
                             (+ (hash-ref distances src) (edge-weight (car (find-edges-by-vertices src dest edges)))))]
             
             [get-neighbours (lambda (v edges)
                               (foldl (lambda (e lv)
                                        (cons (edge-dest e) lv))
                                      '()
                                      (find-edges-by-vertex edge-src v edges)))]

             [init-dijkstra (lambda (g src)
                              (map (lambda (v)
                                     (if (eq? v src)
                                         (begin
                                           (hash-set! distances v 0)
                                           (hash-set! isTreated v #f)
                                           (hash-set! parents v v))
                                         (begin
                                           (hash-set! distances v infinity)
                                           (hash-set! isTreated v #f)
                                           (hash-set! parents v null))))
                                   (graph-l_vertices g)))]

             [select-min-dist (lambda (vertices min v)
                                (match vertices
                                  ['()         v]
                                  [(cons x xs) (if (and (< (hash-ref distances x) min) (eq? #f (hash-ref isTreated x)))
                                                   (select-min-dist xs (hash-ref distances x) x)
                                                   (select-min-dist xs min v))]))]

             [retrieve-path (lambda (s v)
                              (letrec ([retrieve-path-internal (lambda (src vc l)
                                                                 (cond
                                                                   [(null? vc) '()] ; no path exists
                                                                   [(eq? src vc) (cons src l)]
                                                                   [else (retrieve-path-internal src (hash-ref parents vc) (cons vc l))]))])
                                       (map (lambda (v) (vertex-ID v))
                                            (retrieve-path-internal s v '()))))]

             [dijkstra (lambda (g v dest)
                         (begin
                           (map (lambda (n)
                                  (let ([d (compute-dist v n (graph-l_edges g))])
                                    (if (< d (hash-ref distances n))
                                        (begin
                                          (hash-set! distances n d)
                                          (hash-set! parents n v))
                                        #t)))
                                (get-neighbours v (graph-l_edges g)))
                           (hash-set! isTreated v #t)
                           (let ([min (select-min-dist (graph-l_vertices g) infinity null)])
                             (cond
                               [(null? min) (hash-ref distances dest)]
                               [else (dijkstra g min dest)]))))])

      (begin
        (init-dijkstra g src)
        (dijkstra g src dest)
        (retrieve-path src dest)))))


;;(****************************)
;;(*********** GETS ***********)
;;(****************************)

#| Returns the list of vertices of the graph g
   @param: g -> Graph

   @return: the list of vertices of g
|#
(define (get-list-vert g)
  (graph-l_vertices g))

#| Returns the list of edges of the graph g
   @param: g -> Graph

   @return: the list of edges of g
|#
(define (get-list-edge g)
  (graph-l_edges g))

#| Returns the ID of an edge
   @param: e -> Edge

   @return: the ID of the edge
|#
(define (get-ID-edge e)
  (edge-ID e))

#| Returns the src of an edge
   @param: e -> Edge

   @return: the src of the edge
|#
(define (get-src-edge e)
  (edge-src e))

#| Returns the dest of an edge
   @param: e -> Edge

   @return: the dest of the edge
|#
(define (get-dest-edge e)
  (edge-dest e))

#| Returns the ID of a vertex
   @param: v -> Vertex

   @return: the ID of the vertex
|#
(define (get-ID-vertex v)
  (vertex-ID v))

#| Returns the lat of a vertex
   @param: v -> Vertex

   @return: the lat of the vertex
|#
(define (get-lat-vertex v)
  (vertex-lat v))

#| Returns the long of a vertex
   @param: v -> Vertex

   @return: the long of the vertex
|#
(define (get-long-vertex v)
  (vertex-long v))

#| Returns the weight of an edge
   @param: e -> Edge

   @return: the weight of the edge
|#
(define (get-weight-edge e)
  (edge-weight e))

;;(*****************************)
;;(********* CONTRACTS *********)
;;(*****************************)

(provide (contract-out
 ;; Predicate identifying the vertices (returns true iff the argument
 ;; is a vertex)
 ;; @param  v : any/c
 ;; @return   : boolean?
 [vertex? (-> any/c boolean?)]

 ;; Predicate identifying the graphs (returns true iff the argument is
 ;; a graph)
 ;; @param  g : any/c
 ;; @return   : boolean?
 [graph? (-> any/c boolean?)]

 ;; Constructor of a vertex
 ;; @param id   : exact-nonnegative-integer?
 ;; @param lat  : real?
 ;; @param long : real?
 ;; @return     : vertex?
 [vertex-new (-> exact-nonnegative-integer? real? real? vertex?)]

 ;; Predicate identifying the edges (returns true iff the argument
 ;; is an edge)
 ;; @param  v : any/c
 ;; @return   : boolean?
 [edge?  (-> any/c boolean?)]

 ;; Constructor of an edge
 ;; @param id     : exact-nonnegative-integer?
 ;; @param v_src  : vertex?
 ;; @param v_dest : vertex?
 ;; @param weight : number?
 ;; @return       : edge?
 [edge-new (-> exact-nonnegative-integer? vertex? vertex? number? edge?)]
 ))
