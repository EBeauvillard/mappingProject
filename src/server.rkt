#lang racket

(require xml)
(require web-server/servlet
         web-server/servlet-env)
(require racket/runtime-path)
(require "xml_open.rkt" "parse.rkt" "graph_interface.rkt" "optimal_distance.rkt")

;==========================================================================
;file to open
(define-runtime-path str
  (if (zero? (vector-length (current-command-line-arguments)))
      "../maps/projMapping.osm"
      (string-append "../" (vector-ref (current-command-line-arguments) 0))))

;Prints the list of IDs vertices and edges (to help for distance and route page)
(define sgraph
  (data-argument str))

(get-lid-vertex sgraph)
(get-lid-edge sgraph)

;==========================================================================
;graph for test page

(define v0 (vertex-new 0 0 0))
(define v1 (vertex-new 1 45 -45))
(define v2 (vertex-new 2 89 0))
(define v3 (vertex-new 3 -89 179))
(define v4 (vertex-new 4 89 -179))
(define e0 (edge-new 100 v0 v1 3))
(define e1 (edge-new 101 v1 v2 2))
(define e2 (edge-new 102 v2 v0 5))
(define e3 (edge-new 103 v2 v3 8))
(define e4 (edge-new 104 v4 v0 100))

(define (g0)
  (add-edge
   e4
   (add-edge
    e3
    (add-edge
     e2
     (add-edge
      e1
      (add-edge
       e0
       (add-vertex
        v4
        (add-vertex
         v3
         (add-vertex
          v2
          (add-vertex
           v1
           (add-vertex
            v0
            (graph-new))))))))))))

;==========================================================================
;returns a list of svg circles to symbolise vertices (specific radius to fit the page)
(define (l-circles el)
  `(circle((cx ,(number->string (+ 4 (get-long-vertex el))))
           (cy ,(number->string (+ 3  (get-lat-vertex el))))
           (r "0.1")
           (fill "red"))))

;returns a list of svg lines to symbolise edges (specific width to fit the page)
(define (l-lines e)
  `(line ((x1 ,(number->string (+ 4 (get-long-vertex (get-src-edge e)))))
          (x2 ,(number->string (+ 4 (get-long-vertex (get-dest-edge e)))))
          (y1 ,(number->string (+ 3 (get-lat-vertex (get-src-edge e)))))
          (y2 ,(number->string (+ 3 (get-lat-vertex (get-dest-edge e)))))
          (stroke-width "0.05")
          (stroke "black"))))

;; main page
(define (main-page req)
  (response/xexpr
   (let ([bindings (request-bindings req)]
         [url (url->string (request-uri req))]
         [g (cons-graph str)])
     (let ([lv (get-list-vert g)]
           [le (get-list-edge g)])
       `(html (head (title "MAIN PAGE"))
              (body
               (h1 ,url)
               (svg ((viewBox "0 0 15 15")
                     (xmlns "http://www.w3.org/2000/svg"))
                    ,@(if (not(null? lv))
                          (map l-circles lv)
                          `(pre "no vertices"))
                    ,@(if (not(null? le))
                          (map l-lines le)
                          `(pre "no edges")))))))))

;==========================================================================
;returns a list of svg circles to symbolise vertices (specific radius to fit the page)
(define (l-circles-test el)
  `(circle((cx ,(number->string (+ 180 (get-long-vertex el))))
           (cy ,(number->string (+ 90  (get-lat-vertex el))))
           (r "2")
           (fill "red"))))

;returns a list of svg lines to symbolise edges (specific width to fit the page)
(define (l-lines-test e)
  `(line ((x1 ,(number->string (+ 180 (get-long-vertex (get-src-edge e)))))
          (x2 ,(number->string (+ 180 (get-long-vertex (get-dest-edge e)))))
          (y1 ,(number->string (+ 90 (get-lat-vertex (get-src-edge e)))))
          (y2 ,(number->string (+ 90 (get-lat-vertex (get-dest-edge e)))))
          (stroke-width "1")
          (stroke "black"))))

;a test page
(define (test-page req)
  (response/xexpr
   (let ([bindings (request-bindings req)]
         [url (url->string (request-uri req))]
         [g (g0)])
     (let ([lv (get-list-vert g)]
           [le (get-list-edge g)])
       `(html (head (title "TEST PAGE"))
              (body
               (h1 ,url)
               (svg ((viewBox "0 0 370 190")
                     (xmlns "http://www.w3.org/2000/svg"))
                    ,@(if (not(null? lv))
                          (map l-circles-test lv)
                          `(pre "no vertices"))
                    ,@(if (not(null? le))
                          (map l-lines-test le)
                          `(pre "no edges")))))))))

;==========================================================================
;computes the distance beetween 2 vertices
(define (dist-vert v1 v2)
  (optimal-distance 6371 v1 v2))

;; Distance page
(define (distance-page req)
  (response/xexpr
   (let ([bindings (request-bindings req)]
         [url (url->string (request-uri req))]
         ;[lv (lv-test)]
         [g (cons-graph str)])
         (let ([lv (get-list-vert g)])
     (cond [(or (equal? bindings '()) (null? (car bindings)) (null? (cdr bindings)))
           `(html (head (title "DISTANCE PAGE"))
                   (body
                    (h1,url)
                    (h6, "Disconnected Universe Error")))]
           [(equal? (find-vertex-by-ID (string->number (cdar bindings)) lv) #f)
            `(html (head (title "DISTANCE PAGE"))
                   (body
                    (h1,url)
                    (h6, "Disconnected Universe Error")))]
           [(equal? (find-vertex-by-ID (string->number (cdadr bindings)) lv) #f)
            `(html (head (title "DISTANCE PAGE"))
                   (body
                    (h1,url)
                    (h6, "Disconnected Universe Error")))]
           [`(html (head (title "DISTANCE PAGE"))
                   (body
                    (h1 ,url)
                    (pre , (format "Source : ~a" (cdar bindings)))
                    (pre , (format "Destination : ~a" (cdadr bindings)))
                    (pre ,
                         (format "Distance de ~a "(cdar bindings)),
                         (format "à ~a" (cdadr bindings)),
                         (format " en km : ~a" (dist-vert (find-vertex-by-ID (string->number (cdar bindings)) lv) (find-vertex-by-ID (string->number (cdadr bindings)) lv))))))])))))

;==========================================================================
;computes the route beetween 2 vertices (ID : id1 and id2) in the graph g
(define (route id1 id2 g)
  (let ([lv (get-list-vert g)])
    (dijkstra-complete g (find-vertex-by-ID (string->number id1) lv) (find-vertex-by-ID (string->number id2) lv))))

;; Route page
(define (route-page req)
  (response/xexpr
   (let ([bindings (request-bindings req)]
         [url (url->string (request-uri req))]
         [g (cons-graph str)])
     (let ([lv (get-list-vert g)])
         (cond[(or (equal? bindings '()) (null? (car bindings)) (null? (cdr bindings)))
               `(html (head (title "ROUTE PAGE"))
                      (body
                       (h1,url)
                       (h6, "Disconnected Universe Error")))]
              [(equal? (find-vertex-by-ID (string->number (cdar bindings)) lv) #f)
               `(html (head (title "ROUTE PAGE"))
                      (body
                       (h1,url)
                       (h6, "Disconnected Universe Error")))]
              [(equal? (find-vertex-by-ID (string->number (cdadr bindings)) lv) #f)
               `(html (head (title "ROUTE PAGE"))
                      (body
                       (h1,url)
                       (h6, "Disconnected Universe Error")))]
              
              [else
               (let ([route (route (cdar bindings) (cdadr bindings) g)])
                 `(html (head (title "ROUTE PAGE"))
                        (body
                         (h1, url)
                         (pre, (if (null? route)
                                   (format "Il n'existe aucun chemin menant de la source à la destination spécifiées.")
                                   (format "Chemin à prendre -> ~a" route))))))])))))

;==========================================================================
;; Routing function
;;     /display          --->   display-page
;;     /route            --->   route-page
;;     /distance         --->   distance-page
;;     everything else   --->   main-page
(define-values (server-dispatch server-url)
    (dispatch-rules
     [("route") route-page]
     [("distance") distance-page]
     [("test") test-page]
     [else main-page]))

(serve/servlet server-dispatch
               #:servlet-regexp #rx""
               #:port 9000
               #:launch-browser? #f)
