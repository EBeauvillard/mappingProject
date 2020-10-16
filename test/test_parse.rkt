#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "../src/graph_interface.rkt" "../src/parse.rkt" "../src/xml_open.rkt")
(provide parse-tests graph-tests)

;;(***************************)
;;(****** TESTS PARSING ******)
;;(***************************)

(define (parse-tests)
  (test-case                                                          
    "Not empty graph returns not empty list of id edges"                                          
    (let* ([sgraph (data-argument "maps/test.osm")])                                          
      (check-not-equal? (get-lid-edge sgraph) '() )))

   (test-case                                                          
    "Not empty graph returns not empty list of ref edges"                                          
    (let* ([sgraph (data-argument "maps/test.osm")])                                          
      (check-not-equal? (get-lref-edge sgraph) '() )))
   
   (test-case                                                          
    "Not empty graph returns not empty list of id vertex"                                          
    (let* ([sgraph (data-argument "maps/test.osm")])                                          
      (check-not-equal? (get-lid-vertex sgraph) '() )))

   (test-case                                                          
    "Not empty graph returns not empty list of lat vertex"                                          
    (let* ([sgraph (data-argument "maps/test.osm")])                                          
      (check-not-equal? (get-llat-vertex sgraph) '() )))

   (test-case                                                          
    "Not empty graph returns not empty list of long vertex"                                          
    (let* ([sgraph (data-argument "maps/test.osm")])                                          
      (check-not-equal? (get-llon-vertex sgraph) '() )))

   (test-case                                                          
    "Not empty graph with only vertices returns empty list of id edges"                                          
    (let* ([sgraph2 (data-argument "maps/test2.osm")])                                          
      (check-equal? (get-lid-edge sgraph2) '() )))

   (test-case                                                          
    "Not empty graph with only vertices returns empty list of ref edges"                                          
    (let* ([sgraph2 (data-argument "maps/test2.osm")])                                          
      (check-equal? (get-lref-edge sgraph2) '() )))

   (test-case                                                          
    "Not empty graph with only edges returns empty list of id vertex"                                          
    (let* ([sgraph2_2 (data-argument "maps/test2_2.osm")])                                          
      (check-equal? (get-lid-vertex sgraph2_2) '() )))

   (test-case                                                          
    "Not empty graph with only edges returns empty list of lat vertex"                                          
    (let* ([sgraph2_2 (data-argument "maps/test2_2.osm")])                                          
      (check-equal? (get-llat-vertex sgraph2_2) '() )))

   (test-case                                                          
    "Not empty graph with only edges returns empty list of long vertex"                                          
    (let* ([sgraph2_2 (data-argument "maps/test2_2.osm")])                                          
      (check-equal? (get-llon-vertex sgraph2_2) '() ))))

#|
(define sgraph3
  (data-argument "maps/test3.osm"))

(define sgraph2_2
  (data-argument "maps/test2_2.osm"))

(define sgraph2
  (data-argument "maps/test2.osm"))
  
(get-lid-edge sgraph3)
(get-lref-edge-src sgraph3)
(get-lref-edge-dest sgraph3)
(get-lref-edge sgraph3)

(get-lid-vertex sgraph3)
(get-llat-vertex sgraph3)
(get-llon-vertex sgraph3)

(get-lid-edge sgraph2_2)
(get-lref-edge-src sgraph2_2)
(get-lref-edge-dest sgraph2_2)
(get-lref-edge sgraph2_2)

(get-lid-vertex sgraph2)
(get-llat-vertex sgraph2)
(get-llon-vertex sgraph2)
|#

;;(*****************************)
;;(******** TESTS GRAPH ********)
;;(*****************************)

(print "tests graph")

(define str "maps/projMapping.osm")

(define (glv str)
  (get-list-vert (cons-graph str)))

(define (gle str)
  (get-list-edge (cons-graph str)))

(define (gsrce str)
  (let ([le (gle str)])
    (map get-src-edge le)))

(define (gdeste str)
  (let ([le (gle str)])
    (map get-dest-edge le)))

(define (graph-tests)
  
  (test-case                                                          
    "vertex 1 of ProjMapping.osm = 515330686"                                          
    (check-equal?
     (car
      (let ([lv (glv str)])
        (map get-ID-vertex lv)))
     515330686))
  
  (test-case                                                          
    "vertex 2 of ProjMapping.osm = 2097959544"                                          
    (check-equal?
     (cadr
      (let ([lv (glv str)])
        (map get-ID-vertex lv)))
     2097959544))

  (test-case                                                          
    "lat vertex 1 of ProjMapping.osm = 44.7754213"                                          
    (check-equal?
     (car
      (let ([lv (glv str)])
        (map get-lat-vertex lv))) 44.7754213))

  (test-case                                                          
    "lat vertex 2 of ProjMapping.osm = 44.7216967"                                          
    (check-equal?
     (cadr
      (let ([lv (glv str)])
        (map get-lat-vertex lv))) 44.7216967))

  (test-case                                                          
    "long vertex 1 of ProjMapping.osm = -0.8587464"                                          
    (check-equal?
     (car
      (let ([lv (glv str)])
        (map get-long-vertex lv))) -0.8587464))

  (test-case                                                          
    "long vertex 2 of ProjMapping.osm = -0.7988466"                                          
    (check-equal?
     (cadr
      (let ([lv (glv str)])
        (map get-long-vertex lv))) -0.7988466))

  (test-case                                                          
    "edge of ProjMapping.osm = 199797372"                                          
    (check-equal?
     (car
      (let ([le (gle str)])
        (map get-ID-edge le))) 199797372))

  (test-case                                                          
    "src edge of ProjMapping.osm = 2097959544"                                          
    (check-equal?
     (car
      (map get-ID-vertex (gsrce str)))
       2097959544))

  (test-case                                                          
    "dest edge of ProjMapping.osm = 515330686"                                          
    (check-equal?
     (car
      (map get-ID-vertex (gdeste str)))
       515330686))
  )

#|
(define (gIDv str)
  (let ([lv (glv str)])
    (map get-ID-vertex lv)))

(define (glatv str)
  (let ([lv (glv str)])
    (map get-lat-vertex lv)))

(define (glongv str)
  (let ([lv (glv str)])
    (map get-long-vertex lv)))

(define (gIDe str)
  (let ([le (gle str)])
    (map get-ID-edge le)))

(gIDv str)
(glatv str)
(glongv str)
(gIDe str)
(map get-ID-vertex (gsrce str))
(map get-ID-vertex (gdeste str))
|#