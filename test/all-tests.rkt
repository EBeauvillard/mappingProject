#lang racket

(require rackunit)
(require rackunit/text-ui)

(require "test_parse.rkt" "test_graph_interface.rkt")


(define all-tests                                                      
  (test-suite
   "tests parse"
   (parse-tests)
   "tests graph"
   (graph-tests)
   "tests graph-interface"
   (graph-interface-tests))
)

                                                                     
(printf "Running tests\n")
(run-tests all-tests)