#lang racket
(require xml)
(require xml/path)
(require racket/runtime-path)
(provide data data-argument)

;;; opens the .osm file and returns it as a list
(define (data str)
  (xml->xexpr (document-element
    (read-xml (open-input-file str)))))

;;; uses the function below, but with a path either set here by default or in the command line to execute the progam
(define (data-argument str)
  (data str))

;;; sets a path, either "../maps/projMapping.osm" by default or in the command line to execute the program
(define-runtime-path str
  (if (zero? (vector-length (current-command-line-arguments)))
      "../maps/projMapping.osm"
      (vector-ref (current-command-line-arguments) 0)))
