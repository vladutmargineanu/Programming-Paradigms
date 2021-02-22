#lang racket

(include "search-breadth-lazy.rkt")
(include "stream-operators.rkt")

(define (palindromes n symbols)
  (let ([symbol-stream (list->stream symbols)])
    (lazy-breadth-search-goal
     '()
     (lambda (state)
       (stream-map (lambda (symbol) (cons symbol state))
                   symbol-stream))
     (lambda (state)
       (and (>= (length state) n) (equal? state (reverse state)))))))

(stream-take 10 (palindromes 2 '(a b)))