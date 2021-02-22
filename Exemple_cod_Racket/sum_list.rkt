#lang racket
(define (sum-list L)
  (if (null? L)
      0
      (+ (car L) (sum-list(cdr L)))))