#lang racket

(define (minList1 L)
  (if (= (length L) 1) (car L)
      (min (car L) (minList1 (cdr L)))))

(define (minList2 L)
  (foldl min (car L) (cdr L)))