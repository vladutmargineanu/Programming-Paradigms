#lang racket

(include "pack.rkt")

(define-syntax-rule (node key fst snd)
  (pack (list key fst snd)))

(define key car)
(define fst (compose unpack cadr))
(define snd (compose unpack caddr))

(define graph
  (letrec ([a (node 'a a b)]
           [b (node 'b b a)])
    (unpack a)))

(eq? graph (fst graph)) ; similar cu == din Java
; #f pentru inchideri, #t pentru promisiuni

graph
(fst graph)
(fst (fst graph))
(snd graph)
(fst graph)
(fst (fst graph))

(eq? graph (snd (snd graph)))