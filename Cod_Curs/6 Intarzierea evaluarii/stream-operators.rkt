;#lang racket

#|
(define-syntax-rule (stream-cons head tail)
  (cons head (pack tail)))

(define stream-first car)

(define stream-rest (compose unpack cdr))

(define empty-stream '())

(define stream-empty? empty?)
|#

(define (stream-take n s)
  (cond [(zero? n) '()]
        [(stream-empty? s) '()]
        [else (cons (stream-first s)
                    (stream-take (- n 1) (stream-rest s)))]))

(define (list->stream L)
  (if (null? L) empty-stream
      (stream-cons (car L)
                   (list->stream (cdr L)))))

(define (stream-zip-with f s1 s2)
  (if (stream-empty? s1) s2
      (stream-cons (f (stream-first s1) (stream-first s2))
                   (stream-zip-with f (stream-rest s1) (stream-rest s2)))))
