#lang racket

(include "stream-operators.rkt")

;; Fluxul de 1

(define ones (stream-cons 1 ones))
; (stream-take 5 ones)  ; (1 1 1 1 1)

;; Fluxul numerelor naturale: formulare explicită

(define (naturals-from n)
  (stream-cons n (naturals-from (+ n 1))))

(define naturals-1 (naturals-from 0))

;; Fluxul numerelor naturale: formulare implicită

(define naturals-2
  (stream-cons 0 
               (stream-zip-with +
                                ones
                                naturals-2)))

;; Fluxul numerelor pare

(define even-naturals-1
  (stream-filter even? naturals-1))

(define even-naturals-2
  (stream-zip-with + naturals-1 naturals-1))

;; Fluxul sumelor parțiale ale altui flux

(define (sums s)
  (letrec ([out (stream-cons
                 0
                 (stream-zip-with + s out))])
    out))

; (stream-take 10 (sums naturals-1))

;; Fluxul numerelor Fibonacci

(define fibo
  (stream-cons 0
   (stream-cons 1
    (stream-zip-with +
                     fibo
                     (stream-rest fibo)))))

;; Fluxul numerelor prime

(define (sieve s)
  (if (stream-empty? s) s
      (stream-cons
       (stream-first s)
       (sieve
        (stream-filter 
         (lambda (n)
           (not (zero? (remainder
                        n
                        (stream-first s)))))
         (stream-rest s))))))

(define primes (sieve (naturals-from 2)))