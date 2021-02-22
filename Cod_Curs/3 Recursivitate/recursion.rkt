#lang racket

(require (lib "trace.ss"))

;; Factorial - recursivitate pe stivă

(define (fact-stack n)
  (if (= n 1)
      1
      (* n (fact-stack (- n 1)))))

;(trace fact-stack)
;(fact-stack 5)

;(time (fact-stack 10000))


;; Factorial - recursivitate pe coadă

(define (fact-tail n) 
  (fact-tail-helper 1 1 n))

(define (fact-tail-helper product i n)
  (if (> i n)
      product
      (fact-tail-helper (* product i)
                        (+ i 1)
                        n)))

;(trace fact-tail-helper)
;(fact-tail 5)

;(time (fact-tail 10000))


;; Fibonacci - recursivitate pe stivă

(define (fib-stack n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fib-stack (- n 1))
                 (fib-stack (- n 2)))]))

;(trace fib-stack)
;(fib-stack 5)

;(time (fib-stack 35))


;; Fibonacci - recursivitate pe coadă

(define (fib-tail n)
  (fib-tail-helper 1 0 n))

(define (fib-tail-helper a b count)
  (if (= count 0)
      b
      (fib-tail-helper (+ a b) a (- count 1))))

;(trace fib-tail-helper)
;(fib-tail 5)

;(time (fib-tail 35))