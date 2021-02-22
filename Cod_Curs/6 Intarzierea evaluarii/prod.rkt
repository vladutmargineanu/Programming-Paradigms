#lang racket

(include "pack.rkt")

;; Implementare directă

(define (prod1 x y)
  (if x (* y (+ y 1)) 0))

(define (test1 x)
  (let ([y 5])
    (prod1 x (begin (display "y") y))))

(test1 #f)  ; y  0
(test1 #t)  ; y 30

;; 'quote' și 'eval'

(define (prod2 x y)
  (if x (* (eval y) (+ (eval y) 1)) 0))

(define (test2 x)
  (let ([y 5])
    (prod2 x '(begin (display "y") y))))

(test2 #f)  ; 0
;(test2 #t)  ; y y: undefined

;; Închideri funcționale

(define (prod3 x y)
  (if x (* (y) (+ (y) 1)) 0))

(define (test3 x)
  (let ([y 5])
    (prod3 x (lambda ()
               (begin (display "y") y)))))

(test3 #f)  ; 0
(test3 #t)  ; yy 30

;; 'delay' și 'force'

(define (prod4 x y)
  (if x (* (force y) (+ (force y) 1)) 0))

(define (test4 x)
  (let ([y 5])
    (prod4 x (delay (begin (display "y") y)))))

(test4 #f)  ; 0
(test4 #t)  ; y 30

;; Varianta generică

(define (prod-g x y)
  (if x (* (unpack y) (+ (unpack y) 1)) 0))

(define (test-g x)
  (let ([y 5])
    (prod-g x (pack (begin (display "y") y)))))

(test-g #f)
(test-g #t)