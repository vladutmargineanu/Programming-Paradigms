;; Utilizați 'Pretty Big', care permite 'define'-uri multiple pentru variabile
;; cu același nume.
;; #lang racket interzice acest lucru.

(define comp
  (lambda (f)
    (lambda (g)
      (lambda (x)
        (f (g x))))))

(define inc (lambda (x) (+ x 1)))
(define comp-inc (comp inc))

(define double (lambda (x) (* x 2)))
(define comp-inc-double (comp-inc double))

(comp-inc-double 5)  ; 11

(define inc (lambda (x) x))
(comp-inc-double 5)  ; tot 11!