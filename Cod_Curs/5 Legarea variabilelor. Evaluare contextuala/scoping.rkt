;; Utilizați 'Pretty Big', care permite 'define'-uri multiple pentru variabile
;; cu același nume.
;; #lang racket interzice acest lucru.

;; let, pentru legarea statică a variabilelor locale

(let ([x 1] [y 2])
  (+ x y))

(let ([x 1])
  (let ([x 2])
    x))

(let ([x 1])
  (+ (let ([x 2])
       x)
     x))

;; Cu toate că permite legări multiple, 'let' izolează definițiile
;; una de cealaltă. Mai jos, în definiția lui 'y', 'x' nu este vizibil,
;; secvența generând eroare!
;(let ([x 2] [y x])
;  (+ x y))

;; În schimb, secvența de mai jos funcționează, întrucât se folosesc
;; două construcții 'let'.
(let ([x 1])
  (let ([x 2]
        [y x])  ; Aici, numai definiția exterioară (x = 1) este vizibilă!
    (+ x y)))

;; let*

;; Secvența eronată de mai sus funcționează cu 'let*'.
(let* ([x 2] [y x])
  (+ x y))

;; letrec

;; Funcție recursivă locală
(letrec ([factorial (λ (n)
                      (if (zero? n) 1
                          (* n (factorial (- n 1)))))])
  (factorial 5))

;; define, pentru legarea dinamică a variabilelor top-level.

(define x 0)
(define f (lambda () x))
(f)  ; 0
(define x 1)
(f)  ; 1

;; Efecte obscure ale utilizării 'define'.
(define factorial
  (lambda (n)
    (if (zero? n) 1
        (* n (factorial (- n 1))))))

(factorial 5)

(define g factorial)
(define factorial (lambda (x) x))  ; Această linie afectează aplicația RECURSIVĂ
; a lui 'factorial' din corpul lui 'g'.

(g 5)

;; Legare mixtă

(define x 0)
(define f (lambda () x))
(define x 1)

(define g
  (lambda (x)  ; Cu toate că 'x' este 2 când 'f' este aplicat, acest 'x'
    (f)))      ; nu este vizibil în definiția lui 'f'.

(g 2)

;; Aplicație

;; Mai jos, B este trimis nemodificat fiecărei aplicații recursive
(define (app A B)
  (if (null? A)
      B
      (cons (car A) (app (cdr A) B))))

(app '(1 2 3) '(4 5 6))

;; Rescriem cu 'letrec'.
(define (app2 A B)
  (letrec ([internal
            (lambda (L)
              (if (null? L) B
                  (cons (car L)
                        (internal (cdr L)))))])
    (internal A)))

(app2 '(1 2 3) '(4 5 6))

;; Rescriem cu 'named let'.
(define (app3 A B)
  (let internal ([L A])
    (if (null? L) B
        (cons (car L)
              (internal (cdr L))))))

(app3 '(1 2 3) '(4 5 6))