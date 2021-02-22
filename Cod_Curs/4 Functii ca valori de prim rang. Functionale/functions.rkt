#lang racket

;; Evaluarea unei subexpresii care întoarce o funcție
((if true + -) (+ 1 2) 3)

;; Funcția de compunere.
;; Primește două funcții și întoarce funcția compusă.
;; Simbolul λ se obține cu Ctrl+\
(define (comp f g)
  (λ (x)
    (f (g x))))

;; Obține al doilea element al unei liste, aplicând mai întâi 'cdr',
;; apoi 'car' asupra rezultatului.
((comp car cdr) '(1 2 3))

;; Compune funcțiile de incrementare și dublare
((comp (λ (x) (+ x 1)) (λ (x) (* x 2))) 5)

;; Înmulțește cu 10 toate elementele listei.
(map (λ (x) (* x 10)) '(1 2 3))

;; Obține paritatea fiecărui numar (true = par)
(map (λ (x) (even? x)) '(1 2 3))

;; Funcție care întoarce o altă funcție; zisă "curried".
(define (mult-map-by q)
  (λ (x)
    (* x q)))

(map (mult-map-by  5) '(1 2 3))
(map (mult-map-by 10) '(1 2 3))

;; Extrage numerele pare dintr-o listă
(filter even? '(1 2 3))

;; Suma elementelor unei liste
(foldl + 0 '(1 2 3))
(foldr + 0 '(1 2 3))

;; Implementare explicită 'foldl', folosind recursivitate pe coadă.
(define (my-foldl f acc L)
  (if (null? L) acc
      (my-foldl f (f (car L) acc) (cdr L))))

;; Implementare explicită 'foldr', folosind recursivitate pe stivă.
(define (my-foldr f acc L)
  (if (null? L) acc
      (f (car L) (my-foldr f acc (cdr L)))))

;; Suma elementelor unei liste
(my-foldl + 0 '(1 2 3))
(my-foldr + 0 '(1 2 3))

;; Implementarea 'map' folosind 'foldr'
(define (my-map f L)
  (foldr (λ (x acc) (cons (f x) acc)) '() L))

(my-map (mult-map-by  5) '(1 2 3))

;; Implementarea 'filter' folosind 'foldr'
(define (my-filter f L)
  (foldr (λ (x acc)
           (if (f x)
               (cons x acc)
               acc))
         '()
         L))

(my-filter even? '(1 2 3))

;; Implementarea 'reverse' folosind 'foldl'
(define (rev L)
  (foldl cons '() L))

(rev '(1 2 3))

;; Întrepătrunderea utilizării funcționalelor și a recursivității explicite

;; Aplică o funcție pe elemente, indiferent de nivelul de imbricare.
(define (deep-map f L)
  (map (λ (x)
         (if (not (list? x)) (f x)
             (deep-map f x)))
       L))

(deep-map add1 '(1 (2 3 (4) 5)))

;; Implementarea perechilor folosind EXCLUSIV funcții

;; O pereche este o funcție care așteaptă un selector, ce va fi aplicat
;; asupra componentelor
(define (my-cons a b)
  (λ (selector)
    (selector a b)))

;; Aplică perechea asupra unui selector care întoarce prima componentă
(define (my-car pair)
  (pair (λ (x y) x)))

;; Aplică perechea asupra unui selector care întoarce a doua componentă
(define (my-cdr pair)
  (pair (λ (x y) y)))

;; Utilizați stepper-ul pentru a înțelege pașii de evaluare.
(define pair (my-cons 1 2))
pair

(my-car pair)
(my-cdr pair)