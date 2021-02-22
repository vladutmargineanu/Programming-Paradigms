#lang racket

;; Motor de răspuns la interogările adresate unei baze de date, constituite
;; din fapte simbolice, ca mai jos. Sistemul identifică toate faptele
;; care se potrivesc cu șabloanele prezente în interogare, legând eventualele
;; variabile din șabloane la componente ale faptelor.
;;
;; Adaptare după:
;; Abelson, H. and Sussman, G. J. (1996).
;; Structure and Interpretation of Computer Programs, 2nd edition.
;;
;; Exemplu de interogare, unde simbolurile care încep cu '?' (exemplu: '?x)
;; reprezintă variabile, iar celelalte, constante:
;;
;; (and (job ?name (computer programmer))
;;      (salary ?name ?salary)
;;
;; Evaluați:
;;
;; (eval-query '(and (job ?name (computer programmer)) (salary ?name ?salary)) '(()))

(require test-engine/racket-tests)

(define FACTS
  '(
    (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
    (job (Bitdiddle Ben) (computer wizard))
    (salary (Bitdiddle Ben) 60000)
    
    (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
    (job (Hacker Alyssa P) (computer programmer))
    (salary (Hacker Alyssa P) 40000)
    (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
    (address (Fect Cy D) (Cambridge (Ames Street) 3))
    (job (Fect Cy D) (computer programmer))
    (salary (Fect Cy D) 35000)
    (supervisor (Fect Cy D) (Bitdiddle Ben))
    (address (Tweakit Lem E) (Boston (Bay State Road) 22))
    (job (Tweakit Lem E) (computer technician))
    (salary (Tweakit Lem E) 25000)
    (supervisor (Tweakit Lem E) (Bitdiddle Ben))
    
    (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
    (job (Reasoner Louis) (computer programmer trainee))
    (salary (Reasoner Louis) 30000)
    (supervisor (Reasoner Louis) (Hacker Alyssa P))
    
    (supervisor (Bitdiddle Ben) (Warbucks Oliver))
    (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
    (job (Warbucks Oliver) (administration big wheel))
    (salary (Warbucks Oliver) 150000)
    
    (address (Scrooge Eben) (Weston (Shady Lane) 10))
    (job (Scrooge Eben) (accounting chief accountant))
    (salary (Scrooge Eben) 75000)
    (supervisor (Scrooge Eben) (Warbucks Oliver))
    (address (Cratchet Robert) (Allston (N Harvard Street) 16))
    (job (Cratchet Robert) (accounting scrivener))
    (salary (Cratchet Robert) 18000)
    (supervisor (Cratchet Robert) (Scrooge Eben))
    
    (address (Aull DeWitt) (Slumerville (Onion Square) 5))
    (job (Aull DeWitt) (administration secretary))
    (salary (Aull DeWitt) 25000)
    (supervisor (Aull DeWitt) (Warbucks Oliver))
    
    (can-do-job (computer wizard) (computer programmer))
    (can-do-job (computer wizard) (computer technician))
    
    (can-do-job (computer programmer)
                (computer programmer trainee))
    
    (can-do-job (administration secretary)
                (administration big wheel))
    ))

;; Rezolvă o interogare complexă, formată din interogări simple legate
;; prin cuvântul cheie 'and.
(define (eval-query query frames)
  (let ([query-type (car query)])
    (cond [(eq? query-type 'and) (eval-and (cdr query) frames)]
          [(eq? query-type 'or)  (eval-or  (cdr query) frames)]
          [else (simple-query query frames)])))

;; Rezolvă lista de interogări 'queries', legate implicit prin 'and, pornind
;; de la lista de mulțimi de legări 'frames'.
;;
;; Se observă că 'frames' obținut prin rezolvarea unei interogări constituie
;; punctul de plecare pentru rezolvarea interogării următoare.
(define (eval-and queries frames)
  (foldl (λ (query frames-acc)
           (eval-query query frames-acc))
         frames
         queries))

;; Rezolvă lista de interogări 'queries', legate implicit prin 'or, pornind
;; de la lista de mulțimi de legări 'frames'.
;;
;; Se observă că rezolvarea interogării curente se face permanent în raport
;; cu 'frames' inițial.
(define (eval-or queries frames)
  (concat (map (λ (query)
                 (eval-query query frames))
               queries)))

;; Rezolvă interogarea simplă 'query' (care nu conține conectori precum 'and
;; sau 'or) în raport cu întreaga bază de date 'FACTS' de mai sus, pornind
;; de la lista de mulțimi de legări 'frames'.
;;
;; Având în vedere că 'match-facts' întoarce o listă de mulțimi de legări
;; pentru o singură mulțime de legări, aplicația lui 'map' se evaluează la o listă
;; de liste de mulțimi de legări. Astfel, este necesar 'concat', pentru revenirea
;; la liste de mulțimi de legări.
(define (simple-query query frames)
  (concat (map (λ (frame)
                 (match-facts query frame))
               frames)))

;; Verifică potrivirea șablonului 'pattern' cu fiecare dintre faptele din baza
;; de date 'FACTS' de mai sus, conform legărilor preexistente din mulțimea
;; 'frame'. Întoarce o listă de mulțimi de legări.
;;
;; 'values' este funcția identitate din Racket, '(λ (x) x)', necesară
;; pentru eliminarea posibilelor valori #f întoarse de 'match'.
(define (match-facts pattern frame)
  (filter values
          (map (λ (fact)
                 (match pattern fact frame))
               FACTS)))

;; Verifică dacă șablonul 'pattern' se potrivește cu faptul 'fact', conform
;; legărilor preexistente din mulțimea 'frame'. Dacă potrivirea se poate realiza,
;; întoarce noua listă de legări, plecând de la 'frame', pe care eventual
;; o îmbogățește. Dacă cele două elemente nu se potrivesc, întoarce 'false'.
;;
;; Funcțiile 'match' și 'extend-consistent' sunt mutual recursive.
(check-expect (match 'a 'a '()) '())
(check-expect (match 'a 'b '()) #f)
(check-expect (match '?x 'a '()) '((?x a)))
(check-expect (match '?x 'a '((?x a))) '((?x a)))
(check-expect (match '?x 'a '((?x b))) #f)
(check-expect (match '(?x ?x) '(a a) '()) '((?x a)))
(check-expect (match '(?x ?x) '(a b) '()) #f)

(define (match pattern fact frame)
  (cond [(eq? frame #f) #f]
        [(equal? pattern fact) frame]
        [(var? pattern) (extend-consistent pattern fact frame)]
        [(and (pair? pattern) (pair? fact))
         (match (cdr pattern) (cdr fact)
           (match (car pattern) (car fact) frame))]
        [else #f]))

;; Extinde mulțimea de legări 'frame' cu asocierea dintre variabila 'var'
;; și faptul 'fact', dacă aceasta din urmă este consistentă cu legările
;; preexistente.
(define (extend-consistent var fact frame)
  (let ([binding (find-binding var frame)])
    (if binding
        (match (value binding) fact frame)
        (extend var fact frame))))

;; Extinde mulțimea de legări 'frame' cu asocierea dintre variabila 'var'
;; și faptul 'fact'.
(define (extend var fact frame)
  (cons (make-binding var fact) frame))

;; Întoarce 'true' dacă simbolul reprezintă o variabilă, conform convențiilor
;; menționate la începutul fișierului.
(check-expect (var? '?x) #t)
(check-expect (var? 'a)  #f)

(define (var? exp)
  (and (symbol? exp)
       (equal? (string-ref (symbol->string exp) 0) #\?)))

;; Concatenează toate listele din lista Ls. Echivalent cu '(apply append Ls)'.
(define (concat Ls)
  (foldr append '() Ls))

;; Sinonime pentru funcții predefinite, cu nume mai relevant pentru sistemul
;; de față.
(define make-binding list)
(define key car)
(define value cadr)
(define find-binding assq)

(test)