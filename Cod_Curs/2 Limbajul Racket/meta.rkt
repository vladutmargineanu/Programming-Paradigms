;; Metaprogramare

(define plus (list '+ 3 2))
(eval plus)  ; 5

(define minus (cons '- (cdr plus)))
(eval minus)  ; 1