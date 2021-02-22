(define-syntax-rule (pack expr)
  (delay expr))        ; sau (lambda () expr)

(define unpack force)  ; sau (lambda (p) (p))
