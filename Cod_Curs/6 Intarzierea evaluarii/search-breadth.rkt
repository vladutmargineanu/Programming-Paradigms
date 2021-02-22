(define (breadth-search-goal init expand goal?)
  (let search ([states (list init)])
    (if (null? states) '()
        (let ([state  (car states)]
              [states (cdr states)])
          (if (goal? state) state
              (search (append states
                              (expand
                               state))))))))