(define (lazy-breadth-search init expand)
  (let search
    ([states (stream-cons init empty-stream)])
    (if (stream-empty? states) states
        (let ([state  (stream-first states)]
              [states (stream-rest states)])
          (stream-cons
           state
           (search (stream-append
                    states
                    (expand state))))))))

(define (lazy-breadth-search-goal
         init expand goal?)
  (stream-filter goal?
                 (lazy-breadth-search init
                                      expand)))