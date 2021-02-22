(define (even-sum-iter a b)
  (let iter ([n a]
             [sum 0])
    (cond [(> n b) sum]
          [(even? n) (iter (+ n 1) (+ sum n))]
          [else (iter (+ n 1) sum)])))

(define (even-sum-lists a b)
    (foldl + 0 (filter even? (interval a b))))