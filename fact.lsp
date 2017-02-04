(define (fact n)
  (let loop ((n n))
    (if (= n 1)
      1
      (* n (loop (- n 1))))))
