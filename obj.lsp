(define-class x (ivars a b c))
(define-method (x 'initialize aa bb cc)
  (set! a aa b bb c cc)
  self)
(define-method (x 'foo x y)
  (list a b c x y))

(define-class y (ivars d) (super x))
(define-method (y 'initialize aa bb cc dd)
  (super 'initialize aa bb cc)
  (set! d dd)
  self)
(define-method (y 'bar e)
  (list a b c d e))

(set! o1 (x 'new 1 2 3))
(set! o2 (y 'new 3 4 5 6))

