#|

Quasiquote expander for XLISP 3.0

Derived from the code in Appendix C of "Common Lisp" by Guy L. Steele Jr.
without the simplifier for now.

|#

(define (qq-process x)
  (cond ((symbol? x)
         (list 'quote x))
        ((atom? x)
         x)
        ((eq? (car x) 'quasiquote)
         (qq-process (qq-process (cadr x))))
        ((eq? (car x) 'unquote)
         (cadr x))
        ((eq? (car x) 'unquote-splicing)
         (error ",@ after ` in ~S" (cadr x)))
        (else
         (let loop ((p x) (q '()))
           (if (atom? p)
             (cons 'append
                   (append (reverse q) (list (if (symbol? p) (list 'quote p) p))))
             (begin
               (if (eq? (car p) 'unquote)
                 (begin
                   (if (cddr p) (error "malformed , in ~S" p))
                   (cons 'append
                         (append (reverse q) (list (cadr p)))))
                 (if (eq? (car p) 'unquote-splicing)
                   (error "dotted ,@ in ~S" p)
                   (loop (cdr p) (cons (qq-bracket (car p)) q))))))))))
               
(define (qq-bracket x)
  (cond ((atom? x)
         (list 'list (qq-process x)))
        ((eq? (car x) 'unquote)
         (list 'list (cadr x)))
        ((eq? (car x) 'unquote-splicing)
         (cadr x))
        (else
         (list 'list (qq-process x)))))
            