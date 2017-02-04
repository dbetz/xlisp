; for XLisp 3.0

(define %compile compile)

(define (%expand-macros expr)
  (if (pair? expr)
    (if (symbol? (car expr))
      (let ((expander (get (car expr) '%syntax)))
        (if expander
          (expander expr)
          (let ((expander (get (car expr) '%macro)))
            (if expander
              (%expand-macros (expander expr))
              (cons (car expr) (%expand-list (cdr expr)))))))
      (%expand-list expr))
    expr))

(define (%expand-list lyst)
  (if (pair? lyst)
    (cons (%expand-macros (car lyst)) (%expand-list (cdr lyst)))
    lyst))

(define (compile expr #!optional env)
  (if (default-object? env)
    (%compile (%expand-macros expr))
    (%compile (%expand-macros expr) env)))

(put 'macro '%macro
  (lambda (form)
    (list 'put
          (list 'quote (cadr form))
          (list 'quote '%macro)
          (caddr form))))

(macro syntax
  (lambda (form)
    #f))

(macro compiler-syntax
  (lambda (form)
    (list 'begin
          (list 'put
                (list 'quote (cadr form))
                (list 'quote '%syntax)
                (caddr form))
          (list 'quote (cadr form)))))

(compiler-syntax quote
  (lambda (form) form))
	  
(compiler-syntax quasiquote
  (lambda (x)
    (qq-process (cadr x))))     
    
(define (parse-higher-order-function-definition lambda-list body)
  (let loop ((lambda-list lambda-list) (body body))
    (let ((var (car lambda-list))
          (formals (cdr lambda-list)))
      (if (symbol? var)
        (values var `(named-lambda ,var ,formals ,@body))
        (loop var `((lambda ,formals ,@body)))))))
        
(define (convert-internal-definitions body)
  (let loop ((body body) (bindings '()))
    (if (and body (pair? (car body)) (eq? (caar body) 'define))
      (let* ((expr (car body))
             (var (second expr)))
        (if (pair? var)
          (multiple-value-bind (var val)
                               (parse-higher-order-function-definition var (cddr expr))
            (loop (cdr body) (cons `(,var ,val) bindings)))
          (let ((val (third expr)))
            (loop (cdr body) (cons `(,var ,val) bindings)))))
      (if bindings
        `((letrec ,(reverse bindings) ,@body))
        body))))

(compiler-syntax lambda
  (lambda (form)
    `(lambda ,(second form)
       ,@(%expand-list (convert-internal-definitions (cddr form))))))

(compiler-syntax named-lambda
  (lambda (form)
    `(named-lambda ,(second form) ,(third form)
       ,@(%expand-list (convert-internal-definitions (cdddr form))))))

(compiler-syntax define
  (lambda (form)
    (let ((var (second form)))
      (if (pair? var)
        (let ((body (%expand-list (convert-internal-definitions (cddr form)))))
          (multiple-value-bind (var val)
                               (parse-higher-order-function-definition var body)
            `(define ,var ,val)))
        (let ((val (%expand-macros (third form))))
          (if (and (pair? val) (eq? (car val) 'lambda))
            (let ((val `(named-lambda ,var ,@(cdr val))))
              `(define ,var ,val))
            `(define ,var ,val)))))))
  
(compiler-syntax multiple-value-bind
  (lambda (form)
    `(multiple-value-bind ,(second form)
                          ,(%expand-macros (third form))
       ,@(%expand-list (convert-internal-definitions (cdddr form))))))

(compiler-syntax set!
  (lambda (form)
    `(set!
      ,(second form)
      ,@(%expand-list (cddr form)))))

(define (%cond-expander lyst)
  (cond
      ((pair? lyst)
       (cons
         (if (pair? (car lyst))
           (%expand-list (car lyst))
           (car lyst))
         (%cond-expander (cdr lyst))))
      (else lyst)))

(compiler-syntax cond
  (lambda (form)
    `(cond ,@(%cond-expander (cdr form)))))

; The following code for expanding let/let*/letrec was donated by:
;
; Harald Hanche-Olsen
; The University of Trondheim
; The Norwegian Institute of Technology
; Division of Mathematics
; N-7034 Trondheim NTH
; Norway

(define (%expand-let-assignment pair)
  (if (pair? pair)
    (cons
      (car pair)
      (%expand-macros (cdr pair)))
    pair))

(define (%expand-let-form form)
  (cons
    (car form)
    (cons
      (let ((lyst (cadr form)))
        (if (pair? lyst)
          (map %expand-let-assignment lyst)
          lyst))
      (%expand-list (convert-internal-definitions (cddr form))))))

(compiler-syntax let %expand-let-form)
(compiler-syntax let* %expand-let-form)
(compiler-syntax letrec %expand-let-form)

(macro define-integrable
  (lambda (form)
    `(define ,@(cdr form))))

(macro declare
  (lambda (form) #f))

(define (macro-expand x)
  (let ((expander (get (car x) '%macro)))
    (expander x)))
    
(define (subst new old tree)
  (define (subst1 tree)
    (cond ((pair? tree) (cons (subst1 (car tree))
                              (subst1 (cdr tree))))
          ((eqv? tree old) new)
	  (else tree)))
  (subst1 tree))

(macro define-macro
  (lambda (form)
    (let ((name (caadr form))
          (args (subst '&rest '&body (cdadr form)))
          (body (cddr form)))
      `(macro ,name (named-lambda ,name (form)
                      (apply (lambda ,args ,@body) (cdr form)))))))

(define-macro (fluid-let bindings &body body)
  (let ((vars (map (lambda (binding) (if (pair? binding) (car binding) binding)) bindings))
        (inits (map (lambda (binding) (if (pair? binding) (cadr binding) binding)) bindings))
        (init-vars (map (lambda (binding) (gensym)) bindings))
        (save-vars (map (lambda (binding) (gensym)) bindings))
        (make-set (lambda (v i) `(set! ,v ,i))))
    `(let ,(append (map (lambda (sv v) (list sv v)) save-vars vars)
                   (map (lambda (iv i) (list iv i)) init-vars inits))
       (unwind-protect
         (begin ,@(append (map make-set vars init-vars) body))
         ,@(map make-set vars save-vars)))))
                            
(define-macro (when test &body body)
  `(if ,test (begin ,@body)))

(define-macro (unless test &body body)
  `(if (not ,test) (begin ,@body)))

(define-macro (case test &body cases)
  (let* ((sym (gensym))
         (clauses (map (lambda (x)
                         (cond ((eq? (car x) 'else)
                                x)
			       ((atom? (car x))
			        `((eqv? ,sym ',(car x)) ,@(cdr x)))
			       (else
                               `((memv ,sym ',(car x)) ,@(cdr x)))))
                      cases)) )
    `(let ((,sym ,test))
       (cond ,@clauses))))

(define-macro (multiple-value-list expr)
  `(multiple-value-call list ,expr))
    
(define-macro (multiple-value-set! vars expr)
  (let* ((tmps (map (lambda (x)
	              (gensym))
	            vars))
	 (set-forms (map (lambda (v tv)
                           `(set! ,v ,tv))
                         vars tmps)))
    `(multiple-value-bind ,tmps
			  ,expr
       ,@set-forms)))

;;; Contributed by Matthew Halfant

(define-macro (push! ob lst)
  `(begin 
    (set! ,lst (cons ,ob ,lst))
    ,lst))

(define-macro (pop! lst)
  (let ((var (gensym)))
    `(let ((,var (car ,lst)))
       (set! ,lst (cdr ,lst))
       ,var)))

(define-macro (inc! x &optional (inc 1))
  `(set! ,x (+ ,x ,inc)))

(define-macro (dec! x &optional (dec 1))
  `(set! ,x (- ,x ,dec)))

;;; (dotimes (i 10 [result]) (print i)) prints integers from 0 to 9
;;; This version doesn't support embedded RETURN.
;;; Contributed by Matthew Halfant

(define-macro (dotimes range &body body)
  (let ((incvar (car range))
        (maxvar (cadr range))
	(result (caddr range))
        (loop (gensym)))
    `(let ,loop ((,incvar 0))
       (if (>= ,incvar ,maxvar)
	 ,result
         (begin
           ,@body
           (,loop (+ ,incvar 1)))))))

(define-macro (dotimes2 range &body body)
  (let ((var (car range))
        (maximum (cadr range))
	(result (caddr range)))
    `(let ((,var 0))
       (while (< ,var ,maximum)
         ,@body
         (set! ,var (1+ ,var)))
       ,result)))

;;; (dolist (x '(a b) [result]) (print i)) prints a and b
;;; This version doesn't support embedded RETURN.
;;; Modified from dotimes contributed by Matthew Halfant

(define-macro (dolist range &body body)
  (let ((var (car range))
        (value-list (cadr range))
	(result (caddr range))
        (loop (gensym))
	(list-var (gensym)))
    `(let ,loop ((,list-var ,value-list))
       (if ,list-var
         (begin
           (let ((,var (car ,list-var)))
             ,@body)
           (,loop (cdr ,list-var)))
	 ,result))))

(define-macro (dolist2 range &body body)
  (let ((var (car range))
        (value-list (cadr range))
	(result (caddr range))
	(list-var (gensym)))
    `(let ((,list-var ,value-list))
       (while ,list-var
         (let ((,var (car ,list-var)))
           ,@body)
         (set! ,list-var (cdr ,list-var)))
       ,result)))

(define-macro (do bindings test-result &body body)
  (let ((loop (gensym))
        (let-bindings nil)
        (step-exprs nil)
        (test (car test-result))
        (result (cdr test-result)))
    (let loop ((bindings bindings))
      (if bindings
        (let* ((binding (car bindings))
               (var (first binding))
               (init (second binding))
               (step (if (cddr binding) (third binding) var)))
          (push! (list var init) let-bindings)
          (push! step step-exprs)
          (loop (cdr bindings)))))
    (set! let-bindings (reverse let-bindings))
    (set! step-exprs (reverse step-exprs))
    `(let ,loop ,let-bindings
       (if ,test
         (begin ,@result)
         (begin ,@(append body (list (cons loop step-exprs))))))))
                      
(define-macro (time &body body)
  (let ((time (gensym)))
    `(let ((,time (get-time)))
       (begin ,@body)
       (set! ,time (- (get-time) ,time))
       (format t "~%Elapsed time: ~A seconds" ,time)
       ,time)))

