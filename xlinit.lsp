; xlinit.lsp - support code for XLisp version 3.0

(define ld load)
;(define ld load-noisily)

(ld "qquote.lsp")
(ld "macros.lsp")
(ld "math.lsp")
;(ld "clisp.lsp")
(ld "objects.lsp")
(ld "fasl.lsp")
(ld "crec.lsp")
(ld "pp.lsp")

; this version of EVAL knows about the optional enviroment parameter
(define (eval x &optional env)
  ((compile x env)))

(define basic-apply apply)
(define (apply f &rest args)
  (basic-apply f (basic-apply list* args)))

(define (autoload-from-file file syms &optional env)
  (map (lambda (sym) (put sym '%autoload file)) syms)
  '())
  
(define (*unbound-handler* sym cont)
  (let ((file (get sym '%autoload)))
    (if file (load file))
    (if (bound? sym)
      (cont '()))
    (error "unbound variable - ~S" sym)))

(define head car)
(define (tail x) (force (cdr x)))
(define empty-stream? null?)
(define the-empty-stream '())

(macro cons-stream
  (lambda (x)
    (list 'cons (cadr x) (list 'delay (caddr x)))))

(macro make-environment
  (lambda (x)
    (append '(let ()) (cdr x) '((the-environment)))))

(define initial-user-environment (the-environment))

(define (set-macro-character char fun &optional (non-terminating? #f) (table *readtable*))
  (let ((type (if non-terminating? 'non-terminating-macro 'terminating-macro)))
    (vector-set! table (char->integer char) (cons type fun))
    #t))
    
(define (get-macro-character char &optional (table *readtable*))
  (let ((entry (vector-ref table (char->integer char))))
    (when (and (pair? entry) (not (vector? (cdr entry))))
      (values (cdr entry) (eq? (car entry) 'nmacro)))))

(define (make-dispatch-macro-character char &optional (non-terminating? #f) (table *readtable*))
  (let ((type (if non-terminating? 'non-terminating-macro 'terminating-macro)))
    (vector-set! table (char->integer char) (cons type (make-vector 256)))
    #t))
  
(define (set-dispatch-macro-character dchar char fun &optional (table *readtable*))
  (let ((entry (vector-ref table (char->integer dchar))))
    (unless (vector? entry)
      (error "not a dispatch macro character ~S" dchar))
    (vector-set! entry (char->integer char) fun)
    #t))
    
(define (get-dispatch-macro-character dchar char &optional (table *readtable*))
  (let ((entry (vector-ref table (char->integer dchar))))
    (unless (and (pair? entry) (vector? (cdr entry)))
      (error "not a dispatch macro character ~S" dchar))
    (vector-ref (cdr entry) (char->integer char))))

(define (%get-method-list class)
  (%vector-ref class 2))
  
(define (%get-superclass class)
  (%vector-ref class 5))

(define (%find-method class selector)
  (let ((s (assoc selector (%get-method-list class))))
    (if s
	  (cdr s)
	  (let ((super (%get-superclass class)))
	    (if super
	      (%find-method super selector))))))

(define-macro (instruction-trace &body body)
  `(begin
     (trace-on)
     (unwind-protect
       (begin ,@body)
       (trace-off))))

(define (read-from-string str)
  (read (make-string-input-stream str)))

(define *editor* "ep")

(define (ed &optional file)
  (if file
    (system (string-append *editor*  " " file))
    (system *editor*)))

; load the files mentioned on the command line
(define (loader n)
  (let ((arg (getarg n)))
    (when arg
      (format #t "~&; Loading '~A'" arg)
      (when (not (load arg))
        (display " -- failed"))
      (loader (1+ n)))))

; read/eval/print loop history routines

(define *break-level* 0)
(define *history-stack-size* 20)

(define (setup-history size)
  (set! *history-n* 0)
  (set! *history-exprs* (make-vector size))
  (set! *history-values* (make-vector size)))

(setup-history *history-stack-size*)

(define (next-history-n)
  (+ *history-n* 1))

(define (store-history expr values)
  (push-history-value expr *history-exprs*)
  (push-history-value values *history-values*)
  (set! *history-n* (1+ *history-n*))
  *history-n*)

(define (push-history-value value vect)
  (let loop ((i (-1+ (vector-length vect))))
    (when (> i 0)
      (vector-set! vect i (vector-ref vect (-1+ i)))
      (loop (-1+ i))))
  (vector-set! vect 0 value)
  vect)

(define (get-history-value vect n)
  (let ((i (- *history-n* n)))
    (if (and (>= i 0) (< i (vector-length vect)))
      (vector-ref vect i)
      (values))))
	
; get an expression from the history stack
(define (%e n)
  (get-history-value *history-exprs* n))

; get a value from the history stack
(define (%v n &optional (i 0))
  (list-ref (get-history-value *history-values* n) i))

(define (*toplevel*)
  (let ((code (catch 'error
                (prompt-read-eval-print))))
    (unless (eq? code 'eof)
      (*toplevel*))))

(define (*breaklevel* env)
  (set! *break-level* (1+ *break-level*))
  (let ((prompt (format #f "Debug ~A> " *break-level*)))
    (let ((code (unwind-protect
                  (catch 'error
                    (prompt-read-eval-print env prompt))
                  (set! *break-level* (-1+ *break-level*)))))
      (case code
        (continue (*breaklevel* env))
        (cleanup (throw-error 'continue))
        (reset (throw-error 'reset))
	(eof 'eof)))))

(define (prompt-read-eval-print &optional (env (the-environment)) (prompt ""))
  (listener-prompt prompt)
  (read-eval-print env))

(define (listener-prompt &optional (prompt ""))
  (fresh-line)
  (format #t "~A[~S] " prompt (next-history-n))
  (flush-output))

(define (read-eval-print &optional (env (the-environment)))
  (let ((expr (read)))
    (if (eof-object? expr)
      'eof
      (eval-print expr env))))

(define (eval-print expr &optional (env (the-environment)))
  (let ((vals (multiple-value-list (eval expr env))))
    (store-history expr vals)
    (for-each (lambda (v) (fresh-line) (write v)) vals)
    'continue))

(define (reset)
  (throw-error 'reset))
  
(define (cleanup)
  (throw-error 'cleanup))
  
(define (*error-handler* fun env sp)
  (format t "~%Entering break loop ('(reset)' to quit)")
  (*breaklevel* env))
  
(define (*initialize*)
  (loader 1)
  (*toplevel*))
