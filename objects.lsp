#| macro to send a message to the superclass |#

(define-macro (super selector &rest args)
  `(%send-super %%class ,selector self ,@args))

(define-macro (apply-super selector &rest args)
  `(apply %send-super %%class ,selector self ,@args))

#|

(define-class foo
  (superclass bar)
  (instance-variables a b)
  (class-variables ((c 1)(d 2)))

|#

(define-macro (define-class class-name &body body)
  (let ((super '())
        (ivars '())
        (cvars '()))
    (let loop ((body body))
      (if body
        (let* ((form (car body))
               (keyword (car form))
               (args (cdr form)))
          (cond ((or (eq? keyword 'superclass)
		     (eq? keyword 'super-class)
		     (eq? keyword 'super))
	    	 (set! super (append super args)))
                ((or (eq? keyword 'instance-variables)
                     (eq? keyword 'ivars))
                 (set! ivars (append ivars args)))
                ((or (eq? keyword 'class-variables)
		     (eq? keyword 'cvars))
                 (set! cvars (append cvars args)))
                (else (error "unexpected define-class clause ~S" form)))
          (loop (cdr body)))))
    (let ((super-class (if super (car super) 'object)))
      (list 'begin
	(list 'let (list (list 'meta-class
                               (list 'class ''new
	                             '()
				     '()
				     'class
                                     ''class)))
          (list 'set! class-name (list 'meta-class ''new
                                         (list 'quote ivars)
                                         (list 'quasiquote
                                               (destructure-cvars cvars))
                                         super-class
				         (list 'quote class-name)))
	  (list 'meta-class ''%set-cvars! (list class-name ''%cvars))
	  class-name)))))

(define (destructure-cvars forms)
  (let ((cvars '()))
    (let loop ((forms forms))
      (if forms
        (let ((form (car forms)))
	  (if (pair? form)
	    (set! cvars (append cvars (list (list (car form)
                                            (list 'unquote (cadr form))))))
	    (set! cvars (append cvars `(,form))))
	  (loop (cdr forms)))))
    cvars))
#|

(define-method (foo 'do-something a b) ; foo is a class
  (list self a b))

|#

(define-macro (define-method proto &body body)
  (let ((class (car proto))
	(selector (cadr proto))
	(args (cddr proto))
	(body (%expand-list (convert-internal-definitions body)))
	(sel (gensym)))
    `(let ((,sel ,selector))
       (,class 'answer ,sel ',args ',body)
       ,sel)))

#|

(define-class-method (foo 'do-something a b) ; foo is a class
  (list self a b))

|#

(define-macro (define-class-method proto &body body)
  (let ((class (car proto))
        (selector (cadr proto))
        (args (cddr proto)))
    `(define-method ((,class 'class) ,selector ,@args)
       ,@body)))

(define-method (class '%cvars) cvars)
(define-method (class '%set-cvars! vars) (set! cvars vars))

#| some useful class methods |#

(define-method (class 'name) name)
(define-method (class 'ivars) ivars)

#| a method to show the class variables of a class |#

(define-method (class 'superclass)
  superclass)

(define-method (class 'show-cvars)
  (when cvars
    (let loop ((names (cdr (%vector-ref cvars 1)))
               (i 3))
      (if names
        (begin (fresh-line)
               (write (car names))
               (display " = ")
               (write (%vector-ref cvars i))
               (loop (cdr names) (+ i 1))))))
  self)

(define-method (class 'decompile sel)
  (let ((binding (assoc sel messages)))
    (if binding
      (decompile (cdr binding)))))

(define-method (class 'print &optional (stream *standard-output*))
  (let ((name (self 'name)))
    (if name
      (format stream "#<Class:~S #x~A>" name (%format-address class))
      (super 'print stream))))
