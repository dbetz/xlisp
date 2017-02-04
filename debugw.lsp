(define-class call-stack-list-box
  (superclass list-box)
  (instance-variables
    browser))

(define-method (call-stack-list-box 'initialize parent b &rest key-args)
  (set! browser b)
  (apply-super 'initialize parent :sort? #f :vscroll? #t :border? #t key-args)
  self)

(define-method (call-stack-list-box 'selection-changed)
  (browser 'show-frame (self 'get-selection))
  self)
  
(define-class environment-list-box
  (superclass list-box)
  (instance-variables
    browser))

(define-method (environment-list-box 'initialize parent b &rest key-args)
  (set! browser b)
  (apply-super 'initialize parent :vscroll? #t :border? #t key-args)
  self)
                         
(define-method (environment-list-box 'double-click)
  (format #t "~%Environment double click")
  self)
    
(define-class debug-browser
  (superclass dialog)
  (instance-variables
    running?
    stack-pointer
    call-stack-list
    argument-list
    environment-list
    listener-box))

(define-method (debug-browser 'initialize sp)
  (super 'initialize "Debug Browser")
  (set! running? #t)
  (set! stack-pointer sp)
  (let ((f (frame 'new self :side 'top :fill 'both)))
    (let ((f2 (frame 'new f :side 'left :fill 'both)))
      (static-text 'new f2 :label "Call Stack"
                           :width 100
                           :height 16
                           :anchor 'left)
      (set! call-stack-list
        (call-stack-list-box 'new f2 self :width 200
                                          :vscroll? #t
                                          :height 300
                                          :fill 'both)))
    (frame 'new f :side 'left :width 2 :fill 'y)
    (let ((f3 (frame 'new f :side 'left :fill 'both)))
      (static-text 'new f3 :label "Arguments"
                           :width 100
                           :height 16
                           :anchor 'left)
      (set! argument-list
        (environment-list-box 'new f3 self :sort? #f
                                           :vscroll? #t
                                           :width 200
                                           :height 300
                                           :fill 'both)))
    (frame 'new f :side 'left :width 2 :fill 'y)
    (let ((f4 (frame 'new f :side 'left :fill 'both)))
      (static-text 'new f4 :label "Environment"
                           :width 100
                           :height 16
                           :anchor 'left)
      (set! environment-list
        (environment-list-box 'new f4 self :sort? #t
                                           :vscroll? #t
                                           :width 200
                                           :height 300
                                           :fill 'both))))
  (set! listener-box (rich-edit-text 'new self :label "(under development)"
                                               :multiline? #t
                                               :vscroll? #t
                                               :hscroll? #t
                                               :auto-vscroll? #t
                                               :auto-hscroll? #t
                                               :side 'top
                                               :height 100
                                               :fill 'both))
  (let loop ((n 0))
    (let ((fcn (%get-stack-frame stack-pointer n)))
      (when fcn
        (call-stack-list 'add-string! (format #f "~S" fcn))
        (loop (1+ n)))))
  (self 'show-frame 0)
  (self 'show)
  (message-loop (lambda () running?))
  (format #t "~%Done with debug browser")
  self)

(define-method (debug-browser 'close)
  (set! running? #f)
  self)

(define (test)
  (debug-browser 'new (%get-stack-pointer)))

(define-method (debug-browser 'show-frame n)
  (if (call-stack-list 'set-selection! n)
    (multiple-value-bind (fcn env)
                         (%get-stack-frame stack-pointer n)
      (argument-list 'reset!)
      (environment-list 'reset!)
      (when env
        (let ((len (%vector-length env)))
          (let loop ((i 2))
             (when (< i len)
               (argument-list 'add-string! (format #f "~S" (%vector-ref env i)))
               (loop (1+ i))))))
      (map (lambda (binding)
             (environment-list 'add-string! (format #f "~S = ~S" (car binding)
                                                                 (cdr binding))))
           (get-flattened-environment (environment-parent env)))
      #t)
    #f))

(define (get-flattened-environment env)
  (let env-loop ((env env)
                 (env-bindings '()))
    (if env
      (let binding-loop ((bindings (environment-bindings env))
                         (env-bindings env-bindings))
        (if bindings
          (let ((binding (car bindings)))
            (binding-loop (cdr bindings)
                          (if (assoc (car binding) env-bindings)
                            env-bindings
                            (cons binding env-bindings))))
          (env-loop (environment-parent env) env-bindings)))
      env-bindings)))

(define (inspect value)
  (unless (object-browser 'find-browser value)
    (cond ((pair? value)
           (object-browser 'new (browser-list 'new value)))
          ((vector? value)
           (object-browser 'new (browser-vector 'new value)))
          ((object? value)
           (object-browser 'new (browser-object 'new value)))
          ((and value (environment? value))
           (object-browser 'new (browser-environment 'new value)))))
  (values))

(define-class browser-list-box
  (superclass list-box)
  (instance-variables
    browser
    obj))

(define-method (browser-list-box 'initialize parent o &rest args)
  (apply-super 'initialize parent :sort? #f args)
  (set! browser parent)
  (set! obj o)
  (obj 'fill self)
  self)

(define-method (browser-list-box 'double-click)
  (let ((sel (self 'get-selection)))
    (inspect (obj 'get (self 'get-item-data sel))))
  self)

(define-class browser-thing
  (instance-variables
    thing))

(define-method (browser-thing 'initialize t)
  (set! thing t)
  self)

(define-method (browser-thing 'thing)
  thing)

(define-method (browser-thing 'name)
  thing)

(define-class browser-list
  (superclass browser-thing))

(define-method (browser-list 'fill box)
  (let loop ((lst thing) (i 0))
    (when lst
      (let ((str (format #f "~A: ~S" i (car lst))))
        (box 'add-string! str i))
      (loop (cdr lst) (1+ i))))
  self)

(define-method (browser-list 'get i)
  (list-ref thing i))

(define-class browser-vector
  (superclass browser-thing))

(define-method (browser-vector 'fill box)
  (let ((len (vector-length thing)))
    (let loop ((i 0))
      (when (< i len)
        (let ((str (format #f "~A: ~S" i (vector-ref thing i))))
          (box 'add-string! str i))
        (loop (1+ i)))))
  self)

(define-method (browser-vector 'get i)
  (vector-ref thing i))

(define-class browser-object
  (superclass browser-thing))

(define-method (browser-object 'fill box)
  (let ((str (format #f "Class: ~S" (thing 'class))))
    (box 'add-string! str 0))
  (let loop ((bindings (thing 'instance-bindings))
             (i 1))
    (when bindings
      (let ((binding (car bindings)))
        (let ((str (format #f "~S = ~S" (car binding) (cdr binding))))
          (box 'add-string! str i)
          (loop (cdr bindings) (1+ i))))))
  self)

(define-method (browser-object 'get i)
  (if (= i 0)
    (thing 'class)
    (%vector-ref thing i)))

(define-class browser-environment
  (superclass browser-thing))

(define-method (browser-environment 'fill box)
  (let ((str (format #f "Parent: ~S" (environment-parent thing))))
    (box 'add-string! str 0))
  (let loop ((bindings (environment-bindings thing))
             (i 2))
    (when bindings
      (let ((binding (car bindings)))
        (let ((str (format #f "~S = ~S" (car binding) (cdr binding))))
          (box 'add-string! str i)
          (loop (cdr bindings) (1+ i))))))
  self)

(define-method (browser-environment 'get i)
  (if (= i 0)
    (environment-parent thing)
    (%vector-ref thing i)))

(define-class object-browser
  (superclass dialog)
  (class-variables
    (browsers '()))
  (instance-variables
    browser-thng
    browser-list))

(define-class-method (object-browser 'find-browser thing)
  (let loop ((browsers browsers))
    (if browsers
      (let ((browser (car browsers)))
        (if (eq? thing (browser 'thing))
          (begin
            (browser 'bring-to-top)
            browser)
          (loop (cdr browsers))))
      #f)))
  
(define-method (object-browser 'initialize bt)
  (set! browser-thng bt)
  (super 'initialize (format #f "~S" (browser-thng 'name)) :width 400 :height 400)
  (set! browser-list (browser-list-box 'new self browser-thng :vscroll? #t :fill 'both))
  (push! self browsers)
  (self 'refresh!)
  (self 'show)
  self)

(define-method (object-browser 'refresh!)
  (browser-list 'reset!)
  (browser-thng 'fill browser-list)
  self)

(define-method (object-browser 'close)
  (set! browsers (remove self browsers))
  self)

(define-method (object-browser 'thing)
  (browser-thng 'thing))
