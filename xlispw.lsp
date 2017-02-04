; xlispw.lsp - initialization code for XLispW version 3.0

(set-debug-mode! #t)

; load the xlisp support code
(load "xlinit.lsp")
(load "debugw.lsp")

(define-class listener
  (superclass window)
  (instance-variables
    edit-control
    delete-count
    max-size))

(define-method (listener 'initialize)
  (super 'initialize "Listener" :width 600 :height 400)
  (set! edit-control (rich-edit-text 'new self
                            :label ""
                            :border? #f
                            :multiline? #t
                            :vscroll? #t
                            :hscroll? #t
                            :auto-vscroll? #t
                            :auto-hscroll? #t
                            :fill 'both))
  (edit-control 'setup-listener)
  (set! delete-count 2000)
  (set! max-size 20000)
  (self 'show)
  self)

(define-method (listener 'close)
  (exit))

(define-method (listener 'eval-selection)
  (multiple-value-bind (start end)
                       (edit-control 'get-selection)
    (let ((expr-string
            (if (= start end)
              (begin
			    (fresh-line)
				(let loop ((line (edit-control 'line-from-char end)))
                  (let ((line-start (edit-control 'line-index line)))
                    (edit-control 'set-selection! line-start end)
                    (let ((expr-string (collect-expression (edit-control 'get-selected-text))))
                      (or expr-string
                        (if (= line 0)
                          #f
                          (loop (- line 1))))))))
              (if (> end start)
                (edit-control 'get-selected-text)
                #f))))
      (when expr-string
        (let ((expr (read-from-string expr-string)))
          (edit-control 'set-selection! end end)
          (eval-print expr)
          (listener-prompt))))))

(define (read-from-string str)
  (read (make-string-input-stream str)))
      
(define-method (listener 'guarantee-space)
  (let* ((line-count (edit-control 'get-line-count))
         (char-count (edit-control 'line-index (- line-count 1))))
    (when (> char-count max-size)
      (let* ((line-to-delete (edit-control 'line-from-char delete-count))
             (end-delete (edit-control 'line-index line-to-delete)))
        (multiple-value-bind (start end)
                             (edit-control 'get-selection)
          (let ((save-start (max 0 (- start end-delete)))
                (save-end (max 0 (- end end-delete))))
            (edit-control 'set-selection! 0 end-delete)
            (edit-control 'clear-selection!)
            (edit-control 'set-selection! save-start save-end)))))))

; create the listener window
(define *listener* (listener 'new))

(*listener* 'add-accelerator! "Ctrl-Enter" (lambda () (*listener* 'eval-selection)))

; load a file
(define (file-load)
  (let ((file (*listener* 'open-file-dialog
	        :filter "Lisp (*.lsp)|*.lsp|All Files (*.*)|*.*")))
    (when file
      (load file))))

; load a file noisily
(define (file-load-noisily)
  (let ((file (*listener* 'open-file-dialog
	        :filter "Lisp (*.lsp)|*.lsp|All Files (*.*)|*.*")))
    (when file
      (load-noisily file))))

; quit
(define (file-exit)
  (exit))

; create the 'file' menu
(define *file-menu* (menu 'new))
(*file-menu* 'append-string! "Load..." file-load)
(*file-menu* 'append-string! "Load Noisily..." file-load-noisily)
(*file-menu* 'append-separator!)
(*file-menu* 'append-string! "E&xit" file-exit)

; create the menu bar
(define *menu-bar* (menu 'new))
(*menu-bar* 'append-popup! "&File" *file-menu*)

; set as the listener menu
(*listener* 'set-menu! *menu-bar*)

; setup the error handler
(define (*error-handler* fcn env sp)
  (debug-browser 'new sp))

; load any user extensions
(load "local.ini")

; load the files mentioned on the command line
(loader 1)

