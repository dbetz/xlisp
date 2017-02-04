; xlisptk.lsp

; load the standard xlisp files
(load "xlinit.lsp")

; called by the tcl listener code (broken at the moment)
(define (tcl-eval-print str)
  (let ((expr (read-from-string str)))
    (eval-print expr)
    (listener-prompt)))
    
; tcl file loader
(define (tcl-load file)
  (let loop ((path *load-path*))
    (when path
      (let* ((full-name (format #f "~A~A" (car path) file))
             (port (open-input-file full-name)))
        (if port
          (begin
            (close-port port)
            (format #t "~%Loading tcl file ~S" full-name)
            (tcl (format #f "source ~S" full-name)))
          (loop (cdr path)))))))
               
; load the widget classes
(load "tk.lsp")

; start the gui
(tcl-load "xlisptk.tcl")

; the error handler doesn't work with the current listener window
(set! *error-handler* (lambda (fun env sp) (show-stack)))
