;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the start of a widget hierarchy for
; Xlisp/Tk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; function to generate a new name
; only re-bind this if it is not already bound;
; this makes it feasible to re-load this file
; without tcl/tk getting confused over window names
;

(if (not (bound? 'tk-newname))
    (define tk-newname
      (let ((next-num 0))
	(lambda ()
	  (set! next-num (+ next-num 1))
	  (string-append "tkobj" (number->string next-num))))))

(define quote-dollars
  (lambda (s)
    (let ((first-dollar (string-search "$" s)))
      (if first-dollar
        (string-append (substring s 0 first-dollar)
                       "\\$"
		       (quote-dollars (substring s (1+ first-dollar))))
        s))))

;
; a function to quote strings for tcl's benefit (to produce an output
; string like "I said \"Hello\"!" for {I said "Hello"!})
; this also turns a lisp list into a tcl list
; i.e. (a b c) -> {a b c}
;
(if (not (bound? 'tcl-quote))
    (define (tcl-quote s)
      (if (list? s)
        (string-append "{" (tcl-quote-list s) "}")
	(quote-dollars (format #f "~S" s)))))

;
; function to quote a list
;
(if (not (bound? 'tcl-quote-list))
    (define (tcl-quote-list s)
      (if (null? s)
	  ""
	(let ((rest (cdr s)))
	 (if (null? rest)
	     (tcl-quote (car s))
	   (string-append (tcl-quote (car s)) " " (tcl-quote-list (cdr s)))))
	)))
;
; some utility functions
;

; convert symbols to/from lower case strings
(define (symbol->lcstring x) (string-downcase (symbol->string x)))
(define (lcstring->symbol x) (string->symbol (string-upcase x)))

; a function that does nothing
(define (*tk-nullfn*)() )

; options: if the argument "str" is non-NIL, then return the concatentation
; of "optname" and "str", otherwise return an empty string
;
;(define (-tk-string-option optname str)
;  (if str 
;      (format #f " ~A ~S" optname str)
;    ""))

; options: if the argument "str" is non-NIL, then return the concatentation
; of "optname" and "str", otherwise return an empty string; this option
; can be a list, so tcl-quote and string-append must be used rather
; than format

(define (-tk-string-option optname str)
  (if str 
      (string-append " " optname " " (tcl-quote str))
    ""))

; options: if the argument "win" is non-NIL, then return the concatentation
; of "optname" and the window's Tk name, otherwise return an empty string

(define (-tk-widget-option optname win)
  (if win (format #f " ~A ~A" optname (win 'tk-name)) ""))

; options: if the argument "img" is non-NIL, then return the concatentation
; of "optname" and the window's Tk name, otherwise return an empty string

(define (-tk-image-option optname img)
  (if img (format #f " ~A ~A" optname (img 'tk-name)) ""))


; options: a variable option

(define (-tk-variable-option optname img)
  (if img
      (format #f " ~A ~A" optname (img 'tk-name))
    ""))

; options: a text window index option
; an index may either be a tag name or a list (1 0) which gets translated to 1.0

(define (-tk-index-option optname index)
  (if
   (list? index)
    (format #f " ~A ~A.~A" optname (car index) (cadr index))
   ;else
    (if
	(eq? index 'begin)
	(format #f " ~A 1.0" optname)
      ; else
      (format #f " ~A ~S" optname index)))
)

;
; -tk-thunk turns a lisp command into a tcl command
;
(define (-tk-thunk command &key append break)
  (let ((newsym (tk-newname)))
    (set-symbol-value! (lcstring->symbol newsym) command)
    (string-append "{"
		   (if append "+" "")
		   "xlisp {" newsym "}"
		   (if break "; break" "")
		   "}")))

;
; -tk-thunk-prefix turns a lisp command into a tcl command prefix
;
(define (-tk-thunk-prefix command)
  (let ((newsym (tk-newname)))
    (set-symbol-value! (lcstring->symbol newsym) command)
    (string-append "xlscroll " newsym)))

;
; a function to work on either a single tk object or a list of such objects;
; if it is a single object, its name is returned, otherwise
; a space separated list of names is returned

(define (tk-single-name widget)
  (if (object? widget) (widget 'tk-name) widget))

(define (tk-name-list widgets)
  (cond
   ((null? widgets) "")
   ((list? widgets)
    (string-append (tk-single-name (car widgets)) " " (tk-name-list (cdr widgets))))
   (else (tk-single-name widgets))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; root of the Tk object hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class tk-object%
  (instance-variables
   tk-name))

(define-method (tk-object% 'tk-name)
  tk-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tcl variable class

(define-class tk-variable%
  (super tk-object%))

(define-method (tk-variable% 'initialize)
  (let ((iname (tk-newname)))
    (set! tk-name iname)
    (tcl "global " iname)
    (tcl "set " iname " " (tcl-quote ""))
    self))

(define-method (tk-variable% 'set-value! val)
  (tcl "set " (self 'tk-name) " " (tcl-quote val)))

(define-method (tk-variable% 'get-value)
  (tcl "set " (self 'tk-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; photograph/bitmap/image classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class tk-image%
  (super tk-object%))

(define-method (tk-image% 'initialize)
  (let ((iname (tk-newname)))
    (set! tk-name iname)
    self))

(define-class tk-photo%
  (super tk-image%))

(define-method (tk-photo% 'initialize)
  (super 'initialize)
  self)

(define-method (tk-photo% 'blank)
  (tcl (self 'tk-name) " blank"))

(define-method (tk-photo% 'redither)
  (tcl (self 'tk-name) " redither"))

(define-method (tk-photo% 'set-pixel x y color)
  (tcl (self 'tk-name) " put {{"
       color "}}" (-tk-string-option "-to" x) (-tk-string-option "" y)))

(define-method (tk-photo% 'read filename &key format)
  (tcl (self 'tk-name) " read "
       (-tk-string-name "-format" format)))
;
; an empty image (for explicitly turning images "off")
;
(define tk-null-image (tk-image% 'new))
(tk-null-image 'set-variable! 'tk-name "")

;
; function to create a new photo image
;
(define (tk-photo &key
		  data format file gamma height palette width)
  (let ((img (tk-photo% 'new)))
    (tcl "image create photo " (img 'tk-name)
	 (-tk-string-option "-data" data)
	 (-tk-string-option "-format" format)
	 (-tk-string-option "-file" file)
	 (-tk-string-option "-gamma" gamma)
	 (-tk-string-option "-height" height)
	 (-tk-string-option "-palette" palette)
	 (-tk-string-option "-width" width))
    img))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; basic widget class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class tk-widget%
  (super tk-object%)
  (instance-variables childlist parent))

; initialization method
(define-method (tk-widget% 'initialize par)
  (let ((wname 
	 (string-append
	  (if par (par 'tk-name) "")
	  "." (tk-newname))))
    (set! tk-name wname)
    (set! childlist '())
    (set! parent par)
    (set-symbol-value! (lcstring->symbol wname) self)
    (if par (par 'add-child self))
    self))

; method to change a widget's name
(define-method (tk-widget% 'change-name! newname)
  (set-symbol-value! (lcstring->symbol (self 'tk-name)) '())
  (set! tk-name newname)
  (set-symbol-value! (lcstring->symbol (self 'tk-name)) self)
  self)

; method to add a child
(define-method (tk-widget% 'add-child child)
  (set! childlist (append (list child) childlist))
  childlist)

; method to get one's parent
(define-method (tk-widget% 'parent)
  parent)

; method to destroy a widget
(define-method (tk-widget% 'destroy)
  (if (not (null? childlist))
      (map (lambda(p)(p 'destroy)) childlist))
  (set! childlist '())
  (set-symbol-value! (lcstring->symbol (self 'tk-name)) '())
)

; configuration method
; (common to all widgets)

(define-method
  (tk-widget% 'configure &key
	      activebg activefg activeborder anchor
	      bg bitmap border
	      command
	      disabledbg disabledfg
	      font fg 
	      height image
	      jump justify
	      menu
	      relief
	      selectbg selectfg state
	      text
	      length
	      width wrap
	      xscrollbar yscrollbar )
  (tcl (self 'tk-name) " configure "
       (if command
	   (begin
	    (self 'set-command! command)
	    (-tk-string-option "-command {xlisp " (self 'tk-name) " {'invoke}}"))
	 "")
       (-tk-string-option "-activebackground" activebg)
       (-tk-string-option "-activeforeground" activefg)
       (-tk-string-option "-activeborder" activeborder)
       (-tk-string-option "-anchor" anchor)
       (-tk-string-option "-bitmap" bitmap)
       (-tk-string-option "-background" bg)
       (-tk-string-option "-border" border)
       (-tk-string-option "-disabledbackground" disabledbg)
       (-tk-string-option "-disabledforeground" disabledfg)
       (-tk-string-option "-font" font)
       (-tk-string-option "-foreground" fg)
       (-tk-image-option "-image" image)
       (-tk-string-option "-jump" jump)
       (-tk-string-option "-justify" justify)
       (-tk-widget-option "-menu" menu)
       (-tk-string-option "-relief" relief)
       (-tk-string-option "-selectbackground" selectbg)
       (-tk-string-option "-selectforeground" selectfg)
       (-tk-string-option "-state" state)
       (-tk-string-option "-text" text)
       (-tk-string-option "-wrap" wrap)
       (-tk-string-option "-height" height)
       (-tk-string-option "-width" width)
       (-tk-string-option "-length" length)
       (if xscrollbar
	   (string-append " -xscrollcommand \"" (xscrollbar 'tk-name) " set\"") "")
       (if yscrollbar
	   (string-append " -yscrollcommand \"" (yscrollbar 'tk-name) " set\"") "")
       ))

;
; informational methods
;

; get a window's class
(define-method (tk-widget% 'get-class)
  (tcl "winfo class " (self 'tk-name)))

; get number of bits per pixel
(define-method (tk-widget% 'get-bpp)
  (string->number (tcl "winfo depth " (self 'tk-name))))

; get height in pixels
(define-method (tk-widget% 'get-height)
  (string->number (tcl "winfo height " (self 'tk-name))))

; get width in pixels
(define-method (tk-widget% 'get-width)
  (string->number (tcl "winfo width " (self 'tk-name))))

; get X position relative to screen
(define-method (tk-widget% 'get-rootx)
  (string->number (tcl "winfo rootx " (self 'tk-name))))

; get Y position relative to screen
(define-method (tk-widget% 'get-rooty)
  (string->number (tcl "winfo rooty " (self 'tk-name))))

; find the width of a window's screen, in pixels
(define-method (tk-widget% 'get-screen-width)
  (string->number (tcl "winfo screenwidth " (self 'tk-name))))

; find the height of a window's screen, in pixels
(define-method (tk-widget% 'get-screen-height)
  (string->number (tcl "winfo screenheight " (self 'tk-name))))


; get the widget containing coordinates X, Y
(define (tk-window-containing x y)
  (let ((which (tcl "winfo containing " (-tk-string-option "" x) (-tk-string-option "" y))))
    (if (string=? which "")
	nil
      (get-symbol-value (lcstring->symbol which)))))

; determine whether a window is mapped
(define-method (tk-widget% 'is-mapped?)
  (if (string=? (tcl "winfo ismapped " (self 'tk-name)) "0")
      #f
    #t))

; determine whether a window is viewable
(define-method (tk-widget% 'is-viewable?)
  (if (string=? (tcl "winfo viewable " (self 'tk-name)) "0")
      #f
    #t))

; raise a window
(define-method (tk-widget% 'raise)
  (tcl "raise " (self 'tk-name)))

; lower a window
(define-method (tk-widget% 'lower)
  (tcl "lower " (self 'tk-name)))

; generate an event for a window
(define-method (tk-widget% 'generate-event ename)
  (tcl "event generate " (self 'tk-name) (-tk-string-option "" ename)))

; give widget the input focus
(define-method (tk-widget% 'focus)
  (tcl "focus " (self 'tk-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; toplevel window class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class tk-toplevel%
  (super tk-widget%))

(define-method (tk-toplevel% 'initialize title)
  (super 'initialize nil)
  (tcl "toplevel " (self 'tk-name))
  (tcl "wm title " (self 'tk-name) " " (tcl-quote (string-append "XlispTk - " title)))
  (tcl "wm iconname " (self 'tk-name) " {xlisptk}")
  self)

(define (tk-toplevel title)
  (tk-toplevel% 'new title))

;
; variable used to trigger exit from a modal dialog
;
(define tk-modal-trigger (tk-variable% 'new))

(define (tk-set-modal-trigger!)
  (tk-modal-trigger 'set-value! "done"))

(define-method (tk-toplevel% 'modal)
  (tcl "grab set " (self 'tk-name))
  (tcl "vwait " (tk-modal-trigger 'tk-name))
  (tcl "grab release " (self 'tk-name))
  #t)

;
; methods to manage interaction with the window manager
;

; display the window in normal form (map it if it was previously withdrawn)
(define-method (tk-toplevel% 'deiconify)
  (tcl "wm deiconify " (self 'tk-name)))

; set focus model to either 'active or 'passive
(define-method (tk-toplevel% 'focusmodel which)
  (tcl "wm focusmodel " (self 'tk-name) (-tk-string-option "" which)))

; set geometry for the window
(define-method (tk-toplevel% 'set-geometry! x y w h)
  (tcl "wm geometry " (self 'tk-name)
       (format #f " =~Ax~A+~A+~A" w h x y)))

; get the geometry for the window
(define-method (tk-toplevel% 'get-geometry)
  (let* ((gstr (tcl "wm geometry " (self 'tk-name)))
	 (xpos (string-search "x" gstr))
	 (p1pos (string-search "+" gstr))
	 (wstr (substring gstr 0 xpos))
	 (hstr (substring gstr (+ xpos 1) p1pos))
	 (xystr (substring gstr (+ p1pos 1)))
	 (p2pos (string-search "+" xystr)))
    (list (string->number (substring xystr 0 p2pos))
	  (string->number (substring xystr (+ 1 p2pos)))
	  (string->number wstr)
	  (string->number hstr))))

; set just the position of the window
(define-method (tk-toplevel% 'set-position! x y)
  (tcl "wm geometry " (self 'tk-name)
       (format #f " +~A+~A" x y)))

; set just the dimensions of the window
(define-method (tk-toplevel% 'set-size! w h)
  (tcl "wm geometry " (self 'tk-name)
       (format #f " =~Ax~A" w h)))

; iconify a window
(define-method (tk-toplevel% 'iconify)
  (tcl "wm iconify " (self 'tk-name)))

; set the icon name for a window
(define-method (tk-toplevel% 'set-iconname! name)
  (tcl "wm iconname " (self 'tk-name) (-tk-string-option "" name)))

; get the icon name for a window
(define-method (tk-toplevel% 'get-iconname)
  (tcl "wm iconname " (self 'tk-name)))

; specify whether a window is to be managed by the window
; manager (0) or not (1)
(define-method (tk-toplevel% 'set-override-redirect! yesno)
  (tcl "wm overrideredirect " (self 'tk-name)
       (if yesno " 1" " 0")))

; get the window's title
(define-method (tk-toplevel% 'get-title)
  (tcl "wm title " (self 'tk-name)))

; set the window's title
(define-method (tk-toplevel% 'set-title! newname)
  (tcl "wm title " (self 'tk-name) (-tk-string-option "" newname)))

; mark window as a transient child of "parent"
(define-method (tk-toplevel% 'set-transient! newparent)
  (tcl "wm transient " (self 'tk-name) (-tk-widget-option "" newparent)))

; withdraw window mapping
(define-method (tk-toplevel% 'withdraw)
  (tcl "wm withdraw " (self 'tk-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; button class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class tk-button%
  (super tk-widget%)
  (instance-variables
   tk-cmd))

(define-method (tk-button% 'initialize parent)
  (super 'initialize parent)
  (set! tk-cmd *tk-nullfn*)
  self)

; method called when a button is pushed
(define-method (tk-button% 'invoke)
  (when tk-cmd
    (tk-cmd)))

; method to change a button's command
(define-method (tk-button% 'set-command! newcmd)
  (set! tk-cmd newcmd))

; function to make a button
(define (tk-button parent &key
		   activebg activefg activeborder anchor
		   bg bitmap border
		   disabledbg disabledfg
		   font fg image
		   justify
		   relief
		   text
		   wrap 
		   padx pady
		   command height width
		   )
  (let ((but (tk-button% 'new parent)))
    (if command
	(but 'set-variable! 'tk-cmd command))
    (tcl "button " (but 'tk-name)
	 " -command {xlisp " (but 'tk-name) " {'invoke}}"
	 (-tk-string-option "-activebackground" activebg)
	 (-tk-string-option "-activeforeground" activebg)
	 (-tk-string-option "-activeborder" activeborder)
	 (-tk-string-option "-anchor" anchor)
	 (-tk-string-option "-bg" bg)
	 (-tk-string-option "-bitmap" bitmap)
	 (-tk-string-option "-border" border)
	 (-tk-string-option "-disabledbackground" disabledbg)
	 (-tk-string-option "-disabledforeground" disabledfg)
	 (-tk-string-option "-font" font)
	 (-tk-string-option "-fg" fg)
	 (-tk-image-option "-image" image)
	 (-tk-string-option "-justify" justify)
	 (-tk-string-option "-relief" relief)
	 (-tk-string-option "-text" text)
	 (-tk-string-option "-wrap" wrap)
	 (-tk-string-option "-padx" padx)
	 (-tk-string-option "-pady" pady)
	 (-tk-string-option "-height" height)
	 (-tk-string-option "-width" width))
    but))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; radiobutton class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class tk-radiobutton%
  (super tk-button%))

(define-method (tk-radiobutton% 'initialize parent)
  (super 'initialize parent)
  self)

; inherits methods for 'invoke and 'set-command! from
; tk-button%

(define-method (tk-radiobutton% 'deselect)
  (tcl (self 'tk-name) " deselect"))

(define-method (tk-radiobutton% 'flash)
  (tcl (self 'tk-name) " flash"))

; function to make a radio button
(define (tk-radiobutton parent &key
		   activebg activefg activeborder anchor
		   bg bitmap border
		   disabledbg disabledfg
		   font fg image
		   justify
		   relief
		   text
		   wrap 
		   command height width
		   variable value indicatoron
		   )
  (let ((but (tk-radiobutton% 'new parent)))
    (if command
	(but 'set-variable! 'tk-cmd command))
    (tcl "radiobutton " (but 'tk-name)
	 " -command {xlisp " (but 'tk-name) " {'invoke}}"
	 (-tk-string-option "-activebackground" activebg)
	 (-tk-string-option "-activeforeground" activebg)
	 (-tk-string-option "-activeborder" activeborder)
	 (-tk-string-option "-anchor" anchor)
	 (-tk-string-option "-bg" bg)
	 (-tk-string-option "-bitmap" bitmap)
	 (-tk-string-option "-border" border)
	 (-tk-string-option "-disabledbackground" disabledbg)
	 (-tk-string-option "-disabledforeground" disabledfg)
	 (-tk-string-option "-font" font)
	 (-tk-string-option "-fg" fg)
	 (-tk-image-option "-image" image)
	 (-tk-string-option "-justify" justify)
	 (-tk-string-option "-relief" relief)
	 (-tk-string-option "-text" text)
	 (-tk-string-option "-wrap" wrap)
	 (-tk-string-option "-height" height)
	 (-tk-variable-option "-variable" variable)
	 (-tk-string-option "-value" value)
	 (-tk-string-option "-indicatoron" indicatoron)
	 (-tk-string-option "-width" width))
    but))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; checkbutton class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class tk-checkbutton%
  (super tk-button%))

(define-method (tk-checkbutton% 'initialize parent)
  (super 'initialize parent)
  self)

; inherits methods for 'invoke and 'set-command! from
; tk-button%

(define-method (tk-checkbutton% 'deselect)
  (tcl (self 'tk-name) " deselect"))

(define-method (tk-checkbutton% 'flash)
  (tcl (self 'tk-name) " flash"))

; function to make a check button
(define (tk-checkbutton parent &key
		   activebg activefg activeborder anchor
		   bg bitmap border
		   disabledbg disabledfg
		   font fg image
		   justify
		   relief
		   text
		   wrap 
		   command height width
		   variable indicatoron
		   )
  (let ((but (tk-checkbutton% 'new parent)))
    (if command
	(but 'set-variable! 'tk-cmd command))
    (tcl "checkbutton " (but 'tk-name)
	 " -command {xlisp " (but 'tk-name) " {'invoke}}"
	 (-tk-string-option "-activebackground" activebg)
	 (-tk-string-option "-activeforeground" activebg)
	 (-tk-string-option "-activeborder" activeborder)
	 (-tk-string-option "-anchor" anchor)
	 (-tk-string-option "-bg" bg)
	 (-tk-string-option "-bitmap" bitmap)
	 (-tk-string-option "-border" border)
	 (-tk-string-option "-disabledbackground" disabledbg)
	 (-tk-string-option "-disabledforeground" disabledfg)
	 (-tk-string-option "-font" font)
	 (-tk-string-option "-fg" fg)
	 (-tk-image-option "-image" image)
	 (-tk-string-option "-justify" justify)
	 (-tk-string-option "-relief" relief)
	 (-tk-string-option "-text" text)
	 (-tk-string-option "-wrap" wrap)
	 (-tk-string-option "-height" height)
	 (-tk-variable-option "-variable" variable)
	 (-tk-string-option "-indicatoron" indicatoron)
	 (-tk-string-option "-width" width))
    but))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; label class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class tk-label%
  (super tk-widget%))

(define-method (tk-label% 'initialize parent)
  (super 'initialize parent)
  self)

; function to make a label
(define (tk-label parent &key
		   activebg activefg activeborder anchor
		   bg bitmap border
		   disabledbg disabledfg
		   font fg image
		   jump justify
		   relief
		   text
		   wrap 
		   height width textvar
		   )
  (let ((lbl (tk-label% 'new parent)))
    (tcl "label " (lbl 'tk-name)
	 (-tk-string-option "-activebackground" activebg)
	 (-tk-string-option "-activeforeground" activebg)
	 (-tk-string-option "-activeborder" activeborder)
	 (-tk-string-option "-anchor" anchor)
	 (-tk-string-option "-bitmap" bitmap)
	 (-tk-string-option "-bg" bg)
	 (-tk-string-option "-border" border)
	 (-tk-string-option "-disabledbackground" disabledbg)
	 (-tk-string-option "-disabledforeground" disabledfg)
	 (-tk-string-option "-font" font)
	 (-tk-string-option "-fg" fg)
	 (-tk-image-option "-image" image)
	 (-tk-string-option "-jump" jump)
	 (-tk-string-option "-justify" justify)
	 (-tk-string-option "-relief" relief)
	 (-tk-string-option "-text" text)
	 (-tk-string-option "-wrap" wrap)
	 (-tk-string-option "-height" height)
	 (-tk-string-option "-width" width)
	 (-tk-variable-option "-textvar" textvar))
    lbl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; frame class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class tk-frame%
  (super tk-widget%))

(define-method (tk-frame% 'initialize parent)
  (super 'initialize parent)
  self)

; function to make a frame
(define (tk-frame parent &key
		   activebg activefg activeborder anchor
		   bg border
		   disabledbg disabledfg
		   font fg image
		   jump justify
		   relief
		   text
		   wrap 
		   class height width
		   )
  (let ((lbl (tk-frame% 'new parent)))
    (tcl "frame " (lbl 'tk-name)
	 (-tk-string-option "-activebackground" activebg)
	 (-tk-string-option "-activeforeground" activebg)
	 (-tk-string-option "-activeborder" activeborder)
	 (-tk-string-option "-anchor" anchor)
	 (-tk-string-option "-bg" bg)
	 (-tk-string-option "-border" border)
	 (-tk-string-option "-disabledbackground" disabledbg)
	 (-tk-string-option "-disabledforeground" disabledfg)
	 (-tk-string-option "-font" font)
	 (-tk-string-option "-fg" fg)
	 (-tk-image-option "-image" image)
	 (-tk-string-option "-jump" jump)
	 (-tk-string-option "-justify" justify)
	 (-tk-string-option "-relief" relief)
	 (-tk-string-option "-text" text)
	 (-tk-string-option "-wrap" wrap)
	 (-tk-string-option "-class" class)
	 (-tk-string-option "-height" height)
	 (-tk-string-option "-width" width))
    lbl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; entry widget class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class tk-entry%
  (super tk-widget%))

(define-method (tk-entry% 'initialize parent)
  (super 'initialize parent)
  self)

; methods for an entry window

; delete some characters
(define-method (tk-entry% 'delete first &optional last)
  (tcl (self 'tk-name) " delete "
       (tcl-quote first)
       (if last
	   (string-append " " (tcl-quote last))
	 "")))

; get the characters in the entry
(define-method (tk-entry% 'get)
  (tcl (self 'tk-name) " get"))

; display the cursor at a specified index
(define-method (tk-entry% 'icursor index)
  (tcl (self 'tk-name) " icursor " (tcl-quote index)))

; insert string after index, optionally with a tag attached
(define-method (tk-entry% 'insert index string)
  (tcl (self 'tk-name) " insert " (tcl-quote index) " " (tcl-quote string)))

; set view
(define-method (tk-entry% 'xview index)
  (tcl (self 'tk-name) " xview " (tcl-quote index)))

; function to make an entry window
(define (tk-entry parent &key
		   activebg activefg activeborder anchor
		   bg border
		   disabledbg disabledfg
		   font fg
		   jump justify
		   relief
		   text
		   variable
		   wrap 
		   show state width
		   )
  (let ((twin (tk-entry% 'new parent)))
    (tcl "entry " (twin 'tk-name)
	 (-tk-string-option "-activebackground" activebg)
	 (-tk-string-option "-activeforeground" activefg)
	 (-tk-string-option "-activeborder" activeborder)
	 (-tk-string-option "-anchor" anchor)
	 (-tk-string-option "-bg" bg)
	 (-tk-string-option "-border" border)
	 (-tk-string-option "-disabledbackground" disabledbg)
	 (-tk-string-option "-disabledforeground" disabledfg)
	 (-tk-string-option "-font" font)
	 (-tk-string-option "-fg" fg)
	 (-tk-string-option "-jump" jump)
	 (-tk-string-option "-justify" justify)
	 (-tk-string-option "-relief" relief)
	 (-tk-string-option "-text" text)
	 (-tk-variable-option "-textvariable" variable)
	 (-tk-string-option "-wrap" wrap)
	 (-tk-string-option "-show" show)
	 (-tk-string-option "-state" state)
	 (-tk-string-option "-width" width))
    twin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; text window class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class tk-text%
  (super tk-widget%))

(define-method (tk-text% 'initialize parent)
  (super 'initialize parent)
  self)

(define-method (tk-text% 'delete index1 index2)
  (tcl (self 'tk-name) " delete "
       (-tk-index-option "" index1)
       (-tk-index-option "" index2)))

(define-method (tk-text% 'insert index text &key tag)
  (tcl (self 'tk-name) " insert "
       (-tk-index-option "" index)
       (-tk-string-option "" text)
       (-tk-string-option "" tag)))

(define-method (tk-text% 'see index)
  (tcl (self 'tk-name) " see "
       (-tk-index-option "" index)))

(define-method (tk-text% 'tag-add tagname index1 index2 )
  (tcl (self 'tk-name) " tag add "
       (-tk-string-option "" tagname)
       (-tk-index-option "" index1)
       (-tk-index-option "" index2)
       ))

(define-method (tk-text% 'tag-remove tagname index1 index2 )
  (tcl (self 'tk-name) " tag remove "
       (-tk-string-option "" tagname)
       (-tk-index-option "" index1)
       (-tk-index-option "" index2)
       ))

(define-method (tk-text% 'tag-bind tagname event command)
  (tcl (self 'tk-name) " tag bind "
       (-tk-string-option "" tagname)
       (-tk-string-option "" event)
       " " (-tk-thunk command)))

(define-method (tk-text% 'tag-configure tagname &key
			 bg bgstipple border
			 fgstipple font fg
			 justify
			 offset overstrike relief
			 underline wrap)
  (tcl (self 'tk-name) " tag configure "
       (-tk-string-option "" tagname)
       (-tk-string-option "-background" bg)
       (-tk-string-option "-bgstipple" bgstipple)
       (-tk-string-option "-border" border)
       (-tk-string-option "-fgstipple" fgstipple)
       (-tk-string-option "-font" font)
       (-tk-string-option "-foreground" fg)
       (-tk-string-option "-justify" justify)
       (-tk-string-option "-offset" offset)
       (-tk-string-option "-overstrike" overstrike)
       (-tk-string-option "-relief" relief)
       (-tk-string-option "-underline" underline)
       (-tk-string-option "-wrap" wrap)
       ))

; function to make a text window
(define (tk-text parent &key
		   activebg activefg activeborder anchor
		   bg border
		   disabledbg disabledfg
		   font fg
		   jump justify
		   relief
		   text
		   wrap 
		   height state spacing1 spacing2 spacing3 width
		   )
  (let ((twin (tk-text% 'new parent)))
    (tcl "text " (twin 'tk-name)
	 (-tk-string-option "-activebackground" activebg)
	 (-tk-string-option "-activeforeground" activefg)
	 (-tk-string-option "-activeborder" activeborder)
	 (-tk-string-option "-anchor" anchor)
	 (-tk-string-option "-bg" bg)
	 (-tk-string-option "-border" border)
	 (-tk-string-option "-disabledbackground" disabledbg)
	 (-tk-string-option "-disabledforeground" disabledfg)
	 (-tk-string-option "-font" font)
	 (-tk-string-option "-fg" fg)
	 (-tk-string-option "-jump" jump)
	 (-tk-string-option "-justify" justify)
	 (-tk-string-option "-relief" relief)
	 (-tk-string-option "-text" text)
	 (-tk-string-option "-wrap" wrap)
	 (-tk-string-option "-height" height)
	 (-tk-string-option "-spacing1" spacing1)
	 (-tk-string-option "-spacing2" spacing2)
	 (-tk-string-option "-spacing3" spacing3)
	 (-tk-string-option "-state" state)
	 (-tk-string-option "-width" width))
    twin))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; listbox window class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class tk-listbox%
  (super tk-widget%))

(define-method (tk-listbox% 'initialize parent)
  (super 'initialize parent)
  (set! tk-cmd *tk-nullfn*)
  self)

(define-method (tk-listbox% 'tag-bind tagname event command)
  (tcl (self 'tk-name) " tag bind "
       (-tk-string-option "" tagname)
       (-tk-string-option "" event)
       " " (-tk-thunk command)))

(define-method (tk-listbox% 'delete index1 index2)
  (tcl (self 'tk-name) " delete "
       (-tk-index-option "" index1)
       (-tk-index-option "" index2)))

(define-method (tk-listbox% 'insert index text)
  (tcl (self 'tk-name) " insert "
       (-tk-index-option "" index)
       (-tk-string-option "" text)))


(define-method (tk-listbox% 'get-value)
  (tcl (self 'tk-name) " curselection"))

(define-method (tk-listbox% 'set-value! index)
  (when (string->number (self 'get-value))
	  (tcl (self 'tk-name) " selection clear " (-tk-index-option "" (string->number (self 'get-value)))))
  (tcl (self 'tk-name) " selection set " (-tk-index-option "" index)))


(define-method (tk-listbox% 'yview index)
  (tcl (self 'tk-name) " yview " (-tk-index-option "" index)))


(define-method (tk-listbox% 'tag-configure tagname &key
			 activate)
  (tcl (self 'tk-name) " tag configure "
       (-tk-string-option "" tagname)
       (-tk-string-option "-activate" activate)
       ))

; function to make a listbox window
(define (tk-listbox parent &key
		   bg borderWidth
		   cursor exportSelection
		   font fg
		   height
		   highlightBackground highlightThickness
		   relief
		   selectBackground selectForeground selectMode
		   setGrid takeFocus 
		   width
		   command
		   yscrollbar
		   xscrollbar
		   )
  (let ((twin (tk-listbox% 'new parent)))
    (tcl "listbox " (twin 'tk-name)
	 (-tk-string-option "-bg" bg)
	 (-tk-string-option "-borderWidth" borderWidth)
	 (-tk-string-option "-cursor" cursor)
	 (-tk-string-option "-exportSelction" exportSelection)
	 (-tk-string-option "-font" font)
	 (-tk-string-option "-fg" fg)
	 (-tk-string-option "-height" height)
	 (-tk-string-option "-highlightBackground" highlightBackground)
	 (-tk-string-option "-highlightThickness" highlightThickness)
	 (-tk-string-option "-relief" relief)
	 (-tk-string-option "-selectBackground" selectBackground)
	 (-tk-string-option "-selectForeground" selectForeground)
	 (-tk-string-option "-selectmode" selectMode)
	 (-tk-string-option "-setGrid" setGrid)
	 (-tk-string-option "-takeFocus" takeFocus)
	 (-tk-string-option "-width" width)
        (if xscrollbar
	   (string-append " -xscrollcommand \"" (xscrollbar 'tk-name) " set\"") "")
        (if yscrollbar
	   (string-append " -yscrollcommand \"" (yscrollbar 'tk-name) " set\"") "")
        )
     (if command
	(tk-bind twin "<ButtonRelease-1>" command))	
    twin))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; menu widget class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class tk-menu%
  (super tk-widget%)
  (instance-variables
   numentries
   commands))

(define-method (tk-menu% 'initialize parent)
  (super 'initialize parent)
  (set! numentries 0)
  (set! commands '())
  self)

(define-method (tk-menu% 'activate index)
  (tcl (self 'tk-name) " activate " (tcl-quote index)))

(define-method (tk-menu% 'add type &key
			 activebg activefg accelerator
			 bg bitmap
			 command
			 font
			 image indicatoron
			 label menu
			 selectimage state
			 underline
			 value variable
                         columnbreak
			 )
  (tcl (self 'tk-name) " add " (tcl-quote type)
       (-tk-string-option "-activebackground" activebg)
       (-tk-string-option "-activeforeground" activefg)
       (-tk-string-option "-accelerator" accelerator)
       (-tk-string-option "-bg" bg)
       (-tk-string-option "-bitmap" bitmap)
       (if command
	 (begin
	    (set! commands (append commands (list command)))
	    (set! numentries (1+ numentries))
	    (string-append " -command {xlisp " (self 'tk-name) " {'do-cmd} " 
			   (number->string (- numentries 1))
			   "}" )
	    )
	 (begin
	  (set! commands (append commands (list *tk-nullfn*)))
	  (set! numentries (1+ numentries))
	  "")
	 )
       (-tk-string-option "-font" font)
       (-tk-image-option  "-image" image)
       (-tk-string-option "-indicatoron" indicatoron)
       (-tk-string-option "-label" label)
       (-tk-widget-option "-menu" menu)
       (-tk-image-option  "-selectimage" selectimage)
       (-tk-string-option "-state" state)
       (-tk-string-option "-underline" underline)
       (-tk-string-option "-value" value)
       (-tk-variable-option "-variable" variable)
       (-tk-string-option "-columnbreak" columnbreak)
       )
  self)

(define-method (tk-menu% 'do-cmd index)
  (cond
   ((< index 0) #f)
   ((>= index numentries) #f)
   (else ((list-ref commands index)))))

(define-method (tk-menu% 'invoke index)
  (tcl (self 'tk-name) " invoke " (tcl-quote index)))

(define (tk-menu parent &key
                 menu-object
		 activebg activefg activeborder
		 bg border cursor
		 disabledbg disabledfg
		 font fg
		 relief tearoff specialname)
  (let ((menu (or menu-object (tk-menu% 'new parent))))
    (if specialname
	(menu 'change-name! (string-append (parent 'tk-name) "." specialname)))
    (tcl "menu " (menu 'tk-name)
	(-tk-string-option "-activebackground" activebg)
	(-tk-string-option "-activeforeground" activefg)
	(-tk-string-option "-activeborder" activeborder)
	(-tk-string-option "-bg" bg)
	(-tk-string-option "-border" border)
	(-tk-string-option "-cursor" cursor)
	(-tk-string-option "-disabledbackground" disabledbg)
	(-tk-string-option "-disabledforeground" disabledfg)
	(-tk-string-option "-font" font)
	(-tk-string-option "-fg" fg)
	(-tk-string-option "-relief" relief)
	(-tk-string-option "-tearoff" tearoff)
	)
    menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; menubutton widget class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class tk-menubutton%
  (super tk-widget%)
  (instance-variables
    menu))

; function to make a menubutton

(define (tk-menubutton parent &key text)
  (let* ((win (tk-menubutton% 'new parent))
         (menu (tk-menu% 'new win)))
    (tcl "menubutton " (win 'tk-name)
        (-tk-string-option "-text" text)
        (-tk-widget-option "-menu" menu)
        )
    (tk-menu win :menu-object menu :tearoff 0)
    (win 'set-menu! menu)
    win))

(define-method (tk-menubutton% 'menu)
  menu)

(define-method (tk-menubutton% 'set-menu! m)
  (set! menu m)
  self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; scrollbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class tk-scrollbar%
  (super tk-widget%))

(define-method (tk-scrollbar% 'initialize parent)
  (super 'initialize parent)
  self)

; function to make a scrollbar window

(define (tk-scrollbar parent &key
                   command
		   activebg activefg
		   bg border
		   fg height
		   jump
		   orient relief
		   activerelief width
		   scrollx scrolly
		   )
  (let ((win (tk-scrollbar% 'new parent)))
    (tcl "scrollbar " (win 'tk-name)
         (if command
	   (-tk-string-option "-command" (-tk-thunk-prefix command))
	   "")
	 (-tk-string-option "-activebackground" activebg)
	 (-tk-string-option "-activeforeground" activefg)
	 (-tk-string-option "-bg" bg)
	 (-tk-string-option "-border" border)
	 (-tk-string-option "-fg" fg)
	 (-tk-string-option "-height" height)
	 (-tk-string-option "-jump" jump)
	 (-tk-string-option "-orient" orient)
	 (-tk-string-option "-relief" relief)
	 (-tk-string-option "-activerelief" activerelief)
	 (-tk-string-option "-width" width)
	 (if scrollx
	     (string-append " -command \"" (scrollx 'tk-name) " xview\"") "")
	 (if scrolly
	     (string-append " -command \"" (scrolly 'tk-name) " yview\"") "")
	 )
    win))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; scale
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class tk-scale%
  (super tk-widget%)
  (instance-variables
    tk-cmd))

(define-method (tk-scale% 'initialize parent)
  (super 'initialize parent)
  self)

; method called when a scale is updated
(define-method (tk-scale% 'invoke new-value)
  (when tk-cmd
    (tk-cmd new-value)))

; method to change a scale's command
(define-method (tk-scale% 'set-command! newcmd)
  (set! tk-cmd newcmd))

; function to make a scrollbar window

(define (tk-scale parent &key
                   command
		   activebackground
                   background
                   bigincrement
                   borderwidth
                   cursor
                   digits
                   from
                   font
                   foreground
                   highlightbackground
                   highlightforeground
                   highlightthickness
                   label
                   length
                   orient
                   relief
                   repeatdelay
                   repeatinterval
                   resolution
                   showvalue
                   sliderlength
                   sliderrelief
                   state
                   takefocus
                   tickinterval
                   to
                   troughcolor
                   variable
                   width
                   bg
                   fg
		   )
  (let ((win (tk-scale% 'new parent)))
    (tcl "scale " (win 'tk-name)
         (if command
	   (-tk-string-option "-command" (-tk-thunk-prefix command))
	   "")
	 (-tk-string-option "-activebackground" activebackground)
	 (-tk-string-option "-background" background)
	 (-tk-string-option "-bigincrement" bigincrement)
	 (-tk-string-option "-borderwidth" borderwidth)
	 (-tk-string-option "-cursor" cursor)
	 (-tk-string-option "-digits" digits)
	 (-tk-string-option "-from" from)
	 (-tk-string-option "-font" font)
	 (-tk-string-option "-foreground" foreground)
	 (-tk-string-option "-highlightbackground" highlightbackground)
	 (-tk-string-option "-highlightforeground" highlightforeground)
	 (-tk-string-option "-highlightthickness" highlightthickness)
	 (-tk-string-option "-label" label)
	 (-tk-string-option "-length" length)
	 (-tk-string-option "-orient" orient)
	 (-tk-string-option "-relief" relief)
	 (-tk-string-option "-repeatdelay" repeatdelay)
	 (-tk-string-option "-repeatinterval" repeatinterval)
	 (-tk-string-option "-resolution" resolution)
	 (-tk-string-option "-showvalue" showvalue)
	 (-tk-string-option "-sliderlength" sliderlength)
	 (-tk-string-option "-sliderrelief" sliderrelief)
	 (-tk-string-option "-state" state)
	 (-tk-string-option "-takefocus" takefocus)
	 (-tk-string-option "-tickinterval" tickinterval)
	 (-tk-string-option "-to" to)
	 (-tk-string-option "-troughcolor" troughcolor)
	 (-tk-variable-option "-variable" variable)
	 (-tk-string-option "-width" width)
	 (-tk-string-option "-bg" bg)
	 (-tk-string-option "-fg" fg)
	 )
    win))

(define-method (tk-scale% 'get-value)
  (string->number (tcl (self 'tk-name) " get")))

(define-method (tk-scale% 'set-value! new-value)
  (tcl (self 'tk-name) (format #f " set ~A" new-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; miscellaneous Tk commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; command to "pack" a widget or a list of widgets
;   -- need to take this so it can actually take a list of arguments
;
(define (tk-pack widgets &key after anchor before expand fill in ipadx ipady padx pady side)
  (tcl "pack " (tk-name-list widgets)
       (-tk-widget-option "-after" after)
       (-tk-widget-option "-before" before)
       (-tk-string-option "-anchor" anchor)
       (-tk-string-option "-expand" expand)
       (-tk-string-option "-fill" fill)
       (-tk-widget-option "-in" in)
       (-tk-string-option "-ipadx" ipadx)
       (-tk-string-option "-ipady" ipady)
       (-tk-string-option "-padx" padx)
       (-tk-string-option "-pady" pady)
       (-tk-string-option "-side" side)))



;
; command to "grid" a widget or a list of widgets
;   -- need to take this so it can actually take a list of arguments
;
(define (tk-grid widgets &key column columnspan in ipadx ipady padx pady row rowspan sticky)
  (tcl "grid " (tk-name-list widgets)
       (-tk-string-option "-column" column)
       (-tk-string-option "-columnspan" columnspan)
       (-tk-widget-option "-in" in)
       (-tk-string-option "-ipadx" ipadx)
       (-tk-string-option "-ipady" ipady)
       (-tk-string-option "-padx" padx)
       (-tk-string-option "-pady" pady)
       (-tk-string-option "-row" row)
       (-tk-string-option "-rowspan" rowspan)
       (-tk-string-option "-sticky" sticky)))

;
; command to destroy a widget
;
(define (tk-destroy widget)
  (tcl "destroy " (widget 'tk-name))
  (widget 'destroy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; function to bind commands to events
; this can take two forms:
;  (tk-bind widget sequence command)
;  (tk-bind string sequence command)
; in the former case the command is bound to a specific widget, in the
; latter it is bound to a class name
;
; Also: command can be either a lisp function, or a string; if the
; latter, it is assumed to be a tcl command string
;
; an optional keyword, append, if true causes the command to be appended to
; the existing binding

(define (tk-bind tag sequence command &key append break)
  (let ((wname (if (object? tag) (tag 'tk-name) tag))
	(cmdstr (if (string? command) (tcl-quote command)
		  (-tk-thunk command :append append :break break))))
    (tcl "bind "
	 (-tk-string-option "" wname)
	 (-tk-string-option "" sequence)
	 " " cmdstr)))

;
; command to specify what order bindings should be checked in
;

(define (tk-bindtags window taglist)
  (tcl "bindtags " (-tk-widget-option "" window)
       (-tk-name-list taglist)))

;
; event object
;
(define-class tk-after-event%
  (super tk-object%)
  (instance-variables
   aftername
   cmd))

(define-method (tk-after-event% 'initialize the_cmd)
  (let ((iname (string-append "tk-thunk-" (tk-newname))))
    (set! tk-name iname)
    (set! aftername #f)
    (set! cmd the_cmd)
    (set-symbol-value! (lcstring->symbol iname) self)
    self))

(define-method (tk-after-event% 'schedule milliseconds)
  (set-symbol-value! (lcstring->symbol (self 'tk-name)) self)
  (if (not aftername)
      (set! aftername (tcl "after " (-tk-string-option "" milliseconds)
			   " {xlisp " (self 'tk-name) " {'trigger}}" )))
  aftername)

(define-method (tk-after-event% 'trigger)
  (set-symbol-value! (lcstring->symbol (self 'tk-name)) nil)
  (set! aftername #f)
  (cmd))

(define-method (tk-after-event% 'cancel)
  (if aftername
      (tcl "after cancel " aftername))
  (set! aftername #f)
  (set-symbol-value! (lcstring->symbol (self 'tk-name)) nil))

;
; command to schedule a function after some time
; returns a label that may be used to cancel the command
;
(define (tk-after milliseconds cmd)
  (let ((a (tk-after-event% 'new cmd)))
    (a 'schedule milliseconds)
    a))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; message boxes and dialogs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; command to put up a message box
;
(define (tk-message-box &key default icon message parent title type)
  (tcl "tk_messageBox "
       (-tk-string-option "-default" default)
       (-tk-string-option "-icon" icon)
       (-tk-string-option "-message" message)
       (-tk-widget-option "-parent" parent)
       (-tk-string-option "-title" title)
       (-tk-string-option "-type" type)))

;
; command to do OpenFile/SaveFile dialog boxes
;
(define (tk-get-open-file &key parent filetypes initialfile defaultextension initialdir title)
  (tcl "tk_getOpenFile "
       (-tk-widget-option "-parent" parent)
       (-tk-widget-option "-title" title)
       (-tk-string-option "-initialfile" initialfile)
       (-tk-string-option "-initialdir" initialdir)
       (-tk-string-option "-defaultextension" defaultextension)
       (-tk-string-option "-filetypes" filetypes)))


(define (tk-get-save-file &key parent filetypes initialfile defaultextension initialdir title)
  (tcl "tk_getSaveFile "
       (-tk-widget-option "-parent" parent)
       (-tk-widget-option "-title" title)
       (-tk-string-option "-initialfile" initialfile)
       (-tk-string-option "-initialdir" initialdir)
       (-tk-string-option "-defaultextension" defaultextension)
       (-tk-string-option "-filetypes" filetypes)))

;
; some predefined windows
;

; the top level Tk window
(define tk-root (tk-toplevel% 'make-instance))
(tk-root 'set-variable! 'tk-name ".")

; the frame that holds the listener window
(define tk-listener-frame (tk-frame% 'new tk-root))
(tk-listener-frame 'set-variable! 'tk-name ".xLispTk.lframe")

; the actual text widget in the listener window
(define tk-listener-text (tk-text% 'new tk-listener-frame))
(tk-listener-text 'set-variable! 'tk-name ".xLispTk.lframe.listener")






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; optionmenu class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class tk-optionmenu%
  (super tk-widget%)
  (instance-variables
   tk-cmd))

(define-method (tk-optionmenu% 'initialize parent)
  (super 'initialize parent)
  (set! tk-cmd *tk-nullfn*)
  self)

; method called when a button is pushed
(define-method (tk-optionmenu% 'invoke)
  (when tk-cmd
    (tk-cmd)))

; method to change a button's command
(define-method (tk-optionmenu% 'set-command! newcmd)
  (set! tk-cmd newcmd))

; function to make a button
(define (tk-optionmenu parent variable items &key command)
  (let ((but (tk-optionmenu% 'new parent)))
    (if command
	(but 'set-variable! 'tk-cmd command))
    (tcl "tk_optionMenu " (but 'tk-name)
	(-tk-variable-option "" variable) " "
	(tk-name-list items))
    but))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; canvas class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class tk-canvas%
  (super tk-widget%))

(define-method (tk-canvas% 'initialize parent)
  (super 'initialize parent)
  self)


(define-method (tk-canvas% 'poly coord-list &key fill outline smooth splinesteps stipple width)
  (tcl (self 'tk-name) " create poly " (tcl-quote-list coord-list)
	(-tk-string-option "-fill" fill)
	(-tk-string-option "-outline" outline)
	(-tk-string-option "-smooth" smooth)
	(-tk-string-option "-splinesteps" splinesteps)
	(-tk-string-option "-stipple" stipple)
	(-tk-string-option "-width" width))
  self)



(define-method (tk-canvas% 'line coord-list &key arrow fill capstyle joinstyle smooth splinesteps stipple width)
  (tcl (self 'tk-name) " create line " (tcl-quote-list coord-list)
	(-tk-string-option "-fill" fill)
	(-tk-string-option "-arrow" arrow)
	(-tk-string-option "-capstyle" capstyle)
	(-tk-string-option "-joinstyle" joinstyle)
	(-tk-string-option "-smooth" smooth)
	(-tk-string-option "-splinesteps" splinesteps)
	(-tk-string-option "-stipple" stipple)
	(-tk-string-option "-width" width))
 self)

(define-method (tk-canvas% 'oval x0 y0 x1 y1 &key fill outline stipple width)
  (tcl (self 'tk-name) " create oval " 
	(-tk-string-option "" x0)
	(-tk-string-option "" y0)
	(-tk-string-option "" x1)
	(-tk-string-option "" y1)
	(-tk-string-option "-fill" fill)
	(-tk-string-option "-outline" outline)
	(-tk-string-option "-stipple" stipple)
	(-tk-string-option "-width" width))
  self)

(define-method (tk-canvas% 'arc x0 y0 x1 y1 &key fill outline stipple width start style 
	outlinestipple extent)
  (tcl (self 'tk-name) " create arc " 
	(-tk-string-option "" x0)
	(-tk-string-option "" y0)
	(-tk-string-option "" x1)
	(-tk-string-option "" y1)
	(-tk-string-option "-fill" fill)
	(-tk-string-option "-outline" outline)
	(-tk-string-option "-outlinestipple" outlinestipple)
	(-tk-string-option "-start" start)
	(-tk-string-option "-style" style)
	(-tk-string-option "-extend" extent)
	(-tk-string-option "-stipple" stipple)
	(-tk-string-option "-width" width))
  self)


(define-method (tk-canvas% 'rect x0 y0 x1 y1 &key fill outline stipple width)
  (tcl (self 'tk-name) " create rect " 
	(-tk-string-option "" x0)
	(-tk-string-option "" y0)
	(-tk-string-option "" x1)
	(-tk-string-option "" y1)
	(-tk-string-option "-fill" fill)
	(-tk-string-option "-outline" outline)
	(-tk-string-option "-stipple" stipple)
	(-tk-string-option "-width" width))
  self)



(define-method (tk-canvas% 'text x y text-str &key fill font justify stipple width anchor)
  (tcl (self 'tk-name) " create text " 
	(-tk-string-option "" x)
	(-tk-string-option "" y)
	(-tk-string-option "-text" text-str)
	(-tk-string-option "-fill" fill)
	(-tk-string-option "-font" font)
	(-tk-string-option "-justify" justify)
	(-tk-string-option "-anchor" anchor)
	(-tk-string-option "-stipple" stipple)
	(-tk-string-option "-width" width))
  self)

(define-method (tk-canvas% 'move who x y)
  (tcl (self 'tk-name) " move " 
	 (-tk-string-option "" who)
	 (-tk-string-option "" x)
	 (-tk-string-option "" y))
  self)



(define-method (tk-canvas% 'postscript filename &key colormode height pageanchor pageheight
	pagewidth pagex pagey rotate width x y)
  (tcl (self 'tk-name) " postscript " 
	 (-tk-string-option "-file" filename)
	 (-tk-string-option "-colormode" colormode)
	 (-tk-string-option "-height" height)
	 (-tk-string-option "-pageanchor" pageanchor)
	 (-tk-string-option "-pageanchor" pageanchor)
	 (-tk-string-option "-pagewidth" pagewidth)
	 (-tk-string-option "-pageheight" pageheight)
	 (-tk-string-option "-pagex" pagex)
	 (-tk-string-option "-pagey" pagey)
	 (-tk-string-option "-rotate" rotate)
	 (-tk-string-option "-width" width)
	 (-tk-string-option "-x" x)
	 (-tk-string-option "-y" y))
  self)

; function to make a canvas
(define (tk-canvas parent &key
		bg borderwidth closeenough confine cursor height highlightbackground
		highlightcolor highlightthickness insertbackground insertborderwidth
		insertofftime insertontime relief scrollincrement scrollregion 
		selectbackground selectforeground selectborderwidth takefocus width xscrollincrement
		yscrollincrement xscrollbar yscrollbar
		   )
  (let ((can (tk-canvas% 'new parent)))
    (tcl "canvas " (can 'tk-name)

	(-tk-string-option "-bg" bg)
	(-tk-string-option "-borderwidth" borderwidth)
	(-tk-string-option "-closeenough" closeenough)
	(-tk-string-option "-confine" confine)
	(-tk-string-option "-cursor" cursor)
	(-tk-string-option "-height" height)
	(-tk-string-option "-highlightbackground" highlightbackground)
	(-tk-string-option "-highlightcolor" highlightcolor)
	(-tk-string-option "-highlightthickness" highlightthickness)
	(-tk-string-option "-insertbackground" insertbackground)
	(-tk-string-option "-insertborderwidth" insertborderwidth)
	(-tk-string-option "-insertofftime" insertofftime)
	(-tk-string-option "-insertontime" insertontime)
	(-tk-string-option "-relief" relief)
	(-tk-string-option "-scrollincrement" scrollincrement)
	(-tk-string-option "-scrollregion" scrollregion)
	(-tk-string-option "-selectbackground" selectbackground)
	(-tk-string-option "-selectforeground" selectforeground)
	(-tk-string-option "-selectborderwidth" selectborderwidth)
	(-tk-string-option "-takefocus" takefocus)
	(-tk-string-option "-width" width)
	(-tk-string-option "-xscrollincrement" xscrollincrement)
	(-tk-string-option "-yscrollincrement" yscrollincrement)
        (if xscrollbar
	   (string-append " -xscrollcommand \"" (xscrollbar 'tk-name) " set\"") "")
        (if yscrollbar
	   (string-append " -yscrollcommand \"" (yscrollbar 'tk-name) " set\"") "")
        )
    can))

