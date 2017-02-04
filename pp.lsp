;;; hacking by mh, 11/14/94

;;; Macros must come before first usage:

(define-macro (pp-push *item *stack)
  `(set! ,*stack (cons ,*item ,*stack)) )
 
 
(define-macro (pp-pop *stack)
  `(let ((top* (car ,*stack)))
     (set! ,*stack (cdr ,*stack))
     top*) )
 
 


;+
;               PP 1.0 : (C) Copyright 1985 by Gregory Frascadore
;
;   This software may be copied, modified, and distributed to others as long
;   as it is not sold for profit, and as long as this copyright notice is
;   retained intact. For further information contact the author at:
;               frascado%umn-cs.CSNET   (on CSNET)
;               75106,662               (on CompuServe)
;-
 
;+
;                               PP 1.0
; DESCRIPTION
;   PP is a function for producing pretty-printed XLISP code. Version 1.0
;   works with XLISP 1.4 and may work with other versions of XLISP or other
;   lisp systems.
;
; UPDATE HISTORY
;   Version 1.0 - Original version, 11 April 1985 by Gregory Frascadore.
;
;-
 
;+
; pp
;   This function pretty-prints an s-expression.
;
; format
;   (pp <expr> [<sink>] )
;
;       <expr>  the expression to print.
;       <sink>  optional. the sink to print to. defaults to
;                   *standard-output*
;       <maxlen> the threshold that pp uses to determine when an expr
;                   should be broken into several lines. The smaller the
;                   value, the more lines are used. Defaults to 45 which
;                   seems reasonable and works well too.
;-


(set! pp-stack* nil
      pp-istack* nil
      pp-currentpos* nil
      pp-sink* nil
      pp-maxlen* nil)
 
(define (pp *expr &optional (*sink *standard-output*) (*maxlen 45))
   (fluid-let ((pp-stack* nil)
               (pp-istack* '(0))
               (pp-currentpos* 0)
               (pp-sink* *sink)
               (pp-maxlen* *maxlen))
      (pp-newline)
      (pp-expr *expr)
      (values)))

(define (pp1 *expr &optional (*sink *standard-output*) (*maxlen 45))
   (fluid-let ((pp-stack* nil)
               (pp-istack* '(0))
               (pp-currentpos* 0)
               (pp-sink* *sink)
               (pp-maxlen* *maxlen))
      (pp-expr *expr)
      (values)))
 
 
(define (pp-expr *expr)
   (cond ((pair? *expr)
            (pp-list *expr) )
 
         (else (pp-write *expr)) ) )
 
 
;+
; pp-list
;   Pretty-print a list expression.
;       IF <the write-size length of *expr is less than pp-maxlen*>
;           THEN print the expression on one line,
;       ELSE
;       IF <the car of the expression is an atom>
;           THEN print the expression in the following form:
;                   "(atom <item1>
;                          <item2>
;                           ...
;                          <itemn> )"
;       ELSE
;       IF <the car of the expression is a list>
;           THEN print the expression in the following form:
;                   "(<list1>
;                     <item2>
;                       ...
;                     <itemn> )"
;
;-
 

(define (pp-list *expr)
   (cond ((< (write-size *expr) pp-maxlen*)
            (pp-write *expr) )
 
         ((atom? (car *expr))
            (case (car *expr)
              ((define lambda named-lambda)
               (pp-pushmargin (+ pp-currentpos* 2))
               (pp-start)
               (pp-write (car *expr))
               (pp-display " "))
              (else
               (pp-start)
               (pp-write (car *expr))
               (pp-display " ")
               (pp-pushmargin)))
            (pp-rest (cdr *expr))
            (pp-popmargin)
            (pp-finish) )
 
         (else
            (pp-start)
            (pp-pushmargin)
            (pp-rest *expr)
            (pp-popmargin)
            (pp-finish) ) ) )
 
;+
; pp-rest
;   pp-expr each element of a list and do a pp-newline after every call to
;   pp-expr except the last.
;-
 
#|
(define (pp-rest *rest)
   (do* ((item* *rest (cdr item*)))
        ((null? item*))
            (pp-expr (car item*))
            (if (not (null? (cdr item*))) (pp-newline)) ) )
|#

(define (pp-rest *rest)
  (let loop ((item* *rest))
    (unless (null? item*)
      (if (atom? item*)
        (begin
          (pp-display ".")
          (pp-newline)
          (pp-expr item*))
        (begin
          (pp-expr (car item*))
          (if (not (null? (cdr item*))) (pp-newline))
          (loop (cdr item*)))))))

;+
; pp-newline
;   Print out a newline character and indent to the current margin setting
;   which is maintained at the top of the pp-istack. Note that is the
;   current top of the pp-stack* is a ")" we push a " " so that we will know
;   to print a space before closing any parenthesis which were started on a
;   different line from the one they are being closed on.
;-
 
(define (pp-newline)
   (if (eqv? ")" (pp-top pp-stack*)) (pp-push " " pp-stack*))
 
   (newline pp-sink*)
   (spaces (pp-top pp-istack*) pp-sink*)
   (set! pp-currentpos* (pp-top pp-istack*)) )
 
;+
; pp-finish
;   Print out the closing ")". If the top of the pp-stack* has a " " on it,
;   then print out the space, then the ")" , and then pop both off the stack.
;-
 
(define (pp-finish)
   (cond ((eqv? ")" (pp-top pp-stack*))
            (pp-display ")") )
 
         (else
            (pp-display " )")
            (pp-pop pp-stack*) ) )
 
   (pp-pop pp-stack*) )
 
;;#|
 
;+
; pp-start
;   Start printing a list. ie print the "(" and push a ")" on the pp-stack*
;   so that pp-finish knows to print a ")" when closing an list.
;-
 
(define (pp-start)
   (pp-display "(")
   (pp-push ")" pp-stack*) )
 
;+
; pp-display
;   Prints out an expr without any quotes and updates the pp-currentpos*
;   pointer so that we know where on the line the cursor is at.
;-
 
(define (pp-display *expr)
    (set! pp-currentpos* (+ pp-currentpos* (display-size *expr)))
    (display *expr pp-sink*) )
 
;+
; pp-write
;   Does the same thing as pp-write, except that the expr is printed with
;   quotes if needed. Hence pp-write uses write-size to calc expr length instead
;   of display-size.
;-
 

(define (pp-write *expr)
    (set! pp-currentpos* (+ pp-currentpos* (write-size *expr)))
    (write *expr pp-sink*) )
 
(define (pp-top *stack) (car *stack))
 
 
(define (pp-pushmargin &optional (new-margin pp-currentpos*))
   (pp-push new-margin pp-istack*) )
 
 
(define (pp-popmargin)
   (pp-pop pp-istack*) )

#|
(define (spaces n f)
    (dotimes (x n) (write-char 32 f)))
|#

(define (spaces n f)
  (let loop ((i 0))
    (if (< i n)
      (begin
        (write-char #\space f)
	(loop (1+ i))))))