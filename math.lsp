#|

Portions of this file are:

Copyright (c) 1989-94 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

(define (gcd . integers)
  (fold-left gcd2 0 integers))

(define (lcm . integers)
  (fold-left lcm2 1 integers))

(define (gcd2 n m)
  (let loop ((n n) (m m))
    (cond ((not (zero? m)) (loop m (remainder n m)))
	  ((negative? n) (- n))
	  (else n))))

(define (lcm2 n m)
  (if (or (zero? n) (zero? m))
    0
    (quotient (let ((n (* n m)))
		(if (negative? n)
		  (- n)
		  n))
	      (gcd2 n m))))

(define (fold-left procedure initial olist)
  (let fold ((initial initial)
	     (list olist))
    (if (pair? list)
	(fold (procedure initial (car list))
	      (cdr list))
	(begin
	  (if (not (null? list))
	      (error "wrong-type-argument ~S" olist))
	  initial))))

