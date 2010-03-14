;;;; Representing complex numbers - Section 2.4.1

;;; Complex numbers may be represented in either the rectangular or polar
;;; forms.

;;; Complex number operations that use abstract-data specified by
;;; the various constructors/selectors

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitide z1) (* magnitude z2))
		     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))

;;; Representations can be a pair of numbers that represent either the
;;; rectangular or polar forms. We can use _either_ on of the two.

;;; Rectangular representation
;;; The pair of numbers is the real and imag parts

#|
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitide z) 
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (/ (imag-part z) (real-part z))))

(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))
|#

;;; Polar representation
;;; The pair of numbers are the magnitude and angle

#|
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (real-part z) 
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (make-from-mag-ang r a) (cons r a))
(define (make-from-real-imag x y) 
  (cons (sqrt (+ square x) (square y))
	(atan (/ y x))))
|#


;;; The complex number arithmetic operations (add-complex etc) will
;;; work with both representations, but only one can be used at a time
;;; because we lack the ability to tell whether a pair (1 2) is 
;;; a complex number in the rectangular or polar form!

;;;; Tagged data - Section 2.4.2

;;; In order to use both representations simultaneously, we'll need to 
;;; tag the data with its type, and change the names used in the 
;;; react/polar representations

;;; Working with type-tags 

(define (attach-tag type-tag contents)
  (list type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG")))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS")))

;;; Predicates to figure out if the datum is in rectangular or polar form
(define (rectangular? datum)
  (eq? (type-tag datum) 'rectangular))
(define (polar? datum)
  (eq? (type-tag datum) 'polar))

;;; The selectors/constructors in the rectangular and polar form need
;;; to be renamed (example - with a -rectangular and a -polar) suffix
;;; to prevent name clashes. The constructors need to be rewritten to tag
;;; the data they create

;;; rectangular representation
(define (real-part-reactangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z)) 
	   (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (/ (imag-part-rectangular z)
	   (real-part-rectanguar z))))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang r a)
  (attach-tag 'rectangular 
	      (cons (* r (cos a)) (* r (sin a)))))

;;; polar-representation
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (make-from-mag-ang-polar r a) 
  (attach-tag 'polar (cons r a)))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
	      (cons (sqrt (+ square x) (square y))
		    (atan (/ y x)))))

;;; The generic selectors and constructors - these look a the tag
;;; and call the rectangular/polar versions of the selector/constructor

(define (real-part z)
  (cond ((rectangular? z)
	 (real-part-rectangular (contents z)))
	((polar? z)
	 (real-part-polar (contents z)))
	(else (error "Unknown type -- REAL_PART"))))
(define (imag-part z)
  (cond ((reactangular? z)
	 (imag-part-rectangular (contents z)))
	((polar? z)
	 (imag-part-polar (contents z)))
	(else (error "Unknown type - IMAG-PART"))))
(define (magnitude z)
  (cond ((rectangular? z)
	 (magnitude-rectangular (contents z)))
	((polar? z)
	 (magnitude-polar (contents z)))
	(else (error "Unknown type -- MAGNITUDE"))))
(define (angle z)
  (cond ((rectangular? z)
	 (angle-rectangular (contents z)))
	((polar? z)
	 (angle-polar (contents z)))
	(else (error "Unknown type - ANGLE"))))
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;;; Note that the original add-complex etc need no modification

 

  