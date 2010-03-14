;;;; Data Directed programming and additivity
;;;; Section 2.4.3

#|
> ,open big-scheme
> ,load "/opt/local/share/scheme48-1.8/misc/sicp.scm"
> 
|#

;;; Many complex number constructors/selectors need square
(define (square x) (* x x))

;;; procedures for working with tagged data
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG")))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS")))

;;; The rectangular representation
(define (install-rectangular-package)
  ;; internal procedures
  (define (make-from-real-imag x y) (cons x y))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (/ (imag-part z) (real-part z))))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; Interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-angle 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;;; The polar package
(define (install-polar-package)
  ;; internal procedures
  (define (make-from-mag-ang r a) (cons r a))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (real-part z) 
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'make-from-mag-angle 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

;;; The apply-generic procedure
(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
	 (proc (get op type-tags)))
    (if proc
	(apply proc (map contents args))
	(error "No operation/method for these types -- APPLY-GENERIC"))))

;;; The generic selectors for complex numners
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;;; The generic selectors
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;;; Note - It looks like the get/put procedures in the sicp.scm file cannot 
;;; handle a name of the form '(rectangular). It works w/o problems with
;;; names of form 'rectangular
#|
> (get 'real-part '(rectangular)) ; this has a procedure as value!!
#f
> (get 'make-from-real-imag 'polar)
#{Procedure 9037 (unnamed in install-polar-package)}
>
|#

