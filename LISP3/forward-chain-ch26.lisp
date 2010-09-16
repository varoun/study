;;;; Rule based expert systems - Forward Chaining

;;; We begin with the prereqs -- Pattern Matchers and Streams

;;;;;;;;;;
;;; The 'match' procedure -- See pattern-match-ch24.lisp for details

(defun extract-variable (pattern-variable-expression)
  (second pattern-variable-expression))

(defun make-binding (variable datum)
  (list variable datum))

(defun add-binding (pattern-variable-expression datum bindings)
  (if (eq '_ (extract-variable pattern-variable-expression))
      bindings
      (cons 
       (make-binding (extract-variable pattern-variable-expression) datum)
       bindings)))

(defun find-binding (pattern-variable-expression bindings)
  (unless (eq '_ (extract-variable pattern-variable-expression))
    (assoc (extract-variable pattern-variable-expression) bindings)))

(defun extract-key (binding) (first binding))

(defun extract-value (binding) (second binding))

(defun elements-p (p d)
  (and (atom p) (atom d)))

(defun variable-p (p)
  (and (listp p) (eq (first p) '?)))

(defun recursive-p (p d)
  (and (listp p) (listp d)))


(defun match-atoms (p d bindings)
  (if (eql p d)
      bindings
      'fail))

(defun match-variable (p d bindings)
  (let ((binding (find-binding p bindings)))
    (if binding
	(match (extract-value binding) d bindings)
	(add-binding p d bindings))))

(defun match-pieces (p d bindings)
  (let ((result (match (first p) (first d) bindings)))
    (if (eq result 'fail)
	'fail
	(match (rest p) (rest d) result))))

(defun match (p d &optional bindings)
  (cond ((elements-p p d)
	 (match-atoms p d bindings))
	((variable-p p)
	 (match-variable p d bindings))
	((recursive-p p d)
	 (match-pieces p d bindings))
	(t 'fail)))
;;;;;;;;;;

;;; Streams
;; For simplicity, we dont use lazy streams

;; Constructors and selectors
(defun stream-endp (stream) (eq stream 'empty-stream))

(defun stream-first (stream) (first stream))

(defun stream-rest (stream) (second stream))

(defun stream-cons (object stream) (list object stream))

;; Procedures for working with streams

(defun stream-append (stream1 stream2)
  (if (stream-endp stream1)
      stream2
      (stream-cons (stream-first stream1)
		   (stream-append (stream-rest stream1) stream2))))
#|
CL-USER> (setf stream1 (stream-cons 'object-a
				    (stream-cons 'object-b 'empty-stream)))
(OBJECT-A (OBJECT-B EMPTY-STREAM))
CL-USER> (setf stream2 (stream-cons 'object-x
				    (stream-cons 'object-y 'empty-stream)))
(OBJECT-X (OBJECT-Y EMPTY-STREAM))
CL-USER> (stream-append stream1 stream2)
(OBJECT-A (OBJECT-B (OBJECT-X (OBJECT-Y EMPTY-STREAM))))
CL-USER> 
|#

;;; This version of stream-concatenate is a more straightforward than the
;;; one listed in the book on Pg 369. It would be much better if we had
;;; a type predicate to test for streams, aka streamp
(defun stream-concatenate (stream-of-streams)
  (if (stream-endp stream-of-streams)
      'empty-stream
      (stream-append (stream-first stream-of-streams)
		     (stream-concatenate (stream-rest stream-of-streams)))))
#|
CL-USER> (setf stream-of-streams (stream-cons stream1 
					      (stream-cons stream2
							   'empty-stream)))

((OBJECT-A (OBJECT-B EMPTY-STREAM)) ((OBJECT-X (OBJECT-Y EMPTY-STREAM)) EMPTY-STREAM))
CL-USER> (stream-concatenate stream-of-streams)
(OBJECT-A (OBJECT-B (OBJECT-X (OBJECT-Y EMPTY-STREAM))))
CL-USER> 
|#

;;; stream-transform is analogous to mapcar
(defun stream-transform (procedure stream)
  (if (stream-endp stream)
      'empty-stream
      (stream-cons (funcall procedure (first stream))
		   (stream-transform procedure (stream-rest stream)))))
#|
CL-USER> (setf number-stream (stream-cons 2 (stream-cons 4 'empty-stream)))
(2 (4 EMPTY-STREAM))
CL-USER> (stream-transform #'(lambda (n) (expt n 2)) number-stream)
(4 (16 EMPTY-STREAM))
CL-USER> 
|#

;;; stream-member
(defun stream-member (object stream)
  (cond ((stream-endp stream) nil)
	((equal object (stream-first stream)) t) ; test uses equal vs eql
	(t (stream-member object (stream-rest stream)))))

;;; stream-remember 
(defmacro stream-remember (object variable)
  "Adds the object to the end of the stream thats bound to the variable provided its not already in there"
  `(unless (stream-member ,object ,variable)
     (setf ,variable 
	   (stream-append ,variable (stream-cons ,object 'empty-stream)))
     ,object))
#|
CL-USER> (setf long-stream (stream-append stream1 stream2))
(OBJECT-A (OBJECT-B (OBJECT-X (OBJECT-Y EMPTY-STREAM))))
CL-USER> (stream-remember 'last-object long-stream)
LAST-OBJECT
CL-USER> long-stream
(OBJECT-A (OBJECT-B (OBJECT-X (OBJECT-Y (LAST-OBJECT EMPTY-STREAM)))))
CL-USER> (stream-remember 'last-object long-stream)
NIL
CL-USER> 
|#


	 