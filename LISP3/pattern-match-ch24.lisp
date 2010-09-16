;;;; Symbolic Pattern Matching -- Chapter 24

;;; Adding, finding and extracting pattern-variable bindings

(defun add-binding (pattern-variable-expression datum bindings)
  (if (eq '_ (extract-variable pattern-variable-expression))
      bindings
      (cons (make-binding 
	     (extract-variable pattern-variable-expression)
	     datum)
	    bindings)))
#|
CL-USER> (add-binding '(? x) 'apple '((y red)))
((X APPLE) (Y RED))
CL-USER> (add-binding '(? _) 'apple '((y red)))
((Y RED))
CL-USER> 
|#

(defun extract-variable (pattern-variable-expression)
  (second pattern-variable-expression))

(defun make-binding (variable datum)
  (list variable datum))

(defun find-binding (pattern-variable-expression bindings)
  (unless (eq '_ (extract-variable pattern-variable-expression))
    (assoc (extract-variable pattern-variable-expression) bindings)))

#|
CL-USER> (find-binding '(? x) '((y red) (x apple)))
(X APPLE)
CL-USER> (find-binding '(? _) '((y red) (x apple)))
NIL
CL-USER> 
|#

(defun extract-key (binding)
  (first binding))

(defun extract-value (binding)
  (second binding))

;;; The match procedure

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

#|
(defun match-variable (p d bindings)
  (let ((binding (find-binding p bindings)))
    (if binding
	(match (extract-value binding) d bindings)
	(add-binding p d bindings))))
|#

(defun match-pieces (p d bindings)
  (let ((result (match (first p) (first d) bindings)))
    (if (eq 'fail result)
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

#|
CL-USER> (match '(color apple red) '(color apple red))

NIL
CL-USER> (match '(color (? x) red) '(color apple red))
((X APPLE))
CL-USER> (match '(color (? x) (? x)) '(color apple red))
FAIL
CL-USER> (match '(color (? _) (? _)) '(color apple red))
NIL
CL-USER> (match '(color (? x) (? x)) '(color orange orange))
((X ORANGE))
CL-USER> 
|#

;;; Adding in the 'restrictions' feature to match

(defun predicates-satisfied-p (predicate-list datum)
  "Return t if all predicates are true when applied to datum"
  (dolist (predicate predicate-list t)
    (unless (funcall predicate datum)
      (return nil))))

;;; New match-variable procedure
(defun extract-predicates (pattern-variable-expression)
  (cddr pattern-variable-expression))

(defun match-variable (p d bindings)
  (let ((binding (find-binding p bindings))
	(predicates (extract-predicates p)))
    (cond (binding
	   (match (extract-value binding) d bindings))
	  ((predicates-satisfied-p predicates d)
	   (add-binding p d bindings))
	  (t 'fail))))

#|
CL-USER> (defun 4letter-color-p (x) (member x '(pink blue)))
4LETTER-COLOR-P
CL-USER> (defun patriotic-color-p (x) (member x '(red white blue)))
PATRIOTIC-COLOR-P
CL-USER> (match '(color (? x) (? y patriotic-color-p))
	        '(color apple red)) 
((Y RED) (X APPLE))
CL-USER> (match '(color (? x) (? y patriotic-color-p 4letter-color-p))
	        '(color apple red)) 
FAIL
CL-USER> 
|#

;;; Unification Matching

(defun unify (p1 p2 &optional bindings)
  (cond ((elements-p p1 p2)
	 (unify-atoms p1 p2 bindings))
	((variable-p p1)
	 (unify-variable p1 p2 bindings))
	((variable-p p2)
	 (unify-variable p2 p1 bindings))
	((recursive-p p1 p2)
	 (unify-pieces p1 p2 bindings))
	(t 'fail)))

(defun unify-atoms (p1 p2 bindings)
  (if (eql p1 p2) bindings 'fail))

(defun unify-pieces (p1 p2 bindings)
  (let ((result (unify (first p1) (first p2) bindings)))
    (if (eq 'fail result)
	'fail
	(unify (rest p1) (rest p2) result))))

(defun unify-variable (p1 p2 bindings)
  (let ((binding (find-binding p1 bindings)))
    (if binding
	(unify (extract-value binding) p2 bindings)
	(if (inside-p p1 p2 bindings)
	    'fail
	    (add-binding p1 p2 bindings)))))

(defun inside-p (variable expression bindings)
  (if (equal variable expression)
      nil
      (inside-or-equal-p variable expression bindings)))

(defun inside-or-equal-p (variable expression bindings)
  (cond ((equal variable expression) t)
	((atom expression) nil)
	((eq '? (first expression))
	 (let ((binding (find-binding expression bindings)))
	   (when binding
	     (inside-or-equal-p variable 
				(extract-value binding)
				bindings))))
	(t
	 (or (inside-or-equal-p variable
				(first expression)
				bindings)
	     (inside-or-equal-p variable
				(rest expression)
				bindings)))))
#|
CL-USER> (unify '((? x) with (hair blond))
		'((patrick is-a (? y)) with (hair blond)))
((X (PATRICK IS-A (? Y))))
CL-USER> (unify '((? x) with (hair blond))
		'((patrick is-a (? x)) with (hair blond)))
FAIL
CL-USER> 
|#
