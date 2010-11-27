;;;; CSP 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Data Structures 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass csp ()
  ((number-of-variables                 
    :accessor csp-number-of-variables  
    :initarg :csp-number-of-variables
    :documentation "This slot holds the total number of variables(nodes) in the
  csp graph")
   (variables
    :accessor csp-variables
    :initarg :csp-variables
    :documentation "This is a list of variable indices.")
   (arcs
    :accessor csp-arcs
    :initarg :csp-arcs
    :documentation "This is a list of arcs. Refer the arc class for details ")
   (arcs-from
    :accessor csp-arcs-from
    :initarg :csp-arcs-from
    :documentation "A vector whose elements are lists of arcs. The ith entry is
  a list of members from csp-arcs whose tail index equals i, i.e, the list of
  directed arcs going out from the ith variable.")
   (arcs-into 
    :accessor csp-arcs-into
    :initarg :csp-arcs-into
    :documentation "A vector whose elements are lists of arcs. The jth entry is
  a list of members from csp-arcs whose head index equals j, i.e, the list of
  directed arcs going into the the jth variable")
   (arcs-added-for-new-var
    :accessor csp-arcs-added-for-new-var
    :initarg :csp-arcs-added-for-new-var
    :documentation "A vector whose elements are a list of arcs. The kth entry is
  a list of members from csp-arcs when one index (head or tail) equals k and the
  other index (head or tail resp) is less than or equal to k, i.e arcs
  connecting the kth variable to variables with a smaller index.")
   (variable-domains 
    :accessor csp-variable-domains
    :initarg :csp-variable-domains
    :documentation "A vector of lists, the ith element is the list of current
  values, ie, the domain of the ith variable")
   (variable-names
    :accessor csp-variable-names
    :initarg :csp-variable-names
    :documentation "A vector whose ith element is the symbol/name for the ith
  variable"))
  (:documentation "The csp class holds the csp graph"))

(defclass arc ()
  ((head 
    :initarg :arc-head
    :accessor arc-head
    :documentation "This slot holds the index of the variable at the head of the
  arc")
   (tail 
    :initarg :arc-tail
    :accessor arc-tail
    :documentation "This slot holds the index of the variable at the tail of the
  arc")
   (constraint-function
    :initarg :arc-constraint-function
    :accessor arc-constraint-function
    :documentation "This slot holds the constraint function on the arc. The
  constraint function takes an index for the variable i, a value for the ith
  variable, an index for the variable j, and a value for the jth variable
  and returns true if the constraint is satisfied, false otherwise.")
   (index 
    :initarg :arc-index
    :accessor arc-index
    :documentation "The index for this arc."))
  (:documentation "An arc is defined by the variables it connects and a
  constraint function. Since this is a directed arc, we identify the variables
  as a head and tail variable."))


;;; Global variable that holds the currrent constraint graph
(defvar *csp*)

;;; Keep track of the number of operations, aka arc tests done. 
(defparameter *number-of-arc-tests* 0)

;;; Control verbosity and monitoring
(defparameter *monitor-arc-tests* nil)
(defparameter *verbose* nil)

;;; Accessing domains of variables
(defun domain-of-variable (variable)
  "Take an index for a variable, and return its domain by looking it up in *csp*
that stores the current csp constraint graph."
  (elt (csp-variable-domains *csp*) variable))

(defun set-variable-domain (variable value)
  "Take a variable index and a list of values that make up its domain and update
the csp-variable-domain vector in the current csp constraint graph, *csp*."
  (setf (elt (csp-variable-domains *csp*) variable) value))

;;; Getting the name of a variable
(defun name-of-variable (variable)
  "Retrieve the name of the variable given its index, look it up from the
variable-names slot in the current constraint graph, *csp*"
  (elt (csp-variable-names *csp*) variable))

;;; Accessing arcs given the head/tail variable indices.
(defun arcs-from-variable (variable)
  "Get a list of arcs that have the given variable at its tail."
  (elt (csp-arcs-from *csp*) variable))

(defun arcs-into-variable (variable)
  "Get the list of arcs that have the give variable at its head"
  (elt (csp-arcs-into *csp*) variable))

(defun set-arcs-from-variable (variable arcs)
  "Set the list of arcs provided as the arcs that have the given variable as the
tail"
  (setf (elt (csp-arcs-from *csp*) variable) arcs))

(defun set-arcs-into-variable (variable arcs)
  "Set the list of arcs provided as the arcs that have the given variable as the
head"
  (setf (elt (csp-arcs-into *csp*) variable) arcs))

(defun new-arcs-for-variable (variable)
  "Get the list of arcs that connect the given variable with variables of a
smaller index"
  (elt (csp-arcs-added-for-new-var *csp*) variable))

(defun set-new-arcs-for-variable (variable arcs)
  "Set the list of arcs that connect the given variable to those variables with
a smaller index to the provided arc list"
  (setf (elt (csp-arcs-added-for-new-var *csp*) variable) arcs))

;;; Save the current variable domains so that we can restore them when
;;; backtracking
(defun save-domains ()
  "Returns a copy of the variable domains"
  (copy-seq (csp-variable-domains *csp*)))

;;; Restore variable domains to the one provided. 
(defun restore-domains (saved)
  "Restore the variable domains to the one referrenced by SAVED"
  (setf (csp-variable-domains *csp*) saved))


;;; Initializing the constraint graph
(defun initialize (names-and-domains arcs)
  "names-and-domains are a list of lists. Each list's first element is the
variable name and the rest the variable's domain. Arcs is the list of arcs. Given
these the initialize procedure builds the constraint graph"
  (format t "~%Initializing...")
  (let ((l (length names-and-domains)))
    (setf *csp* 
	  (make-instance 
	   'csp
	   :csp-arcs arcs
	   :csp-number-of-variables l
	   :csp-variables (make-index-list l)
	   :csp-variable-names (coerce (mapcar #'first names-and-domains)
				       'vector)
	   :csp-variable-domains (coerce (mapcar #'rest names-and-domains)
					 'vector)
	   :csp-arcs-from (make-array l :initial-element '())
	   :csp-arcs-into (make-array l :initial-element '())
	   :csp-arcs-added-for-new-var (make-array l :initial-element '())))
    (dolist (arc arcs) ; arcs indexed by tail var
      (set-arcs-from-variable 
       (arc-tail arc)
       (cons arc (arcs-from-variable (arc-tail arc)))))
    (dolist (arc arcs) ; arcs index by head var
      (set-arcs-into-variable
       (arc-head arc)
       (cons arc (arcs-into-variable (arc-head arc)))))
    (dolist (i (csp-variables *csp*)) ; arcs whose head/tail var is less than index 
      (dolist (arc arcs)
	(if (and (<= (arc-tail arc) i)
		 (<= (arc-head arc) i)
		 (or (= (arc-tail arc) i)
		     (= (arc-head arc) i)))
	    (set-new-arcs-for-variable i (cons arc
					       (new-arcs-for-variable i))))))
    (format t "Done~%")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Backtracking Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Check to see if the partial assignment (this is like the partial path) to
;;; the first n variables are consistent. The list of values assigned are in
;;; reverse order.

(defun consistent-p (last-var-index reversed-values)
  "Check to see if the partial assignment as given by reversed-values is valid
for the first last-var-index variables"
  (every 
   #'(lambda (arc)
       ;We pick the values at the tail and head of the current arc by looking
       ;them up from the list of reversed values. Since they are reversed, we
       ;need to index them from the end of the list!
       (let ((tail-val 
	      (elt reversed-values (- last-var-index (arc-tail arc))))
	     (head-val 
	      (elt reversed-values (- last-var-index (arc-head arc)))))
	 ;(format t "~%Checking consistency of arc: ~a" arc)
	 (funcall (arc-constraint-function arc) 
		  (arc-tail arc)
		  tail-val
		  (arc-head arc)
		  head-val)))
   ;This holds the list of arcs whose head/tail indices are less than
   ;last-var-index and hence, have values assigned to them in reversed-values. 
   (new-arcs-for-variable last-var-index)))


;;; Basic implementation of backtrack
;;; This is recursive in nature. Rewrite later ?
(defun backtrack ()
  (labels ((backtrack-aux (var variable-domain partial-assignment)
	     (if (endp variable-domain)
		 nil ; no values left, search has failed
		 (let* ((extended-partial-assignment
			 (cons (first variable-domain) partial-assignment)))
		   
		   (princ var)
		   (if (consistent-p var extended-partial-assignment)
					;assignment is consistent
		       (if (= var (- (csp-number-of-variables *csp* ) 1))
					;We have assigned all variables
			   (reverse extended-partial-assignment)
					; extend partial assignment, fail otherwise
			   (or (backtrack-aux (+ 1 var)
						(domain-of-variable (+ 1 var))
						extended-partial-assignment)
			       (backtrack-aux var 
					      (rest variable-domain) 
					      partial-assignment)))
		       (backtrack-aux var 
				      (rest variable-domain)
				      partial-assignment))))))
    (backtrack-aux 0 (domain-of-variable 0) '())))
#|
CL-USER> (initialize-map-coloring)
Initializing...Done
NIL
CL-USER> (backtrack)
011233444
(RED BLUE RED BLUE GREEN)
CL-USER> 
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; ARC Consistency
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Update the domain of the variable that is the arc tail so that only values
;;; consistent with SOME value at the head variable remain
(defmethod revise ((a arc))
  "Given an arc, update the tail domain so that only values consistent with the
head domain remain"
  (let ((tail-var (arc-tail a))
	(head-var (arc-head a))
	(constraint (arc-constraint-function a)))
    (set-variable-domain 
     tail-var
     (map-non-false 
      #'(lambda (tval)
	  (if (notany #'(lambda (hval)
			  (incf *number-of-arc-tests*)
			  (funcall constraint tail-var tval head-var hval))
		      (domain-of-variable head-var))
	      nil ; Nuke tval from tail domain if constraint fails.
	      tval)) ; Keep tval from tail domain if constraint succeeds.
      (domain-of-variable tail-var)))))


;;; Simple version if propatage-constraints. This only propagates to neighbors
;;; of the input variable.
(defun forward-check (var)
  "Propagate constraints to variables directly connected to the input
variable. Return nil if any domain becomes empty, t otherwise."
  (do ((arcs (arcs-into-variable var) (rest arcs)))
      ((endp arcs) t) ; if we've looked at all of them, return true
    (revise (first arcs)) ; revise the tail domain of the arc
    (when (null (domain-of-variable (arc-tail (first arcs)))) ; if domain becomes empty 
      (return nil)))) ; fail by returning nil

;;; A simpler version of REVISE that only counts the number of revisions to the
;;; domain of the tail variable of the given arc if the head variable value is
;;; set to the given value.
(defun count-revisions (arc value)
  "Count the number of revisions to domain of tail var of arc if the head var is
set to the given value."
  (let ((head-var (arc-head arc))
	(tail-var (arc-tail arc))
	(constraint (arc-constraint-function arc))
	(deleted-count 0))
    (dolist (x (domain-of-variable tail-var) deleted-count)
      (unless (funcall constraint tail-var x head-var value)
	(incf deleted-count)))))

;;; Count the number of domain deletions a call to FORWARD-CHECK will entail if
;;; the given head variable is set to the given value if we did not stop at any 
;;; conflicts.
(defun count-forward-check-deletions (var val)
  "Count the number of revisions to the tail var of all arcs whose head var is
set to the given value"
  (do ((arcs (arcs-into-variable var) ; all arcs whose head is given  
	     (rest arcs)) 
       (revisions 0 
		  (+ revisions (count-revisions (first arcs) val))))
      ((endp arcs) revisions)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Backtracking with Forward Checking with dynamic ordering of variables and
;;;;; values 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun backtrack-fc-dynamic (pick-next-var sort-values)
  "Does backtracking with forward checking. Uses the pick-next-var function to
determine which variable to try next, and trhe sort-values functions to sort the
  values in the variable's domain"
  (labels ((backtrack-fc-dynamic-aux (var var-values done-vars)
	     ; FAIL is called when a tentative assignment fails
	     (labels 
		 ((fail (saved-domains)
		    (restore-domains saved-domains)
		    (backtrack-fc-dynamic-aux var
					      (rest var-values)
					      done-vars)))
	       (if (endp var-values) ; we have a var but no vals
		   nil                ; inconsistent
		   (let ((domains (save-domains)))
		     (set-variable-domain var (list (first var-values)))
		     ; Propagate constraints on arcs affected by our assignment
		     (if (forward-check var)
			 (let ((new-var (funcall pick-next-var var done-vars)))
			   (if new-var
			       (or
				(backtrack-fc-dynamic-aux 
				 new-var
				 (funcall sort-values new-var)
				 (cons new-var done-vars))
				(fail domains))
				; If var is false, it means we've looked at all
				; of them, and have unique vals. We are done. 
			       (map 'list #'first (csp-variable-domains *csp*))))
			 (fail domains)))))))
    ; start with the first variable
    (let ((var (funcall pick-next-var nil '())))
      (if var ; make sure we have a variable to start with
	  (backtrack-fc-dynamic-aux 
	   var 
	   (funcall sort-values var)
	   '())))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Functions to pick the next var and sort variable domains
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This simply picks the next variable by numerical order
(defun pick-next (current-var done-vars)
  "Return the next var in numeric (ascending) order. If the current-var is nil
we are just starting out, return the first variable 0. If not keep increment the
current var and return it, till we do all vars"
  (if current-var
      (if (= current-var
	     (- (csp-number-of-variables *csp*) 1))
	  nil
	  (+ current-var 1))
      0))
#|
CL-USER> (pick-next nil '())
0
CL-USER> (pick-next 4 '(4 3 2 1 0))
5
CL-USER> 
|#


;;; Simple backtrack with forward checking. No dynamic ordering of variables or
;;; values 
#|
CL-USER> (initialize-map-coloring *US-48-STATES-MAP*)
Initializing...Done
NIL
CL-USER> (backtrack-fc-dynamic #'pick-next #'domain-of-variable)
(RED RED RED BLUE BLUE RED RED BLUE GREEN RED RED BLUE RED RED GREEN GREEN BLUE BLUE RED GREEN GREEN YELLOW YELLOW GREEN GREEN RED RED YELLOW GREEN BLUE YELLOW GREEN BLUE GREEN GREEN YELLOW GREEN BLUE BLUE BLUE BLUE GREEN YELLOW RED BLUE YELLOW RED YELLOW)
CL-USER> 
