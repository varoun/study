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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; General Constraint Propagation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
