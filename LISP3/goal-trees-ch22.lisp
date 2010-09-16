;;;; Chapter 22: Goal Trees for the Blocks World of Chapter 21

;;; Before loading this file, do the following. Ignore compiler warnings!
#|
CL-USER> (load "/home/varoun/dev/study/LISP3/blocks-world-ch21.lisp")
#P"/home/varoun/dev/study/LISP3/blocks-world-ch21.lisp"
CL-USER> (assign-instance-to-block-name *blocks*)
DONE
CL-USER> (place-blocks-on-table *blocks* table)
DONE
CL-USER> 
|#

;;; The Node class is used to track details about method calls
(defclass node ()
  ((parent :accessor node-parent :initform nil)
   (children :accessor node-children :initform nil)
   (action :accessor node-action :initform nil)))

;;; The *current-node* special variable is bound to the top level node
(defvar *current-node* (make-instance 'node))

;;; The define-history-method macro generates :around methods for
;;; block world manipulation methods
(defmacro define-history-method (name parameters &body body)
  `(defmethod ,name :around ,parameters
     (let* ((parent *current-node*)
	    (*current-node* (make-instance 'node)) ;*current-node* is special
	    (primary-method-value (call-next-method)))
       (when primary-method-value
	 (attach-parent *current-node* parent)
	 (attach-action *current-node*
			(list ',name
			      ,@(remove-specializers parameters))))
       primary-method-value)))
#| 
CL-USER> (define-history-method put-on ((object movable-block) 
					(support basic-block)))
#<STANDARD-METHOD PUT-ON :AROUND (MOVABLE-BLOCK BASIC-BLOCK)>
CL-USER> 
|#

;;; define-history-method needs this procedure that removes parameter
;;; specializers of the method formal parameters.
#| OLD-VERSION, does not handle &rest, &optional etc correctly
(defun remove-specializers (parameter-list)
  (mapcar #'(lambda (element)
	      (if (listp element)
		  (first element)
		  element))
	  parameter-list))
|#
(defun remove-specializers (parameter-list)
  (let ((result nil))
    (dolist (element parameter-list (reverse result))
      (if (member element lambda-list-keywords)
	  (return (reverse result))
	  (push (if (listp element)
		    (first element)
		    element)
		result)))))
#|
CL-USER> (remove-specializers '((object movable-block)
				(support basic-block)
				&rest ignore))
			      
(OBJECT SUPPORT)
CL-USER> 
|#

;;; ATTACH-PARENT and ATTACH-ACTION
(defmethod attach-parent ((child node) (parent node))
  (setf (node-parent child) parent)
  (setf (node-children parent)
	(append (node-children parent)
		(list child))))
(defmethod attach-action ((node node) action)
  (setf (node-action node) action))

;;; Setup history methods for block methods of interest
(define-history-method put-on ((object movable-block)
			       (support basic-block)))

(define-history-method get-rid-of ((object movable-block)))

(define-history-method make-space ((object movable-block)
				  (support basic-block)))

(define-history-method clear-top ((support load-bearing-block)))

(define-history-method move ((object movable-block)
			     (support basic-block)))

;;; Printing the goal-tree

;; We need to redefine the PRINT-OBJECT method for blocks
(defmethod print-object ((x basic-block) stream)
  (format stream "~a" (block-name x)))

(defun show-simple-tree (node &optional (indentation 0))
  (format t "~&~vt~a"
	  indentation
	  (or (node-action node) 'top-of-tree))
  (dolist (node (node-children node))
    (show-simple-tree node (+ indentation 2))))

#|
CL-USER> (progn (put-on w7 b1)
		(put-on b1 b2))
CL-USER> (show-simple-tree *current-node*)
 TOP-OF-TREE
  (PUT-ON W7 B1)
    (MOVE W7 B1)
  (PUT-ON B1 B2)
    (MOVE B1 B2)
NIL
CL-USER> 
|#

;;; FIND-ACTION searches through the tree of nodes for one whose action
;;; slot contains the given form
(defun find-action (given-form &optional (node *current-node*))
  (let ((node-form (node-action node)))
    (if (equal given-form node-form)
	node
	(dolist (child (node-children node))
	  (let ((result (find-action given-form child)))
	    (when result (return result)))))))

;;; The TELL-WHY macro and the TELL-WHY-AUX procedure are used to
;;; answer the 'Why did you?' questions by finding the direct parent
;;; node of a given action

(defmacro tell-why (name &rest parameters)
  `(tell-why-aux (list ',name ,@parameters)))

(defun tell-why-aux (given-action)
  (let ((node (find-action given-action)))
    (if (not (null node))
	(cond ((node-action (node-parent node))
	       (format t "~&I did ~a because I wanted to ~a."
		       given-action
		       (node-action (node-parent node))))
	      (t (format t "~&I did ~a because you told me to."
			 given-action)))
	(format t "~&I did not ~a." given-action))
    'done))

;;; The TELL-HOW macro describes how a particular action was carried out
;;; by looking at the action slots of all direct children of the respective
;;; node

(defmacro tell-how (name &rest parameters)
  `(tell-how-aux (list ',name ,@parameters)))

(defun tell-how-aux (given-action)
  (let ((node (find-action given-action)))
    (if (not (null node))
	(let ((children (node-children node)))
	  (cond ((null children)
		 (format t "~&I did ~a by just doing it."
			 given-action))
		(t
		 (format t "~&I did ~a by the following operations:" 
			 given-action)
		 (dolist (child children)
		   (format t "~&~4t~a" (node-action child))))))
	(format t "~& I did not ~a" given-action))
    'done))
