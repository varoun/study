;;;; Blocks World -- Chapter 21

;;; Class definitions
(defclass basic-block () 
  ((name :accessor block-name :initarg :name)
   (width :accessor block-width :initarg :width)
   (height :accessor block-height :initarg :height)
   (position :accessor block-position :initarg :position)
   (supported-by :accessor block-supported-by :initform nil)))

(defclass movable-block (basic-block) ())

(defclass load-bearing-block (basic-block)
  ((support-for :accessor block-support-for :initform nil)))

(defclass brick (movable-block load-bearing-block) ())

(defclass wedge (movable-block) ())

(defclass ball (movable-block) ())

(defclass table (load-bearing-block) ())

(defclass hand ()
  ((name :accessor hand-name :initarg :name)
   (position :accessor hand-position :initarg :position)
   (grasping :accessor hand-grasping :initform nil)))

;;; The initial position of blocks in the blocks world - Fig 21-3

(defvar *blocks*
  (list
   (make-instance 'table :name 'table :width 20 :height 0 :position '(0 0))
   (make-instance 'brick :name 'b1 :width 2 :height 2 :position '(0 0))
   (make-instance 'brick :name 'b2 :width 2 :height 2 :position '(2 0))
   (make-instance 'brick :name 'b3 :width 4 :height 4 :position '(4 0))
   (make-instance 'brick :name 'b4 :width 2 :height 2 :position '(8 0))
   (make-instance 'wedge :name 'w5 :width 2 :height 4 :position '(10 0))
   (make-instance 'brick :name 'b6 :width 4 :height 2 :position '(12 0))
   (make-instance 'wedge :name 'w7 :width 2 :height 2 :position '(16 0))
   (make-instance 'ball :name 'l8 :width 2 :height 2 :position '(18 0))))


;;; For increased convenience, assign each instance to the
;;; symbol found in its block-name
(defun assign-instance-to-block-name (block)
  (dolist (l block)
    (set (block-name l) l)) ; set is diff from setf !!
  'done)
#|
CL-USER> (assign-instance-to-block-name *blocks*)
NIL
CL-USER> b1
#<BRICK #x3020008FBC0D>
CL-USER> 
|#


;;; Initially, all blocks are placed on the table, the 
;;; supported-by and support-for slots need filling in
(defun place-blocks-on-table (blocks-world table-name)
  (dolist (l (remove table-name blocks-world)) ; table-name symbol is bound to object!
    (setf (block-supported-by l) table-name)
    (push l (block-support-for table-name)))
  'done)
#|
CL-USER> (place-blocks-on-table *blocks* table)
NIL
CL-USER> (block-supported-by b1)
#<TABLE #x30200085F8BD>
CL-USER> 
|#

;;; The robot hand
(defvar *hand* (make-instance 'hand :name '*hand* :position '(0 6)))

;;; Problem 21-1 -- Easier instantiation of blocks
(defun make-block (type name width height xcoord ycoord)
  (make-instance type 
		 :name name
		 :width width
		 :height height
		 :position (list xcoord ycoord)))

;;; Slot readers are generic functions - the following will ensure that
;;; block-support-for returns nil when called with a non load-bearing block
(defmethod block-support-for ((object basic-block))
  nil)

;;; The put-on method and other short goal oriented methods

(defmethod put-on ((object movable-block) (support basic-block))
  (if (get-space object support)
      (and (grasp object)
	   (move object support)
	   (ungrasp object))
      (format t "~&Sorry, there is no room for ~a on ~a"
	      (block-name object)
	      (block-name support))))

(defmethod get-space ((object movable-block) (support basic-block))
  (or (find-space object support)
      (make-space object support)))

(defmethod grasp ((object movable-block))
  (unless (eq (hand-grasping *hand*) object)
    (when (block-support-for object) (clear-top object))
    (when (hand-grasping *hand*) 
      (get-rid-of (hand-grasping *hand*)))
    (format t "~&Move hand to pick up ~a at location ~a"
	    (block-name object)
	    (top-location object))
    (setf (hand-position *hand*) (top-location object))
    (setf (hand-grasping *hand*) object))
  t)

(defmethod ungrasp ((object movable-block))
  (when (block-supported-by object)
    (format t "~&Ungrasp ~a" (block-name object))
    (setf (hand-grasping *hand*) nil)
    t))

(defmethod get-rid-of ((object movable-block))
  (put-on object table))

(defmethod make-space ((object movable-block) (support basic-block))
  (dolist (obstruction (block-support-for support))
    (get-rid-of obstruction)
    (let ((space (find-space object support)))
      (when space (return space)))))

(defmethod clear-top ((support load-bearing-block))
  (dolist (obstacle (block-support-for support))
    (get-rid-of obstacle)))

#|
;; This early version of move/remove-support/add-support have been
;; replaced by a single 'move' generic function that uses :before and
;; :after methods 
(defmethod move ((object movable-block) (support basic-block))
  (remove-support object)
  (let ((newplace (get-space object support)))
    (format t "~&Move ~a to the top of ~a at location ~a"
	    (block-name object)
	    (block-name support)
	    newplace)
    (setf (block-position object) newplace)
    (setf (hand-position *hand*) (top-location object)))
  (add-support object support)
t)

(defmethod remove-support ((object movable-block))
  (let ((support (block-supported-by object)))
    (when support
      (setf (block-support-for support)
	    (remove object (block-support-for support)))
      (setf (block-supported-by object) nil)
      t)))

(defmethod add-support ((object movable-block) (support basic-block))
  t) ; if the support is not a load-bearing block, dont do anything

(defmethod add-support ((object movable-block) (support load-bearing-block))
  (push object (block-support-for support))
  (setf (block-supported-by object) support)
  t)
|#

;;; The 'move' generic function
(defmethod move ((object movable-block) (support basic-block))
  (let ((newplace (get-space object support)))
    (format t "~&Move ~a to ~a at location ~a"
	    (block-name object)
	    (block-name support)
	    newplace)
    (setf (block-position object) newplace)
   ; (setf (hand-position *hand*) (top-location object)) moved to block-position slot writer generic function
  t))

;; Adding an after method to the slot writer for block-position, investigate!
(defmethod (setf block-position) ; the object can only be moved by the hand!
    :after
    (new-position (object movable-block))
  (setf (hand-position *hand*) (top-location object)))

(defmethod move :before ((object movable-block) ignored-parameter)
  (let ((support (block-supported-by object)))
    (when support 
      (format t "~&Removing support relations between ~a and ~a"
	      (block-name object) (block-name support))
      (setf (block-support-for support)
	    (remove object (block-support-for support)))
      (setf (block-supported-by object) nil)
      t)))

(defmethod move :after ((object movable-block) (support load-bearing-block))
  (format t "~&Adding support relations between ~a and ~a"
	  (block-name object) (block-name support))
  (push object (block-support-for support))
  (setf (block-supported-by object) support)
  t)

;;; Specialising PRINT-OBJECT to print blocks
(defmethod print-object ((x basic-block) stream)
  (format stream "#<block ~a>" (block-name x)))

;;; Numerical procedures FIND-SPACE and TOP-LOCATION
;;; These are simpllified versions

(defun find-space (object support)
  (dotimes (offset (+ 1 (- (block-width support) 
			   (block-width object))))
    (unless (intersection-p object offset 
			    (first (block-position support))
			    (block-support-for support))
      (return (list (+ offset (first (block-position support)))
		    (+ (second (block-position support))
		       (block-height support)))))))

(defun intersection-p (object offset base obstacles)
  (dolist (obstacle obstacles)
    (let* ((ls-proposed (+ offset base))
	   (rs-proposed (+ ls-proposed (block-width object)))
	   (ls-obstacle (first (block-position obstacle)))
	   (rs-obstacle (+ ls-obstacle (block-width obstacle))))
      (unless (or (>= ls-proposed rs-obstacle)
		  (<= rs-proposed ls-obstacle))
	(return t)))))

(defun top-location (object)
  (list (+ (first (block-position object))
	   (/ (block-width object) 2))
	(+ (second (block-position object)) (block-height object))))
	   
