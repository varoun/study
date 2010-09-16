;;;; Chapter 11

;;; Properties

;;; Problem 11-1
(defun father (person)
  (get person 'father))
(defun grandfather (person)
  (get (father person) 'father))

;;; Arrays 
(defun count-bins-with-specified-part (bin part)
  (let ((result 0))
    (dotimes (n (array-dimension bin 0) result)
      (when (eq part (aref bin n))
	(setf result (+ result 1))))))