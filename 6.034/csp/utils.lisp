;;;; Utility functions for CSP

;;; Find the position of an item in a list of lists. We first apply the function
;;; provided to each member of the list of lists to obtain an element that we
;;; can test with EQL.

(defun index-of-element (item list fn)
  "Apply fn to each element of the list to get an element that we can compare
  with item using EQL.Return the index that matches"
  (position-if #'(lambda (element) 
		   (eql (funcall fn element) item))
	       list))
#|
CL-USER> (index-of-element 1 '((3 4 5) (2 3 4) (1 2 3) (4 5 6)) #'first)
2
CL-USER> 
|#


;;; Printing an arc - We specialize print-object
(defmethod print-object ((a arc) stream)
  (format stream "\"Arc~a ~a===:~a:===>~a\""
	  (arc-index a) (arc-tail a) 
	  (arc-constraint-function a) (arc-head a)))
#|
CL-USER> (setf a1 (make-instance 'arc :arc-tail 0 :arc-head 1 :arc-constraint-function #'not-same-p :arc-index 0))
"Arc0 0===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>1"
CL-USER> 
|#


;;; Construct an index list
(defun make-index-list (len)
  "Given a number, make a list that starts from 0 and goes up to the number" 
  (let ((result '()))
    (dotimes (i len (nreverse result))
      (push i result))))
#|
CL-USER> (make-index-list 10)
(0 1 2 3 4 5 6 7 8 9)
CL-USER> 
|#

      

