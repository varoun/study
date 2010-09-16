;;;; Depth first search -- Chapter 19 Problems

;;; The search net
(setf (get 's 'neighbors) '(a d)
      (get 'a 'neighbors) '(s b d)
      (get 'b 'neighbors) '(a c e)
      (get 'c 'neighbors) '(b)
      (get 'd 'neighbors) '(s a e)
      (get 'e 'neighbors) '(b d f)
      (get 'f 'neighbors) '(e))

;;; Problem 19-1
;;; Extending a path
(defun extend-path (path)
  "Extends the path by consing on neighbors that dont result in loops"
  (mapcar #'(lambda (new-node) (cons new-node path))
	  (remove-if #'(lambda (neighbor) (member neighbor path))
		     (get (first path) 'neighbors))))

;;; Depth-first search
(defun depth-first (start finish)
  (do ((queue (list (list start)) 
	      (append (extend-path (first queue)) (rest queue))))
      ((endp queue) nil)
    (print queue)
    (when (eq finish (first (first queue)))
      (return (reverse (first queue))))))
#|
CL-USER> (depth-first 's 'f)

((S)) 
((A S) (D S)) 
((B A S) (D A S) (D S)) 
((C B A S) (E B A S) (D A S) (D S)) 
((E B A S) (D A S) (D S)) 
((D E B A S) (F E B A S) (D A S) (D S)) 
((F E B A S) (D A S) (D S)) 
(S A B E F)
CL-USER> 
|#


;;; Problem 19-2

