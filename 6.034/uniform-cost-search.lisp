;;;; Uniform cost search
;;;; This is the first of our optimal searches.

;;;; NOTE: We do not use a visited list, and check for loops 
;;;; in each node (partial path) for now.

;;; The search graph
;;; The 'length' to each neighbour is defined as well to enable
;;; path-length/path-cost computations.
(setf (get 's 'neighbours) '((2 a) (5 b))
      (get 'a 'neighbours) '((2 c) (4 d))
      (get 'b 'neighbours) '((1 d) (5 g))
      (get 'c 'neighbours) '()
      (get 'd 'neighbours) '((3 c) (2 g))
      (get 'g 'neighbours) '())

;;; ADTs for search queues and search nodes
(defun search-node (queue)
  (first queue))

;;; Expanding a search-node to its neighbours
;;; Need to use abstractions here instead of first/second etc!
(defun expand (search-node)
  "Expands a search node to its neighbours, and computes the new
   partial-path cost"
  (let ((partial-path-cost (first search-node))
	(partial-path (rest search-node)))
    (mapcar #'(lambda (neighbour) 
		(cons (+ partial-path-cost (first neighbour))
		      (cons (second neighbour) partial-path)))
	    (remove-if #'(lambda (state) (member (second state) partial-path))
		       (get (second search-node) 'neighbours)))))
#|
CL-USER> (expand '(5 b s))
((6 D B S) (10 G B S))
CL-USER> 
|#

;;; Uniform cost search
(defun uc (start finish &optional (queue (list (list 0 start))))
  (format t "~%Search Queue:~a" queue)
  (cond ((endp queue) nil)
	((eq (second (search-node queue)) finish)
	 (cons (first (search-node queue)) 
	       (reverse (rest (search-node queue)))))
	(t
	 (uc start
	     finish
	     (merge 'list
		    (sort (expand (search-node queue)) 
			  #'(lambda (node-1 node-2)
			      (< (first node-1) (first node-2))))
		    (rest queue)
		    #'(lambda (n1 n2)
			(< (first n1) (first n2))))))))
#|
CL-USER> (uc 's 'g)

Search Queue:((0 S))
Search Queue:((2 A S) (5 B S))
Search Queue:((4 C A S) (5 B S) (6 D A S))
Search Queue:((5 B S) (6 D A S))
Search Queue:((6 D B S) (6 D A S) (10 G B S))
Search Queue:((6 D A S) (8 G D B S) (9 C D B S) (10 G B S))
Search Queue:((8 G D A S) (8 G D B S) (9 C D A S) (9 C D B S) (10 G B S))
(8 S A D G)
CL-USER> 
|#

