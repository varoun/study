;;;; AnyPath Searches - DepthFirst/BreadthFirst/BestFirst

;;; Our search graph
;;; States are represented as symbols, links (to neighbours), and 
;;; coordinates of states as properties.

(setf (get 's 'neighbour) '(a d)
      (get 'a 'neighbour) '(s b d)
      (get 'b 'neighbour) '(a c e)
      (get 'c 'neighbour) '(b)
      (get 'd 'neighbour) '(s a e)
      (get 'e 'neighbour) '(b d f)
      (get 'f 'neighbour) '(e))

(setf (get 's 'coordinates) '(0 3)
      (get 'a 'coordinates) '(4 6)
      (get 'b 'coordinates) '(7 6)
      (get 'c 'coordinates) '(11 6)
      (get 'd 'coordinates) '(3 0)
      (get 'e 'coordinates) '(6 0)
      (get 'f 'coordinates) '(11 3))

;;; ADT for search nodes
(defun node-state (search-node) 
  "The state of a search node is the first element of the list"
  (first search-node))

;;; The visited list
(defun make-visited-list (starting-state) 
  "The visited list is actually a hash table!"
  (let ((visited-list (make-hash-table)))
    (setf (gethash starting-state visited-list) t)
    visited-list))

(defun visitedp (visited-list state)
  (gethash state visited-list))

(defun add-to-visited-list (visited-list state)
  (setf (gethash state visited-list) t))

;;; The search queue
(defun make-search-queue (starting-state)
  "A search queue consists of a list of search nodes"
  (list (list starting-state)))

(defun get-search-node (search-queue)
  "We always pick the first node from the queue in anypath searches"
  (first search-queue))


;;; Expanding a state
(defun expand (search-node visited-list)
  (mapcar #'(lambda (neighbour) (cons neighbour search-node))
	  (remove-if #'(lambda (state) (visitedp visited-list state))
		     (get (node-state search-node) 'neighbour))))



