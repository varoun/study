;;;; The N Queens puzzle 

;;; Problem 19-12
;;; Printing a board when provided a configuration
(defun print-board (config)
  (labels 
      ((draw-border-top-bot (n)
	 (format t "*")
	 (dotimes (i n)
	   (format t "-"))
	 (format t "*"))
       (draw-border-left-right ()
	 (format t "|"))
       (display-line (queen-position size)
	 (draw-border-left-right)
	 (dotimes (i size)
	   (if (= i (second queen-position))
	       (format t "Q")
	       (format t ".")))
	 (draw-border-left-right)
	 (format t "~%")))
    (let ((s (length config)))
      (draw-border-top-bot s)
      (format t "~%")
      (dolist (pos config)
	(display-line pos s))
      (draw-border-top-bot s)
      (format t "~%"))))

;;; Problem 19-13

(defun threat (i j a b) ; queen 1 at (a,b), queen 2 at (i,j)
  "Predicate to test if two queens threaten each other"
  (or (= i a) ; same row
      (= j b) ; same column
      (= (- i j) (- a b)) ; SW-NE diag
      (= (+ i j) (+ a b)))) ; NW-SE diag

(defun conflict (m n board)
  "Predicate to test if queen at (m,n) is threatened when placed on board"
  (let ((r (first (first board)))
	(c (second (first board))))
    (cond ((endp board) nil)
	  ((threat m n r c) t)
	  (t
	   (conflict m n (rest board))))))

(defun queens (size
	       &optional (board nil) (r 0) (c 0))
  "Solve the n-queens puzzle"
  (unless (= c size)
    (unless (conflict r c board)
      (if (= (+ 1 r) size)
	  (print-board (reverse (cons (list r c) board)))
	  (queens size (cons (list r c) board) (+ r 1) 0)))
    (queens size board r (+ 1 c))))
#|
CL-USER> (queens 3)
NIL
CL-USER> (queens 4)
*----*
|.Q..|
|...Q|
|Q...|
|..Q.|
*----*
*----*
|..Q.|
|Q...|
|...Q|
|.Q..|
*----*
NIL
CL-USER> 
|#
