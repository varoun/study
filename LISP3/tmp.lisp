
(defun count-list (l &optional (result 0))
    (if (endp l)
	result
	(count-list (rest l) (+ result 1))))
