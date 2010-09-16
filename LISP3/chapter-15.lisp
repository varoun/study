;;;; Chapter 15 - Lexical Closures

(defun make-power-of-two-generator ()
  (let ((val 1))
    (labels 
	((reset () (setf val 1))
	 (next-val () (setf val (* val 2)))
	 (dispatch (msg)
	   (cond ((eq msg 'reset) #'reset)
		 ((eq msg 'next) #'next-val))))
      #'dispatch)))

#|
CL-USER> (setf g1 (make-power-of-two-generator))

#<COMPILED-LEXICAL-CLOSURE (:INTERNAL DISPATCH MAKE-POWER-OF-TWO-GENERATOR) #x14E3222E>
CL-USER> (funcall (funcall g1 'next))
2
CL-USER> (funcall (funcall g1 'next))
4
CL-USER> (funcall (funcall g1 'next))
8
CL-USER> (funcall (funcall g1 'reset))
1
CL-USER> (funcall (funcall g1 'next))
2
CL-USER> 
|#
