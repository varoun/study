;;;; Chapter 12 -- Macros

(defun when-plusp-proc (number result)
  (when (plusp number) result))
#|
CL-USER> (when-plusp-proc -3 (print "ALARM"))

"ALARM" ; the arg is evaluated, resulting in this side effect 
NIL
CL-USER> 
|#		       

(defmacro when-plusp (number result)
  `(when (plusp ,number) ,result))
#|
CL-USER> (when-plusp -3 (print "ALARM"))
NIL
CL-USER> (when-plusp 3 (print "ALARM"))

"ALARM" 
"ALARM"
CL-USER> 
|#

(defmacro when-plusp-with-body (number &body body)
  `(when (plusp ,number) ,@body))
#|
CL-USER> (when-plusp-with-body 3 (print "one") (print "two"))

"one" 
"two" 
"two"
CL-USER> (when-plusp-with-body -3 (print "one") (print "two"))
NIL
CL-USER> 
|#


;;; Problem 12-1

(defmacro put (symb prop val)
  `(setf (get ,symb ,prop) ,val))
#|
CL-USER> (put 'varoun 'likes '(math cs))
(MATH CS)
CL-USER> (get 'varoun 'likes)
(MATH CS)
CL-USER> 
|#

