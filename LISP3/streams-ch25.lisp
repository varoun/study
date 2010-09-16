;;;; Lazy Streams -- Chapter 25

;;; Delay and force

(defmacro delay (expr)
  `#'(lambda () ,expr))

(defmacro force (thunk)
  `(funcall ,thunk))

;;; Constructors and selectors for lazy streams
;;; These are odd streams - See SRFI 41
(defmacro stream-cons (object stream)
  `(list ,object (delay ,stream)))

(defun stream-first (stream)
  (first stream))

(defun stream-rest (stream)
  (force (second stream)))

(defun stream-endp (stream)
  (eq stream 'empty-stream))

#|
The current implementation is not Memoised!
CL-USER> (setf s1 (stream-cons (expt 2 2) 
			       (stream-cons (expt 2 3) 'empty-stream))) 

(4 #<Anonymous Function #x3020008C2BCF>)
CL-USER> (stream-first s1)
4
CL-USER> (stream-rest s1)
(8 #<Anonymous Function #x3020008C4A8F>)
CL-USER> s1
(4 #<Anonymous Function #x3020008C2BCF>)
CL-USER> 
|#
