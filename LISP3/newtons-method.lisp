;;;; Newtons Method 

(defparameter dx .00001)

(defun deriv (f)
  "Return a new function that computes the derivative"
  #'(lambda (x) 
      (/ (- (funcall f (+ x dx)) (funcall f x)) dx)))

(defun cube (x) (* x x x))

#|
CL-USER> (funcall (deriv #'cube) 5)
75.531006
CL-USER> 
|#

;; to be contd