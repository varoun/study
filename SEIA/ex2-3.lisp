;;;; Chapter 2 -- Exercise 3 -- Forms

(defun lens ()
  (cl-who:with-html-output-to-string
      (str nil :prologue t :indent t)
    (:html 
     (:head (:title "Focal length calculator"))
     (:body (:h1 "Focal length calculator")
	    (:h2 "Exactly how long a lens do you need ?")
	    (:form :action "/basics/lens-results" :method "post"
		   (:div "Subject distance" (:input :type "text" :name "distance"))
		   (:div "Subject height" (:input :type "text" :name "height"))
		   (:input :type "submit" :value "calculate"))))))


;;; Calculating the focal length needed.

(defun convert-to-inch (feet)
  (* feet 12))

(defun magnification (subject-size-in-inch)
  (/ 1.5 subject-size-in-inch)) ; we assume 35mm negative, 1.5 is the long dimension

(defun focal-length (distance-inches magnification-inches)
  (let ((focal-length-inch (/ distance-inches
			      (+ 1 (/ 1 magnification-inches)))))
    (* focal-length-inch 25.4))) ; convert to mm

(defun calculate-focal-length (distance height)
  (let ((fl (focal-length (convert-to-inch distance)
			  (magnification (convert-to-inch height)))))
    (round fl)))

;;; The lens-results page
;;; No input validation at this point!!!
(defun lens-results ()
  (let ((distance (parse-integer (hunchentoot:parameter "distance")))
	(height (parse-integer (hunchentoot:parameter "height"))))
    (cl-who:with-html-output-to-string 
	(str nil :prologue t :indent t)
      (:html
       (:head (:title "Focal Length Results"))
       (:body (:h1 "Focal Length:")
	      (cl-who:htm (:p "The focal length was calculated to be " 
			      (cl-who:str 
			       (calculate-focal-length distance height)) "mm.")))))))

