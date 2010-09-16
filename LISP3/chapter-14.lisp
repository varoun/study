;;;; Chapter 14 - CLOS

;;; Articles
(defclass article ()
  ((title :accessor article-title :initarg :title)))

(defclass computer-article (article) ())

(defclass business-article (article) ())

(defclass political-article (article) ())

;;; Friends
(defclass friend ()
  ((name :accessor friend-name :initarg :name)))

(defclass hacker-friend (friend) ())
(defclass entrepreneur-friend (friend) ())
(defclass philosopher-friend (friend) ())

;;; Notify friends of interesring articles
(defun print-notification (article friend)
  (format t "~%Tell ~a about \"~a\"" 
	  (friend-name friend)
	  (article-title article)))

(defun display-notifications (articles friends)
  (dolist (article articles)
    (dolist (friend friends)
      (print-notification article friend))))

#|
CL-USER> (setf articles 
	       (list (make-instance 'business-article 
				    :title "Memory Prices Down")
		     (make-instance 'computer-article
				    :title "Memory speeds up")
		     (make-instance 'political-article 
				    :title "Memory Impugned")))
(#<BUSINESS-ARTICLE #x14F886FE> #<COMPUTER-ARTICLE #x14F88576> #<POLITICAL-ARTICLE #x14F883EE>)
CL-USER> (setf friends 
	       (list (make-instance 'hacker-friend :name 'Dan)
		     (make-instance 'hacker-friend :name 'Gerry)
		     (make-instance 'entrepreneur-friend :name 'Philip)
		     (make-instance 'philosopher-friend :name 'David)))
(#<HACKER-FRIEND #x14EFCFA6> #<HACKER-FRIEND #x14EFCE2E> #<ENTREPRENEUR-FRIEND #x14EFCDDE> #<PHILOSOPHER-FRIEND #x14EFCC56>)
CL-USER> (display-notifications articles friends)

Tell DAN about "Memory Prices Down"
Tell GERRY about "Memory Prices Down"
Tell PHILIP about "Memory Prices Down"
Tell DAVID about "Memory Prices Down"
Tell DAN about "Memory speeds up"
Tell GERRY about "Memory speeds up"
Tell PHILIP about "Memory speeds up"
Tell DAVID about "Memory speeds up"
Tell DAN about "Memory Impugned"
Tell GERRY about "Memory Impugned"
Tell PHILIP about "Memory Impugned"
Tell DAVID about "Memory Impugned"
NIL
CL-USER>  
|#

;;; The process method ensures that only the right people get notified

;; the hacker gets computer articles
(defmethod process ((friend hacker-friend) 
		    (article computer-article))
  (print-notification article friend))

;; the entrepreneur gets business articles
(defmethod process ((friend entrepreneur-friend)
		    (article business-article))
  (print-notification article friend))

;; The philosopher gets all articles
(defmethod process ((friend philosopher-friend)
		    (article article))
  (print-notification article friend))

;; Default method that prints nothing in other friend-article
;; combinations. Is this not strictly necessary? Saw no errors
;; w/o this in CCL.
(defmethod process ((friend friend) (article article)))


;;; the display-notification-procedure is now rewritten to use
;;; process
(defun display-notofications-accurate (articles friends)
  (dolist (article articles)
    (dolist (friend friends)
      (process friend article))))
#|
CL-USER>  (display-notofications-accurate articles friends)

Tell PHILIP about "Memory Prices Down"
Tell DAVID about "Memory Prices Down"
Tell DAN about "Memory speeds up"
Tell GERRY about "Memory speeds up"
Tell DAVID about "Memory speeds up"
Tell DAVID about "Memory Impugned"
NIL
CL-USER> 
|#
