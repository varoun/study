;;;; Chapter 2 / Ex 2

(defconstant *day-names* 
  '("Monday" "Tuesday" "Wednesday" "Thursday" 
    "Friday" "Saturday" "Sunday"))

(defun date-time ()
  (multiple-value-bind 
	(second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time (get-universal-time))
    (format nil "~2,'0d:~2,'0d:~2,'0d :: ~a, ~d/~2,'0d/~d (GMT~@d)"
	    hour
	    minute
	    second
	    (nth day-of-week *day-names*)
	    date
	    month
	    year
	    (- tz))))


(defun my-first-program ()
  (cl-who:with-html-output-to-string 
      (str nil :prologue t :indent t)
    (:html
     (:head (:title "Current Time"))
     (:body (:h1 "Current Time")
	    (cl-who:htm (:p (cl-who:str (date-time))))))))

#|
CL-USER> (require 'asdf)
ASDF
("ASDF")
CL-USER> (push #p"/home/varoun/.asdf-install-dir/systems/" asdf:*central-registry*)
(#P"/home/varoun/.asdf-install-dir/systems/" (DIRECTORY-NAMESTRING *DEFAULT-PATHNAME-DEFAULTS*))
CL-USER> (asdf:oos 'asdf:load-op 'hunchentoot)
#<LOAD-OP NIL #x302000900F9D>
CL-USER> (asdf:oos 'asdf:load-op 'cl-who)
#<LOAD-OP NIL #x30200139218D>
CL-USER> (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 4242))
#<ACCEPTOR (host *, port 4242)>
CL-USER> (push (hunchentoot:create-prefix-dispatcher "/basics/my-first-program" 'my-first-program) hunchentoot:*dispatch-table*)
(#<COMPILED-LEXICAL-CLOSURE (:INTERNAL HUNCHENTOOT:CREATE-PREFIX-DISPATCHER) #x30200131400F> HUNCHENTOOT:DISPATCH-EASY-HANDLERS HUNCHENTOOT:DEFAULT-DISPATCHER)
CL-USER> 
|#

;;; This enables logging:
#|
CL-USER> (setq hunchentoot:*message-log-pathname* #p"/home/varoun/tmp/logs/message")
#P"/home/varoun/tmp/logs/message"
CL-USER> (setq hunchentoot:*access-log-pathname* #p"/home/varoun/tmp/logs/access")
#P"/home/varoun/tmp/logs/access"
CL-USER> 
|#


;;; Generating an error

(defun generate-error ()
  (cl-who:with-html-output-to-string 
      (str nil :prologue t :indent t)
    (:html
     (:head (:title "Current Time"))
     (:body (:h1 "Current Time")
	    (cl-who:htm (:p (cl-who:str (/ 1 0))))))))

#|
CL-USER> (push (hunchentoot:create-prefix-dispatcher "/basics/error" 'generate-error) hunchentoot:*dispatch-table*)
(#<COMPILED-LEXICAL-CLOSURE (:INTERNAL HUNCHENTOOT:CREATE-PREFIX-DISPATCHER) #x3020013E154F> #<COMPILED-LEXICAL-CLOSURE (:INTERNAL HUNCHENTOOT:CREATE-PREFIX-DISPATCHER) #x3020012577BF> HUNCHENTOOT:DISPATCH-EASY-HANDLERS HUNCHENTOOT:DEFAULT-DISPATCHER)
CL-USER> 
|#

;;; The "DIVISION-BY-ZERO detected" error is logged to the message
;;; file when the uri is visited.
