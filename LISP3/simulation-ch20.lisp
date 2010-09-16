;;;; Simulation - Chapter 20

;;; Structures for events and tasks

;; Events
(defstruct event
  (time 'unknown)
  (input-task nil)
  (output-tasks nil))

;; Tasks
(defstruct task
  (name 'unknown)
  (input-events nil)
  (output-event nil)
  (duration nil))

;;; Connecting events and tasks together
(defun connect (task output inputs)
  (setf (task-input-events task) inputs) ; inputs
  (dolist (event inputs)
    (push task (event-output-tasks event)))
  (setf (task-output-event task) output) ; outputs
  (setf (event-input-task output) task))

;;; Wiring up the PERT chart on Pg 292/Fig 20-1
(let ((s (make-task :duration 4 :name 'select))
      (n (make-task :duration 6 :name 'negotiate))
      (d (make-task :duration 4 :name 'design))
      (p (make-task :duration 7 :name 'prepare))
      (f (make-task :duration 8 :name 'furnish))
      (m (make-task :duration 1 :name 'move))
      (e1 (make-event :time 0))
      (e2 (make-event)) (e3 (make-event))
      (e4 (make-event)) (e5 (make-event))
      (e6 (make-event)) (e7 (make-event)))
  (connect s e2 (list e1)) (connect n e3 (list e2))
  (connect d e4 (list e2)) (connect p e5 (list e3 e4))
  (connect f e6 (list e3 e4)) (connect m e7 (list e5 e6))
  (setf *start* e1))

;;; Stack overslows when this let form is evaluated. 
;;; Needs debugging

  
	