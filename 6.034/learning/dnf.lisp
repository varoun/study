;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Learning Boolean Functions using Disjunctive Normal Form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Represent a DNF function as a list of lists, example: ((1 2) (3 4)). The
;;; inner list represents conjunction and the outer list disjunction. The
;;; integers are indices into the feature vector, with 1 based index. In the
;;; example this means (f1 ^ f2) v (f3 ^ f4)). A negetive feature index
;;; indicates a negated feature, i.e, (-3 4) is (~f3 ^ f4). 


;;; Learning Algorithm - Given a data set return a hypothesis. Epsilon
;;; represents our tolerance for noise, a value of 0 allows no errors.

(defun learn-dnf (data epsilon)
  "Given the dataset and a tolerance for noise, return the hypothesis in DNF"
  (let ((positive-examples (extract-matching-examples data 1))
	(negative-examples (extract-matching-examples data 0)))
    (learn-dnf-aux positive-examples
		   negative-examples
		   '()
		   (* (length positive-examples) epsilon)
		   (* (length negative-examples) epsilon))))


(defun learn-dnf-aux (p n h np nn)
  "Given positive examples p, negative examples n, an initial hypothesis h, and
the number of errors in the positive and negative examples np and nn we
tolerate, return a DNF hypothesis"
  (if (<= (length p) np) ; the number of uncovered positive examples is less than
		; tolerated error
      h ; return the hypothesis
      (let* ((r (make-rule p n '() nn))
	     (new-p (remove-covered-examples p r)))
	(if (= (length new-p) (length p)) ; new rule did not cover any positive
					; examples 
	    (format t 
		    "~%Failed to find hypothesis that meets error tolerance: ~a"
		    h)
	    (learn-dnf-aux new-p n (cons r h) np nn)))))

(defun make-rule (p n r nn)
  "Construct a conjunct given the current positive and negative examples, p and
n, the conjunct so far, r, and the tolerance for error in the negative examples
nn." 
  (if (<= (length n) nn) ; uncovered examples less than error tolerance
      r ; return the current conjunct
      (let* ((f (best-feature p n r))
	     (new-n (remove-excluded-examples n (list f))))
	(if (= (length n) (length new-n)) ; the best feature is no good
	    r ; give-up
	    (make-rule p new-n (cons f r) nn)))))

(defun best-feature (p n r)
  "Given positive examples - p, negative examples - n, and the conjunct - r find
the best feature to add to the rule" 
  (let ((num-features (length (first p))))
    (do* ((i (- num-features) (+ i 1))
	  (best-score-so-far 0)
	  (best-feature-so-far 0))
	 ((> i num-features) best-feature-so-far)
      (unless (= i 0) ; index is 1 based
	(let ((score (feature-score i p n r)))
	  (when (> score best-score-so-far)
	    (setf best-score-so-far score)
	    (setf best-feature-so-far i)))))))

(defun feature-score (f p n r)
  "Compute the feature score for f in the context of current rule - r."
  (let ((np (num-of-covered-examples p (cons f r)))
	(nn (num-of-covered-examples n (cons f r))))
    (if (= nn 0) ; check for division by zero
	(* np 1000000)
	(/ np nn))))


;;; Given a conjunction and a datapoint, coversp evals to true if all the
;;; features have a value that would make the example true. This means all
;;; positive features should be 1 and all negated features should be 0.
(defun coversp (conjunct datapoint)
  "Given a conjunct and a datapoint, eval to true if ALL the features in the
conjunct have values that make the example true"
  (if (null conjunct) 
      t    ;empty conjunct is true
      (let* ((feature (first conjunct))
	     (index (- (abs feature) 1)))
	(cond ((and (= (elt datapoint index) 0)
		    (plusp feature)) nil) ;non negated feature is 0
	      ((and (= (elt datapoint index) 1)
		    (minusp feature)) nil) ;negated feature is 1
	      (t
	       (coversp (rest conjunct) datapoint))))))
		 


;;; Given a dataset and a conjunct, count the number of covered examples, i.e,
;;; the number of examples for which the rule would evaluate to true.
(defun num-of-covered-examples (dataset conjunct)
  "Count the number of examples in the dataset that would be covered by the
conjunct"
  (let ((count 0))
    (dolist (datapoint dataset count)
      (when (coversp conjunct datapoint)
	(incf count)))))

;;; Given a dataset and a rule, return the dataset with all the datapoints that
;;; the rule would cover removed.
(defun remove-covered-examples (dataset rule)
  "Remove the datapoints that are covered by the rule from the dataset"
  (remove-if #'(lambda (datapoint) (coversp rule datapoint))
	     dataset))

;;; Given a dataset and a rule, return a dataset with the datapoints excluded by
;;; the rule removed. (excluded = not covered)
(defun remove-excluded-examples (dataset rule)
  "Remove excluded datapoints from the dataset"
  (remove-if-not #'(lambda (datapoint) (coversp rule datapoint))
		 dataset))

(defun extract-matching-examples (data v)
  "Return all X components of the dataset whose Y component is V" 
  (let ((result '()))
    (dolist (example data result)
      (when (= (data-point-class example) v)
	(setf result (cons (data-point-features example) result))))))

;;; DATA-POINT-CLASS and DATA-POINT-FEATURES extract the X values and Y values
;;; from each datapoint. For now, we use a list to represent each datapoint,
;;; with the last item representing the Y and the previous items representing
;;; the Xs.
(defun data-point-class (datapoint)
  "Return the class, 0 or 1, of the datapoint. This is the Y value" 
  (first (last datapoint)))

(defun data-point-features (datapoint)
  "Return the features for the datapoint"
  (butlast datapoint))

;;; Test data from lecture notes

(defvar *dataset-1*
  '((0 1 1 0 0)
    (1 0 1 1 1)
    (1 1 1 0 1)
    (0 0 1 1 1)
    (1 0 0 1 0)
    (0 1 1 1 1)))

;;; When we allow negated features, we get a different hypothesis from the
;;; earlier '((2 1) (4 3))
#|
CL-USER> (learn-dnf *dataset-1* 0)
((1 -4) (4 3))
CL-USER> 
|#
