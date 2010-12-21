;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; NAIVE BAYES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The dataset is a list of examples of the form '((features) . class). 

(defun data-point-features (example)
  "Get the features in the provided example"
  (first example))

(defun data-point-class (example)
  "Get the class of the example"
  (rest example))

;;; The count procedure counts the values each feature takes for the output
;;; classes. n11 is the number of times the feature has a value 1 for output
;;; class 1, n10 is the number of times the feature has value 1 for output class
;;; 0. 

(defun get-n11 (feature-counts)
  (first feature-counts))
(defun get-n10 (feature-counts)
  (second feature-counts))
      

(defun incf-n11 (feature-counts)
  "Increments the n11 count in the list '(n11 n10)"
  (incf (first feature-counts)))

(defun incf-n10 (feature-counts)
  "Increment the n10 count in the list '(n11 n10)"
  (incf (second feature-counts)))

(defun count-dataset (dataset)
  "Returns a list of lists, one for each feature and the total number of +ve
and negative examples. Each list  has two elements, the first the number of
times the feature is 1 for +ve examples, and the second, the number of times
the feature is 1 for -ve examples " 
  (let ((counts 
	 (mapcar #'(lambda (feature) (list 0 0)) 
		 (data-point-features (first dataset))))
	(num-positive-examples 0)
	(num-negative-examples 0))
    (dolist (example dataset (values counts num-positive-examples num-negative-examples))
      (let ((example-class (data-point-class example)))
	(if (= example-class 1) 
	    (incf num-positive-examples)
	    (incf num-negative-examples))
	(loop 
	   for feature-counts in counts
	   for feature in (data-point-features example)
	   do
	     (when (and (= example-class 1)
			(= feature 1))
	       (incf-n11 feature-counts))
	     (when (and (= example-class 0)
			(= feature 1))
	       (incf-n10 feature-counts)))))))
#|
CL-USER> (count-dataset *dataset-1*)
((1 5) (1 2) (4 1) (2 4))
5
5
CL-USER> 
|#


;;; The following procedure generates a hypothesis using Naive Bayes. The
;;; hypothesis is a list of lists, one for each feature that holds its 'R'
;;; probability values. Each list for a feature is of the form:
;;; '(R(1,1) R(0,1) R(1,0) R(0,0)). We use Laplace correction.
(defun learn-nb (data)
  "Generate a hypothesis using Naive Bayes"
  (multiple-value-bind (counts positives negatives) 
      (count-dataset data)
    (mapcar #'(lambda (feature-count)
		(let* ((r11 (/ (+ (get-n11 feature-count) 1)
			       (+ positives 2)))
		       (r01 (- 1 r11))
		       (r10 (/ (+ (get-n10 feature-count) 1)
			       (+ negatives 2)))
		       (r00 (- 1 r10)))
		  (list r11 r01 r10 r00)))
	    counts)))

#|
CL-USER> (learn-nb *dataset-1*)
((2/7 5/7 6/7 1/7) (2/7 5/7 3/7 4/7) (5/7 2/7 2/7 5/7) (3/7 4/7 5/7 2/7))
CL-USER> 
|#

;;; Predict an output class when given a hypothesis and input features.
(defun nb-predict (hyp features)
  "Given a previous learned hypothesis and a new input feature, predict the
output class "
  (loop    
     for (r11 r01 r10 r00) in hyp
     for feature in features
     summing (if (= feature 1) (log r11) (log r01)) into s1
     summing (if (= feature 1) (log r10) (log r00)) into s0
     finally (return (if (> s1 s0) 1 0))))
#|
CL-USER> (nb-predict (learn-nb *dataset-1*) '(0 0 1 1))
1
CL-USER> 
|#


;;; Simple dataset for testing
(defvar *dataset-1* 
  '(((0 1 1 0) . 1)
    ((0 0 1 1) . 1)
    ((1 0 1 0) . 1)
    ((0 0 1 1) . 1)
    ((0 0 0 0) . 1)
    ((1 0 0 1) . 0)
    ((1 1 0 1) . 0)
    ((1 0 0 0) . 0)
    ((1 1 0 1) . 0)
    ((1 0 1 1) . 0)))

