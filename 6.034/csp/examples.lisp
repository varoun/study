;;;; A few CSP examples

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Map Coloring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The constraint function constraints the neighbouring variables from having
;;; the same value

(defun not-same-p (variable1 value1 variable2 value2)
  (not (equalp value1 value2)))


;;; Our choice of colors
(defvar *four-colors* '(red blue green yellow))
(defvar *five-colors* '(red blue green yellow purple))
(defparameter *colors* *four-colors*)

;;; The trivial map we use
(defvar *trivial-map*
  '((A :neighbors B D E)
    (B :neighbors A C E)
    (C :neighbors B D E)
    (D :neighbors A C E)
    (E :neighbors A B C D)))

;;; Map of the US
(defvar *US-48-states-map*
  '(
    (AL :neighbors FL GA TN MS)
    (AR :neighbors MO TN MS LA TX OK)
    (AZ :neighbors NM CO UT NE CA)
    (CA :neighbors OR NE AZ)
    (CO :neighbors WY NB KS OK NM AZ UT)
    (CT :neighbors NY MA RI)
    (DE :neighbors NJ PA MD)
    (FL :neighbors GA AL)
    (GA :neighbors SC NC TN AL FL)
    (IA :neighbors MN WI IL MO NB SD)
    (ID :neighbors WA OR NE UT WY MT)
    (IL :neighbors MI MO IA WI IN KY)
    (IN :neighbors MI OH KY IL)
    (KS :neighbors OK MO NB CO)
    (KY :neighbors WV VA TN MO IL IN OH)
    (LA :neighbors MS AR TX)
    (MA :neighbors NH VT NY CT RI)
    (MD :neighbors DE VA WV PA)
    (ME :neighbors NH)
    (MI :neighbors WI IL IN OH)
    (MN :neighbors WI IA SD ND)
    (MO :neighbors AR OK KS NB IA IL KY TN)
    (MS :neighbors AL TN AR LA)
    (MT :neighbors ND SD WY ID WA)
    (NB :neighbors KS IA SD WY CO MO)
    (NC :neighbors VA TN GA SC)
    (ND :neighbors MN SD MT)
    (NE :neighbors OR ID UT AZ CA)
    (NH :neighbors VT MA ME)
    (NJ :neighbors DE PA NY)
    (NM :neighbors TX OK CO AZ UT)
    (NY :neighbors VT MA CT NJ PA)
    (OH :neighbors PA WV KY IN MI)
    (OK :neighbors TX AR MO KS NM CO)
    (OR :neighbors WA ID NE CA)
    (PA :neighbors NY NJ DE MD WV OH)
    (RI :neighbors MA CT)
    (SC :neighbors NC GA)
    (SD :neighbors NB WY MT ND IA MN)
    (TN :neighbors AL VA NC GA MS AR MO KY)
    (TX :neighbors LA AR OK NM)
    (UT :neighbors CO NM AZ NE ID WY)
    (VA :neighbors MD WV KY TN NC)
    (VT :neighbors NY MA NH)
    (WA :neighbors MT ID OR)
    (WI :neighbors MI IL IA MN)
    (WV :neighbors PA MD VA KY OH)
    (WY :neighbors MT SD NB CO UT ID)))


;;; Build a list of map names and domains. 
(defun map-names-and-domains (regions)
  "Given a data structure representing out map, construct a list of lists where
each list has the variable name as the first element and the domain values as
the rest of the elements."
  (mapcar 
   #'(lambda (region) (cons (first region) *colors*))
   regions))
#|
CL-USER> (map-names-and-domains *trivial-map*)
((A RED BLUE GREEN YELLOW) (B RED BLUE GREEN YELLOW) (C RED BLUE GREEN YELLOW) (D RED BLUE GREEN YELLOW) (E RED BLUE GREEN YELLOW))
CL-USER> 
|#
	     
;;; Build a list of arcs 
(defun map-constraint-arcs (regions)
  "Build a list of arcs given the map data structure"
  (do ((region-index 0 (+ region-index 1))
       (region regions (rest region))
       (arc-ind 0)
       (result '()))
      ((endp region) result)
    (mapcar #'(lambda (neighbor)
		(let ((neighbor-index 
		       (index-of-element neighbor regions #'first)))
		  (if neighbor-index
		      (push
		       (make-instance 'arc
				      :arc-tail region-index 
				      :arc-head neighbor-index
				      :arc-constraint-function #'not-same-p
				      :arc-index (let ((ai arc-ind))
						   (incf arc-ind)
						   ai))
		       result)
		      (error "Could not find neighboring region"))))
	    (cddr (first region)))))
;;; Note that we have specialized PRINT-OBJECT to custom print arcs
#|
CL-USER> (map-constraint-arcs *trivial-map*)
("Arc15 4===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>3" "Arc14 4===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>2" "Arc13 4===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>1" "Arc12 4===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>0" "Arc11 3===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>4" "Arc10 3===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>2" "Arc9 3===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>0" "Arc8 2===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>4" "Arc7 2===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>3" "Arc6 2===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>1" "Arc5 1===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>4" "Arc4 1===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>2" "Arc3 1===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>0" "Arc2 0===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>4" "Arc1 0===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>3" "Arc0 0===:#<Compiled-function NOT-SAME-P #x302000BB12DF>:===>1")
CL-USER> 
|#

;;; Initializing the Map coloring constraint graph
(defun initialize-map-coloring (&optional (map *trivial-map*))
  (initialize (map-names-and-domains map)
	      (map-constraint-arcs map)))

