;;;; Chapter 13 - Structures

(defstruct person
  (sex nil)
  (personality 'nice))
#|
CL-USER> (setf per1 (make-person :sex 'male))
#S(PERSON :SEX MALE :PERSONALITY NICE)
CL-USER> (setf per2 (make-person))
#S(PERSON :SEX NIL :PERSONALITY NICE)
CL-USER> (describe per1)
#S(PERSON :SEX MALE :PERSONALITY NICE)
Type: PERSON
Class: #<STRUCTURE-CLASS PERSON>
SEX: MALE
PERSONALITY: NICE
; No value
CL-USER> (person-sex per1)
MALE
CL-USER> (person-p per1)
T
CL-USER> (person-p nil)
NIL
CL-USER> (setf (person-sex per2) 'female)
FEMALE
CL-USER> (person-sex per2)
FEMALE
|#

(defstruct employee 
  (length-of-service 0)
  (payment 'salary))

(defstruct (hacker (:include employee))
  (preferred-language 'lisp))
#|
CL-USER> (setf emp1 (make-employee))
#S(EMPLOYEE :LENGTH-OF-SERVICE 0 :PAYMENT SALARY)
CL-USER> (setf emp2 (make-hacker))
#S(HACKER :LENGTH-OF-SERVICE 0 :PAYMENT SALARY :PREFERRED-LANGUAGE LISP)
CL-USER> (setf emp3 (make-employee :length-of-service 4))
#S(EMPLOYEE :LENGTH-OF-SERVICE 4 :PAYMENT SALARY)
CL-USER> (hacker-preferred-language emp2)
LISP
CL-USER> (hacker-payment emp2)
SALARY
CL-USER> (hacker-length-of-service emp3)
4
CL-USER> (hacker-p emp3)
NIL
CL-USER> (hacker-p emp2)
T
CL-USER> 
|#
