;;; This is the file you'll use to submit most of Problem Set 0. You answer the
;;; questions by replacing the placeholder 'fill-me-in with an answer to the
;;; problem.

;;; Certain problems may ask you to modify other files to accomplish a certain
;;; task. There are also various other files that make the problem set work, and
;;; generally you will _not_ be expected to modify or even understand this code.
;;; Don't get bogged down with unnecessary work.


;; Section 1: Problem set logistics ___________________________________________

;; This is a multiple choice question. You answer by replacing
;; the symbol 'fill-me-in with a number, corresponding to your answer.

;; You get to check multiple choice answers using the tester before you
;; submit them! So there's no reason to worry about getting them wrong.
;; Often, multiple-choice questions will be intended to make sure you have the
;; right ideas going into the problem set. Run the tester right after you
;; answer them, so that you can make sure you have the right answers.

;; What Scheme environment are we using for the code in this course?
;;   1. 6.001 Scheme
;;   2. DrScheme
;;   3. MIT Scheme
;; Fill in your answer in the next line of code:
(define answer-1.2 2)

;; Section 2: Programming warmup _____________________________________________

;; Problem 2.1: Warm-Up Stretch

(define (cube x)
  (* x x x))

(define (factorial x)
  (cond [(< x 0) (error "Factorial: Input must be positive!")]
        [(<= x 1) 1]
        [else (* x (factorial (- x 1)))]))

(define (count-pattern pattern list)
  (define (match-pattern remaining-pattern remaining-list)
    (cond [(null? remaining-pattern) 1]
          [(null? remaining-list) 0]
          [(not (eq? (car remaining-pattern) (car remaining-list))) 0]
          [else (match-pattern (cdr remaining-pattern) (cdr remaining-list))]))
  (define (count-matches count working-list)
    (cond [(null? working-list) count]
          [(eq? (car pattern) (car working-list))
           (count-matches (+ count (match-pattern (cdr pattern) (cdr working-list)))
                          (cdr working-list))]
          [else (count-matches count (cdr working-list))]))
  (count-matches 0 list))
;; Problem 2.2: Expression depth

(define (depth expr)
  'fill-me-in)

;; Problem 2.3: Tree indexing

(define (tree-ref tree index)
  (if (null? index) 
      tree
      (tree-ref (list-ref tree (car index)) (cdr index))))

;; Section 3: Matching _______________________________________________________

; Your solution to this problem doesn't go in this file.
; Instead, you need to modify match.scm to complete the matcher.

(load "match.scm")

;; Section 4: Survey _________________________________________________________

;; Please answer these questions inside the double quotes.

(define when-i-took-6.001 "")
(define hours-per-6.001-project "")
(define how-well-I-learned-6.001 "")
(define how-many-hours-this-pset-took "")
