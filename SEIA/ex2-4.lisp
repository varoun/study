;;;; Chapter 2 / Exercise 4 -- 
;;;; Comparative book shopping


;;;; DATA SOURCES
;; B&N - http://search.barnesandnoble.com/books/product.aspx?EAN=<isbn>
;; Powells - http://www.powells.com/biblio?isbn=<isbn>

;; This should be a alist of store name -  store url
(defparameter *book-data-sources* nil)

;; ADT for bookstores and their urls

(defun make-book-store (store-name url)
  "Takes a name of a bookstore and its search url and glues them together"
  (list store-name url))

(defun add-bookstore-to-db (book-store)
  "book-store is a key-val pair of  a store name and its url"
  (push book-store *book-data-sources*))

(defun get-bookstore-url (bookstore-name)
  "Takes the name of a store, and looks up the url from the db"
  (second (assoc bookstore-name *book-data-sources*)))

;;; lets add bookstores to the db
(add-bookstore-to-db
 (make-book-store 'powells "http://www.powells.com/biblio?isbn="))
(add-bookstore-to-db 
 (make-book-store 'bn "http://search.barnesandnoble.com/books/product.aspx?EAN="))
#|
CL-USER> (get-bookstore-url 'powells)
"http://www.powells.com/biblio?isbn="
CL-USER> 
|#


;;; Querying the external data source
;;; no error handling at the moment
(defun get-book-results (store-name isbn)
  "Take the name of a bookstore and an isbn, query
the store for the book"
  (drakma:http-request 
   (concatenate 'string (get-bookstore-url store-name) isbn)))

;;; REGEX scanners for the various book stores

;;; Scanners for book titles
(defparameter *title-regex-scanners* nil)
(defparameter *price-regex-scanners* nil)

;;; ADT for Book Title and Price regexes
(defun make-regex-scanner (book-store regex-string)
  "Takes a bookstore name and a regex string, creates a scanner
and returns a list of the store name and the scanner"
  (list book-store (cl-ppcre:create-scanner regex-string)))

;;; Adding a scanner to the title or price dbs.

(defun add-scanner-to-title-db (scanner)
  "Takes a scanner created by make-regex-scanner and adds to 
*title-regex-scanners"
  (push scanner *title-regex-scanners*))

(defun add-scanner-to-price-db (scanner)
  "Takes a price regex scanner and adds to *price-regex-scanner"
  (push scanner *price-regex-scanners*))

(defun get-scanner-from-db (bookstore dbname)
  "Takes a name of a bookstore and a db name, and returns the scanner"
  (second (assoc bookstore dbname)))

;;; Lets add the title and price scanners to the DB
(add-scanner-to-title-db 
 (make-regex-scanner 'powells "<h2 class='book-title'>(.*)</h2>"))
(add-scanner-to-title-db
 (make-regex-scanner 'bn "'title': '(.*)'"))
(add-scanner-to-price-db 
 (make-regex-scanner 'powells "<div class=\"price\">(.*)</div>"))
(add-scanner-to-price-db
 (make-regex-scanner 'bn "<span class=\".*onlinePriceValue2.*\">(.*)[0-9]</span>"))

#|
CL-USER> (cl-ppcre:scan-to-strings (get-scanner-from-db 'powells *title-regex-scanners*) *p1*)
"<h2 class='book-title'>Harry Potter #01: Harry Potter and the Sorcerer's Stone</h2>"
#("Harry Potter #01: Harry Potter and the Sorcerer's Stone")
CL-USER> 
|#

;;; Extracting the matching value from a html page
(defun extract-data (bookstore dbname html-page)
  (multiple-value-bind
	(matching-string matching-value)
      (cl-ppcre:scan-to-strings 
       (get-scanner-from-db bookstore dbname)
       html-page)
    (if (null matching-string)
	(error 'no-regex-match :text "Search failed")
	(elt matching-value 0))))
#|
CL-USER> (extract-data 'powells 
		       *title-regex-scanners*
		       (get-book-results 'powells "0590353403"))
"Harry Potter #01: Harry Potter and the Sorcerer's Stone"
CL-USER> (extract-data 'powells 
		       *price-regex-scanners*
		       (get-book-results 'powells "0590353403"))
"$10.50"
CL-USER> 
|#

;;; Some basic error handling
(define-condition no-regex-match (error)
  ((text :initarg :text :reader text)))

;;; Given a isbn, query bookstores in the database for title and price information 

(defun query-bookstores (isbn)
  (let ((store-name (mapcar #'car *book-data-sources*)))
    (dolist (name store-name)
      (let* ((results-page 
	      (handler-case (get-book-results name isbn)
		(usocket:unknown-error () (format nil "HTTP request failed"))))
	     (title 
	      (handler-case (extract-data name *title-regex-scanners* results-page)
		(no-regex-match (data) (text data))))
	     (price 
	      (handler-case (extract-data name *price-regex-scanners* results-page)
		(no-regex-match (data) (text data)))))
	(format t "~%Store:~a~%Title:~a~%Price:~a~%" 
		name title price)))))
#|
CL-USER> (query-bookstores "0590353403")

Store:BN
Title:Harry Potter and the Sorcerer\'s Stone (Harry Potter #1)
Price:$17.9

Store:POWELLS
Title:Harry Potter #01: Harry Potter and the Sorcerer's Stone
Price:$10.50
NIL
CL-USER> (query-bookstores "1588750019")

Store:BN
Title:Travels with Samantha
Price:Search failed <-- note this!!

Store:POWELLS
Title:Travels with Samantha
Price:$7.95
NIL
CL-USER> 
|#

;;; web pages
(defun book-shop ()
  (cl-who:with-html-output-to-string 
      (str nil :prologue t :indent t)
    (:html
     (:head (:title "Comparative book shopping"))
     (:body (:h1 "Comparative book shopping")
	    (:h2 "Search multiple bookstores for a book")
	    (:form :action "/basics/book-results" :method "post"
		   (:div "ISBN" (:input :type "text" :name "isbn"))
		   (:input :type "submit" :value "Search"))))))

(defun book-results ()
  (let ((isbn (hunchentoot:parameter "isbn"))
	(store-name (mapcar #'car *book-data-sources*)))
    (cl-who:with-html-output-to-string
	(str nil :prologue t :indent t)
      (:html
       (:head (:title "Book search  results"))
       (:body (:h1 "Book search results")
	      (dolist (name store-name)
		(let* ((results-page 
			(handler-case (get-book-results name isbn)
			  (usocket:unknown-error () (format nil "HTTP request failed"))))
		       (title 
			(handler-case (extract-data name *title-regex-scanners* results-page)
			  (no-regex-match (data) (text data))))
		       (price 
			(handler-case (extract-data name *price-regex-scanners* results-page)
			  (no-regex-match (data) (text data)))))
		  (cl-who:htm (:p (:pre (cl-who:str (format nil "~%Store:~a~%Title:~a~%Price:~a~%" 
						name title price))))))))))))




;;; Start hunchentoot, setup dispatchers
(defun setup-site (&key (start-server nil))
  (when start-server
    (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 4242)))
  (push (hunchentoot:create-prefix-dispatcher "/basics/books" 'book-shop)
	hunchentoot:*dispatch-table*)
  (push (hunchentoot:create-prefix-dispatcher "/basics/book-results" 'book-results)
	hunchentoot:*dispatch-table*))
