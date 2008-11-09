; sparrow, a web publishing system
;
; Copyright 2008 Michael Stipicevic
;
; see README for details

(use postgresql)
(require-extension fastcgi)
(require 'regex)

(define conn-params
  (list
    '(user . "user")
    '(dbname . "pass") ) )

(define decode-sequence
  (lambda (i)
    (let ((first (read-char i))
	  (second (read-char i)))
      (if (eof-object? first)
	#\space
	(if (eof-object? second)
	  #\space 
	  (let ((tempstring (open-output-string)))
	    (write-char first tempstring)
	    (write-char second tempstring)
	    (integer->char (string->number (get-output-string tempstring) 16))))))))

(define url-decoder-loop
  (lambda (o i)
    (let ((inchar (read-char i)))
      (if (eof-object? inchar)
	(get-output-string o)
	(begin
	  (if (char=? #\% inchar)
	    (write-char (decode-sequence i) o)
	    (if (char=? #\+ inchar)
	      (write-char #\space o)
	      (write-char inchar o)))
	  (url-decoder-loop o i))))))

(define url-decoder
  (lambda (data)
    (let ((o (open-output-string)))
      (url-decoder-loop o data))))


(set! conn
  (pg:connect conn-params ))

(define format-date
  (lambda (date-tuple out n)
    (out (number->string (vector-ref date-tuple n)))
    (if (< n 2)
      (out "-"))
    (if (< n 2)
      (format-date date-tuple out (+ n 1)))))

(define format-post
  (lambda (out)
    (lambda (tuple)
      (out "<span class=\"perma\">")
      (out (string-concatenate (list "<a href=\"/view-specific/" (number->string (vector-ref tuple 0)) "\">permalink</a>")))
      (out "</span>")
      (out "<h2 class=\"title\">")
      (out (vector-ref tuple 1))
      (out "</h2> ")
      (out "<span class=\"date\">")
      (format-date (vector-ref tuple 3) out 0)
      (out "</span>")
      (out (vector-ref tuple 2)))))

(define print-posts
  (lambda (count start out)
    (pg:query-for-each (format-post out)
		       (string-concatenate (list "select * from posts order by posttime desc limit " (number->string count) " offset " (number->string start) ";"))
		       conn)))

(define add-post
  (lambda (title content)
    (let ((title-query (pg:escape-string conn title))
	  (content-query (pg:escape-string conn content)))
      (pg:query-tuples (string-concatenate (list "insert into posts (title,content) values ('" title-query "','" content-query "');")) conn))))

(define output-vars
  (lambda (out env)
    (for-each
      (lambda (pair)
	(out "<p>")
	(out (car pair))
	(out ": ")
	(out (cdr pair))
	(out "</p>"))
      env)))

(define get-var
  (lambda (varname post-data)
    (if (null? (car post-data))
      #f
      (begin
	(let ((split-pair (string-split (car post-data) "=")))
	  (if (null? (car split-pair))
	    (get-var varname (car post-data))
	    (if (string-ci=? (car split-pair) varname)
	      (if (null? (cdr split-pair))
		#f
		(car (cdr split-pair)))
	      (get-var varname (cdr post-data)))))))))

(define massage
  (lambda (strng)
    (url-decoder (open-input-string strng))))
;    (string-substitute* strng evil-regex)))

(define check-pass
  (lambda (post)
    (let ((inputpass (get-var "pass" post)))
	  (if inputpass
	    (string-ci=? inputpass "thepassword")
	    #f))))

(define scan-list
  (lambda (lst predicate)
    (if (null? lst)
      #f
      (if (predicate (car lst))
	(car lst)
	(scan-list (cdr lst) predicate)))))

(define handle-root
  (lambda (in out env uri)
    (let ((tokens (string-split uri "/")))
      (let ((root (car tokens)))
	(cond ((string-ci=? root "view")
	       (let ((page (if (null? (cdr tokens))
			    0
			    (string->number (car (cdr tokens))))))
		 (print-posts 10 (* page 10) out)))
	      ((string-ci=? root "view-specific")
	       (if (null? (cdr tokens))
		 	  0
			  (let ((value (car (cdr tokens))))
			    (if (string->number value)
			      (pg:query-for-each (format-post out)
						 (string-concatenate (list "select * from posts where id = " value ";"))
						 conn)
			      (if (scan-list (list "about" "friends" "powered") (lambda (x) (string-ci=? value x)))
				(dump-file (string-append value ".html") out))
			      ))))

	      ((string-ci=? root "submit")
	       (let ((post-data (fcgi-get-post-data in env)))
		 (if post-data
		   (let ((tokenz (string-split post-data "&")))
		     (if (check-pass tokenz)
		       (let ((title (massage (get-var "title" tokenz)))
			     (content (massage (get-var "content" tokenz))))
			 (if title
			   (if content
			     (add-post title content)
			     (out "<p> need to insert content!</p>"))
			   (out "<p> need to insert title!</p>")))))
		   (dump-file "form.html" out)))))))))





; this is rather inefficient
(define output-chars
  (lambda (port out)
    (if (char-ready? port)
      (if (eof-object? (peek-char port))
	#t
	(begin
	  (out (string (read-char port)))
	  (output-chars port out))))))

(define dump-file
  (lambda (file out)
    (when (file-exists? file)
    (call-with-input-file file
			  (lambda (port)
			    (output-chars port out))))))


(fcgi-dynamic-server-accept-loop
  (lambda (in out err env)

    (out "Content-type: text/html\r\n\r\n")
    (dump-file "header.html" out)
    (handle-root in out env (env "REQUEST_URI"))
	;(output-vars out (env))))
    (dump-file "footer.html" out)))
(pg:close conn)
