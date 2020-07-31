(in-package #:noloop.cl-minify-css)

(defun trim (s)
  (string-trim '(#\Space #\Tab #\Newline) s))

(defun remove-spaces-newlines-tabs (s)
  (remove #\Space
	  (remove #\Newline
		  (remove #\Tab
			  s))))

(defun make-string-from-chars (char-list)
  "(make-string-from-chars '(#\s #\o #\m #\e)) => \"some\""
  (let ((s (make-array 0
		       :element-type 'character
		       :fill-pointer 0
		       :adjustable t)))
    (dolist (char char-list)
      (vector-push-extend char s))
    s))

(defun properties-without-spaces (s)
  "(properties-without-spaces \"margin: .67em 0;
    padding: 5px; \") => \"margin:.67em 0;padding:5px;\""
  (let ((char-list nil)
	(char-remove-p nil))
    (dolist (c (loop for char across s
		  collect char))
      (when (or (char-equal c #\:)
		(char-equal c #\;))
	(push c char-list)
	(setf char-remove-p t))
      (when (and char-remove-p
		 (not (or (char-equal c #\:)
			  (char-equal c #\;)))
		 (not (or (char-equal c #\Space)
			  (char-equal c #\Newline)
			  (char-equal c #\Tab))))
	(setf char-remove-p nil))
      (unless char-remove-p
	(push c char-list)))
    (trim (make-string-from-chars (reverse char-list)))))

(defun remove-css-comments-recursive (css &key (start-search 0) keep-license-p)
  "It removes all comments of the css code, if :keep-license-p t the comments that have \"license\" (non-case-sensitive) are kept. Use :start-search <number> to remove the comments only after a start position string."
  (let* ((css-before-start-search
	  (subseq css 0 start-search))
	 (css-after-start-search
	  (subseq css start-search))
	 (start-comment
	  (search "/*" css-after-start-search))
	 (end-comment
	  (search "*/" css-after-start-search)))
    (unless (and start-comment end-comment)
      (return-from remove-css-comments-recursive css))
    (let* ((css-before-start-comment
		(or (subseq css-after-start-search 0 start-comment) ""))
	       (css-after-end-comment
		(or (subseq css-after-start-search (+ end-comment 2)) ""))
	       (css-without-comment-with-before-code
		(format nil "~a~a~a"
			css-before-start-search
			css-before-start-comment
			css-after-end-comment))
	       (css-without-comment
		(format nil "~a~a"
			css-before-start-comment
			css-after-end-comment))
	       (has-license-p
		(search "LICENSE"
			(string-upcase (subseq css-after-start-search
					       start-comment
					       end-comment)))))
	  (if keep-license-p
	      (if has-license-p
		  (remove-css-comments-recursive css
						 :keep-license-p keep-license-p
						 :start-search (+ 2 (length css-before-start-search) end-comment))
		  (remove-css-comments-recursive css-without-comment-with-before-code
						 :keep-license-p keep-license-p))
	      (remove-css-comments-recursive css-without-comment
					     :keep-license-p keep-license-p)))))

(defun minify-css-class (css &key keep-license-p)
  "(minify-css \".myclass { margin: .67em 0; /* some comment */
    padding: 5px; } \") => \".myclass{margin:.67em 0;padding:5px;}\" "
  (let* ((class-name
	  (remove-css-comments-recursive
	   (remove-spaces-newlines-tabs (trim (subseq css
						      0
						      (search "{" css))))
	   :keep-license-p keep-license-p))
	 (class-value
	  (remove-css-comments-recursive
	   (trim (subseq css
			 (+ (search "{" css) 1)
			 (- (length css) 2)))
	   :keep-license-p keep-license-p)))
    (format nil "~a{~a}"
	    class-name
	    (properties-without-spaces class-value))))

(defun end-bracket-index (css start-bracket)
  (let (())))

(defun split-css-in-classes (css &optional classes)
  (let ((start-bracket
	 (search "{" css))
	(end-bracket
	 (search "}" css)))
    (unless (and start-bracket end-bracket)
      (return-from split-css-in-classes (reverse classes)))
    (let* ((css-before-start-bracket
	    (subseq css 0 start-bracket))
	   (media-query-p
	    (search "@media" css-before-start-bracket)))
      (if media-query-p
	  (let* ((css-between-media-query
		  (subseq css
			  start-bracket
			  (end-bracket-index css start-bracket)))
		 (media-query-classes
		  (split-css-in-classes css-between-media-query))
		 (css-after-media-query
		  (subseq css (+ 1 (end-bracket-index css start-bracket)))))
	    (push (list css-before-start-bracket
			media-query-classes)
		  classes)
	    (split-css-in-classes css-after-media-query
				  classes))
	  (let ((css-between-brackets
		 (subseq css (+ 1 start-bracket) end-bracket))
		(css-after-end-bracket
		 (subseq css (+ 1 end-bracket))))
	    (push (format nil "~a{~a}"
			  css-before-start-bracket
			  css-between-brackets) 
		  classes)
	    (split-css-in-classes css-after-end-bracket
				  classes))))))

(defun minify-css (css &key keep-license-p)
  "It minify a css code removing spaces, newlines, tabs, comments. If want keep the license comment use :keep-license-p t."
  (reduce (lambda (acc curr)
	    (format nil "~a~a" curr acc))
	  (split-css-in-classes css)
	  :key (lambda (class)
		 (minify-css-class class
				   :keep-license-p keep-license-p))))

;; body {
;;   font: 1em/150% Helvetica, Arial, sans-serif;
;;   padding: 1em;
;;   margin: 0 auto;
;;   max-width: 33em;
;; }

;; @media (min-width: 70em) {
;;   body {
;;     font-size: 130%;
;;   }
;; }

;; h1 {
;;   font-size: 1.5em;
;; }

;; div p,
;; #id:first-line {
;;   background-color: red;
;;   border-radius: 3px;
;; }

;; div p {
;;   margin: 0;
;;   padding: 1em;
;; }

;; div p + p {
;;   padding-top: 0;
;; }
