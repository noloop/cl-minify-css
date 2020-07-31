(defpackage #:noloop.cl-minify-css
  (:use #:common-lisp)
  (:nicknames #:cl-minify-css)
  (:export #:trim
	   #:remove-spaces-newlines-tabs
	   #:make-string-from-chars
	   #:properties-without-spaces
	   #:remove-css-comments-recursive
	   #:minify-css-class
	   #:split-css-in-classes
	   #:minify-css))
