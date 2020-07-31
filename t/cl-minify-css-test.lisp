(defpackage #:noloop.cl-minify-css-test
  (:use #:common-lisp
	#:cacau
	#:assert-p
	#:cl-minify-css)
  (:nicknames #:cl-minify-css-test))
(in-package #:noloop.cl-minify-css-test)

(defsuite :cl-minify-css-test ()

  (deftest "trim - Is returned the string without spaces, tabs and newlines?" ()
    (equal-p (trim (format nil " ~%some,~%  more-some, more-more-some  ~%"))
	     (format nil "some,~%  more-some, more-more-some")))

  (deftest "remove-spaces-newlines-tabs - Is returned the string without spaces, newlines and tabs?" ()
    (equal-p (remove-spaces-newlines-tabs (format nil " ~%some,~%  more-some, more-more-some  ~%"))
	     (format nil "some,more-some,more-more-some")))
  
  (deftest "properties-without-spaces - Is returned the string without spaces, tabs and newlines in one css class properties?" ()
    (equal-p (properties-without-spaces "margin: .67em 0;
    padding: 5px; ")
	     "margin:.67em 0;padding:5px;"))

  (deftest "remove-css-comments-recursive - Is returned the string without css comments when keep-license-p nil and the comment isn't license?" ()
    (equal-p (remove-css-comments-recursive "/*By Umaui*/div,ul,li:focus{outline:none;}"
					    :keep-license-p nil)
	     "div,ul,li:focus{outline:none;}"))

  (deftest "remove-css-comments-recursive - Is returned the string without css comments when keep-license-p nil and the comment is license?" ()
    (equal-p (remove-css-comments-recursive "/*By Umaui | License GPLv3*/div,ul,li:focus{outline:none;}"
					    :keep-license-p nil)
	     "div,ul,li:focus{outline:none;}"))

  (deftest "remove-css-comments-recursive - Is returned the string without css comments when keep-license-p nil and have multiple comments?" ()
    (equal-p (remove-css-comments-recursive "/*By Umaui | License GPLv3*/div,ul,li:focus{/*By Umaui*/outline:none;/*By Umaui | License GPLv3*/}/*By Umaui*/"
					    :keep-license-p nil)
	     "div,ul,li:focus{outline:none;}"))
  
  (deftest "remove-css-comments-recursive - Is returned the string without css comments when keep-license-p t and the comment isn't license?" ()
    (equal-p (remove-css-comments-recursive "/*By Umaui*/div,ul,li:focus{outline:none;}"
					    :keep-license-p t)
	     "div,ul,li:focus{outline:none;}"))

  (deftest "remove-css-comments-recursive - Is returned the string without css comments when keep-license-p t and the comment is license?" ()
    (equal-p (remove-css-comments-recursive "/*By Umaui | License GPLv3*/div,ul,li:focus{outline:none;}"
					    :keep-license-p t)
	     "/*By Umaui | License GPLv3*/div,ul,li:focus{outline:none;}"))

  (deftest "remove-css-comments-recursive - Is returned the string without css comments when keep-license-p t and have multiple comments?" ()
    (equal-p (remove-css-comments-recursive "/*By Umaui | License GPLv3*/div,ul,li:focus{/*By Umaui*/outline:none;}"
					    :keep-license-p t)
	     "/*By Umaui | License GPLv3*/div,ul,li:focus{outline:none;}"))

  (deftest "remove-css-comments-recursive - Is returned the string without css comments when keep-license-p t and have multiple comments and multiple licenses?" ()
    (equal-p (remove-css-comments-recursive "/*By Umaui | License GPLv3*/div,ul,li:focus{/*By Umaui*/
  outline:none;/*By Umaui | License GPLv3*/}/*By Umaui*/"
					    :keep-license-p t)
	     "/*By Umaui | License GPLv3*/div,ul,li:focus{
  outline:none;/*By Umaui | License GPLv3*/}"))

  (deftest "minify-css-class - Is returned the string without css comments when keep-license-p nil and have multiple comments?" ()
    (equal-p (minify-css-class "/*By Umaui | License GPLv3*/div,ul,li:focus {/*By Umaui*/
    outline:none; /*something*/
    margin: 5px 0 0 1px;
}"
			       :keep-license-p nil)
	     "div,ul,li:focus{outline:none;margin:5px 0 0 1px;}"))

  (deftest "minify-css-class - Is returned the string without css comments when keep-license-p t and have multiple comments?" ()
    (equal-p (minify-css-class "/*By Umaui | License GPLv3*/div,ul,li:focus {/*By Umaui*/
    outline:none; /*something*/
    margin: 5px 0 0 1px;
}"
			       :keep-license-p t)
	     "/*ByUmaui|LicenseGPLv3*/div,ul,li:focus{outline:none;margin:5px 0 0 1px;}"))

  (deftest "split-css-in-classes - Is returned correctly?" (:only)
    (equal-p (split-css-in-classes "/*By Umaui | License GPLv3*/div,ul,li:focus {/*By Umaui*/
    outline:none; /*something*/
    margin: 5px 0 0 1px;

}

body {
    background-color: pink;
}")
	     '("/*By Umaui | License GPLv3*/div,ul,li:focus {/*By Umaui*/
    outline:none; /*something*/
    margin: 5px 0 0 1px;

}"
	       "

body {
    background-color: pink;
}")))

  (deftest "split-css-in-classes - Is returned correctly when css with media-query?" (:only)
    (equal-p (split-css-in-classes "/*By Umaui | License GPLv3*/div,ul,li:focus {/*By Umaui*/
    outline:none; /*something*/
    margin: 5px 0 0 1px;

}

body {
    background-color: pink;
}

@media (min-width: 30em) {
  div {
    margin: 3px 0 1px 3px;
  }
  body {
    background-color: blue;
  }
}")
	     '("/*By Umaui | License GPLv3*/div,ul,li:focus {/*By Umaui*/
    outline:none; /*something*/
    margin: 5px 0 0 1px;

}"
	       "

body {
    background-color: pink;
}"
	       '("@media (min-width: 30em) "
		 '("div {
    margin: 3px 0 1px 3px;
  }"
		   "body {
    background-color: blue;
  }")))))
  
  (deftest "minify-css - Is returned the string without css comments when keep-license-p t and have multiple comments?" ()
    (equal-p (minify-css "/*By Umaui | License GPLv3*/div,ul,li:focus {/*By Umaui*/
    outline:none; /*something*/
    margin: 5px 0 0 1px;

}

body {
    background-color: pink;
}"
			 :keep-license-p t)
	     "/*ByUmaui|LicenseGPLv3*/div,ul,li:focus{outline:none;margin:5px 0 0 1px;}body{background-color:pink;}")))
