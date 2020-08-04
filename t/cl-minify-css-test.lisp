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

  (deftest "class-name-without-spaces - Is returned the string without spaces, newlines and tabs?" ()
    (equal-p (class-name-without-spaces "div p,
div p + p, h1,
 #myid ")
	     "div p,div p+p,h1,#myid"))
  
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
	     "/*By Umaui | License GPLv3*/div,ul,li:focus{outline:none;margin:5px 0 0 1px;}"))

  (deftest "end-bracket-index - Is returned the closing bracket index when given a opening bracket index?" ()
    (equal-p (end-bracket-index "div,ul,li:focus {outline:none; margin: 5px 0 0 1px;} body {background-color: blue;}"
				  16)
      	       51))

  (deftest "end-bracket-index - Is returned the closing bracket index when given a opening bracket index with nestled brackets?" ()
    (equal-p (end-bracket-index "div,ul,li:focus {outline:none; margin: 5px 0 0 1px;} body {background-color: blue;} @media (min-width: 30em) {div {margin: 3px 0 1px 3px;} body {background-color: blue;}}" 109)
      	       169))

  (deftest "end-bracket-index - Is returned the closing bracket index when given a opening bracket index with nestled brackets and newlines?" ()
    (equal-p (end-bracket-index "div,ul,li:focus {/*By Umaui*/
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
}" 154)
	     236))

  (deftest "end-bracket-index - Is returned the closing bracket index when given a opening bracket index with nestled brackets and with multiple media-queries?" ()
    (equal-p (end-bracket-index "div,ul,li:focus {/*By Umaui*/
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
}

@media (min-width: 30em) {
  div {
    margin: 3px 0 1px 3px;
  }
  body {
    background-color: blue;
  }
}

@media (min-width: 30em) {
  div {
    margin: 3px 0 1px 3px;
  }
  body {
    background-color: blue;
  }
}" 264)
	     346))
  
  (deftest "split-css-in-classes - Is returned correctly?" ()
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

  (deftest "split-css-in-classes - Is returned correctly when css with media-query?" ()
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
	       ("

@media (min-width: 30em) "
		 ("
  div {
    margin: 3px 0 1px 3px;
  }"
		   "
  body {
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
	     "/*By Umaui | License GPLv3*/div,ul,li:focus{outline:none;margin:5px 0 0 1px;}body{background-color:pink;}"))

  (deftest "minify-css - Is returned the string without css comments when keep-license-p t and have multiple comments and with media-query?" ()
    (equal-p (minify-css "/*By Umaui | License GPLv3*/div,ul,li:focus {/*By Umaui*/
    outline:none; /*something*/
    margin: 5px 0 0 1px;

}

body {
  font: 1em/150% Helvetica, Arial, sans-serif;
  padding: 1em;
  margin: 0 auto;
  max-width: 33em;
}

@media (min-width: 70em) {
  body {
    font-size: 130%;
  }
}

h1 {
  font-size: 1.5em;
}
"
			 :keep-license-p t)
	     "/*By Umaui | License GPLv3*/div,ul,li:focus{outline:none;margin:5px 0 0 1px;}body{font:1em/150% Helvetica, Arial, sans-serif;padding:1em;margin:0 auto;max-width:33em;}@media (min-width:70em){body{font-size:130%;}}h1{font-size:1.5em;}"))

  (deftest "minify-css - Is returned the string without css comments when keep-license-p t and have multiple comments and with multiple media-query?" ()
    (equal-p (minify-css "/*By Umaui | License GPLv3*/div,ul,li:focus {/*By Umaui*/
    outline:none; /*something*/
    margin: 5px 0 0 1px;

}

body {
  font: 1em/150% Helvetica, Arial, sans-serif;
  padding: 1em;
  margin: 0 auto;
  max-width: 33em;
}

@media (min-width: 70em) {
  body {
    font-size: 130%;
  }
}

h1 {
  font-size: 1.5em;
}

@media (max-width: 90em) {
  body {
    font-size: 130%;
  }
  h1 {
    font-size: 1em;
  }
}
"
			 :keep-license-p t)
	     "/*By Umaui | License GPLv3*/div,ul,li:focus{outline:none;margin:5px 0 0 1px;}body{font:1em/150% Helvetica, Arial, sans-serif;padding:1em;margin:0 auto;max-width:33em;}@media (min-width:70em){body{font-size:130%;}}h1{font-size:1.5em;}@media (max-width:90em){body{font-size:130%;}h1{font-size:1em;}}"))

  (deftest "minify-css - Is returned the string without css comments when keep-license-p t and have multiple comments and with id and multiple classes together?" ()
    (equal-p (minify-css "/*By Umaui | License GPLv3*/div,ul,li:focus {/*By Umaui*/
    outline:none; /*something*/
    margin: 5px 0 0 1px;

}

body {
  font: 1em/150% Helvetica, Arial, sans-serif;
  padding: 1em;
  margin: 0 auto;
  max-width: 33em;
}

@media (min-width: 70em) {
  body {
    font-size: 130%;
  }
}

h1 {
  font-size: 1.5em;
}

@media (max-width: 90em) {
  body {
    font-size: 130%;
  }
  h1 {
    font-size: 1em;
  }
}

div p,
#id:first-line {
  background-color: red;
  border-radius: 3px;
}

.some-class p {
  margin: 0;
  padding: 1em;
}

.some-class p + p {
  padding-top: 0;
}

div p > p {
  margin: 0;
}

div p < p {
  margin: 0;
}

div p ~ p {
  margin: 0;
}"
			 :keep-license-p t)
	     "/*By Umaui | License GPLv3*/div,ul,li:focus{outline:none;margin:5px 0 0 1px;}body{font:1em/150% Helvetica, Arial, sans-serif;padding:1em;margin:0 auto;max-width:33em;}@media (min-width:70em){body{font-size:130%;}}h1{font-size:1.5em;}@media (max-width:90em){body{font-size:130%;}h1{font-size:1em;}}div p,#id:first-line{background-color:red;border-radius:3px;}.some-class p{margin:0;padding:1em;}.some-class p+p{padding-top:0;}div p>p{margin:0;}div p<p{margin:0;}div p~p{margin:0;}"))

  (deftest "minify-css - Is returned the string without css comments when keep-license-p nil and have comment in media-query?" ()
    (equal-p (minify-css "/*By somebody | License GPLv3*/
div,ul,li:focus {
    outline:none; /*some comment*/
    margin: 5px 0 0 1px;
}

body {
  font: 1em/150% Helvetica, Arial, sans-serif;
  padding: 1em;
  margin: 0 auto; 
  max-width: 33em;
}

@media (min-width: 70em) {
  body {
    font-size: 130%; /*more some comment*/
  }
}

h1 {
  font-size: 1.5em;
}" :keep-license-p nil)
	     "div,ul,li:focus{outline:none;margin:5px 0 0 1px;}body{font:1em/150% Helvetica, Arial, sans-serif;padding:1em;margin:0 auto;max-width:33em;}@media (min-width:70em){body{font-size:130%;}}h1{font-size:1.5em;}"))

  (deftest "minify-css - Is returned the string without css comments when keep-license-p t and have comment in media-query?" ()
    (equal-p (minify-css "/*By somebody | License GPLv3*/
div,ul,li:focus {
    outline:none; /*some comment*/
    margin: 5px 0 0 1px;
}

body {
  font: 1em/150% Helvetica, Arial, sans-serif;
  padding: 1em;
  margin: 0 auto; 
  max-width: 33em;
}

@media (min-width: 70em) {
  body {
    font-size: 130%; /*more some comment*/
  }
}

h1 {
  font-size: 1.5em;
}" :keep-license-p t)
	   "/*By somebody | License GPLv3*/div,ul,li:focus{outline:none;margin:5px 0 0 1px;}body{font:1em/150% Helvetica, Arial, sans-serif;padding:1em;margin:0 auto;max-width:33em;}@media (min-width:70em){body{font-size:130%;}}h1{font-size:1.5em;}")))

