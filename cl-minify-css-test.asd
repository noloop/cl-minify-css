;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defsystem :cl-minify-css-test
  :author "noloop <noloop@zoho.com>"
  :maintainer "noloop <noloop@zoho.com>"
  :license "GPLv3"
  :description "cl-minify-css Test."
  :depends-on (:cacau
               :assert-p
	       :cl-minify-css)
  :defsystem-depends-on (:cacau-asdf)
  :components ((:module "t"
                :components
                ((:cacau-file "cl-minify-css-test"))))
  :perform (test-op (op c) (symbol-call :cacau '#:run
					:colorful t
					:reporter :list)))

