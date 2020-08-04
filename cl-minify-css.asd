;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defsystem :cl-minify-css
  :author "noloop <noloop@zoho.com>"
  :maintainer "noloop <noloop@zoho.com>"
  :license "GPLv3"
  :version "1.0.0"
  :homepage "https://github.com/noloop/cl-minify-css"
  :bug-tracker "https://github.com/noloop/cl-minify-css/issues"
  :source-control (:git "git@github.com:noloop/cl-minify-css.git")
  :description "To minify css with common lisp."
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "cl-minify-css" :depends-on ("package")))))
  :in-order-to ((test-op (test-op "cl-minify-css-test"))))

