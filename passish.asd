;; quick build with
;; sbcl --eval '(progn (asdf:make :passish) (quit))'
(asdf:defsystem #:passish
  :description "Partial implementation of the password-store program (pass binary)"
  :author "Simen Endsjø <simendsjo+passish@gmail.com>"
  :license "MIT"
  :version "0.0.0"
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "passish"
  :entry-point "passish::main"
  :depends-on (#:coalton #:alexandria #:serapeum #:trivial-package-local-nicknames #:split-sequence #:trivial-clipboard #:clingon)
  :serial t
  :components ((:file "passish")))
