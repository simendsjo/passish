(ctnicks:defpackage #:passish/env
  (:documentation "Environment variables.")
  (:use #:coalton
        #:coalton-prelude
        #:passish/utils)
  (:export #:get
           #:get-list
           #:get-paths))

(cl:in-package :passish/env)

(coalton-toplevel
 (declare get (String -> Optional String))
 (define (get key)
   "Get the value of an environment variable."
   (lisp (Optional String) (key)
         (cl:let ((value (uiop:getenv key)))
           (cl:if value
                  (Some value)
                  (None)))))

 (declare get-list (String -> (Optional (List String))))
 (define get-list
   "Get a list of values from an environment variable. Elements are separated
by spaces."
   (.> get
       (map (split #\space))))

 (declare get-paths (String -> (Optional (List String))))
 (define get-paths
   "Get a list of paths from an environment variable. Elements are separated by
colons."
   (.> get
       (map (split #\:)))))
