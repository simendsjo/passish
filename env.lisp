(ctnicks:defpackage #:passish/env
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
    (lisp (Optional String) (key)
      (cl:let ((value (uiop:getenv key)))
        (cl:if value
               (Some value)
               (None)))))

  (declare get-list (String -> (Optional (List String))))
  (define get-list
    (\.> get
         (map (split #\space))))

  (declare get-paths (String -> (Optional (List String))))
  (define get-paths
    (\.> get
         (map (split #\:)))))
