(ctnicks:defpackage #:passish/fs
  (:use #:coalton
        #:coalton-prelude
        #:passish/utils)
  (:export #:Pathname
           #:pathname->string
           #:directory-files
           #:file-exists?
           #:directory-exists?
           #:exists?))

(cl:in-package :passish/fs)

(coalton-toplevel
  ;; Should Coalton define these for all types defined in the cl package?
  (repr :native cl:pathname)
  (define-type Pathname)

  (declare pathname->string (Pathname -> String))
  (define (pathname->string path)
    (lisp String (path)
      (cl:namestring path)))

  (declare directory-files (String -> String -> List Pathname))
  (define (directory-files path pattern)
    (lisp (List Pathname) (path pattern)
      (uiop:directory-files path pattern)))


  (declare file-exists? (String -> Boolean))
  (define (file-exists? path)
    (lisp Boolean (path)
      (cl:if (uiop:file-exists-p path)
             True
             False)))

  (declare directory-exists? (String -> Boolean))
  (define (directory-exists? path)
    (lisp Boolean (path)
      (cl:if (uiop:directory-exists-p path)
             True
             False)))

  (declare exists? (String -> Boolean))
  (define (exists? file-or-directory)
    (or (file-exists? file-or-directory)
        (directory-exists? file-or-directory))))
