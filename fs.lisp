(ctnicks:defpackage #:passish/fs
  (:documentation "Filesystem.")
  (:use #:coalton
        #:coalton-prelude
        #:passish/utils)
  (:export #:Pathname
           #:string->pathname
           #:pathname->string
           #:directory-files
           #:file-exists?
           #:directory-exists?
           #:exists?))

(in-package :passish/fs)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;; Should Coalton define these for all types defined in the cl package?
  (repr :native cl:pathname)
  (define-type Pathname)

  (declare string->pathname (String -> Pathname))
  (define (string->pathname path)
    "Parse a native namestring into a pathname. See
uiop:parse-native-namestring."
    (lisp Pathname (path)
      (uiop:parse-native-namestring path)))

  (declare pathname->string (Pathname -> String))
  (define (pathname->string path)
    "Convert a pathname to a native namestring. See cl:namestring."
    (lisp String (path)
      (cl:namestring path)))

  (declare directory-files (String -> String -> List Pathname))
  (define (directory-files path pattern)
    "Return a list of pathnames in the directory PATH that match PATTERN. See
uiop:directory-files."
    (lisp (List Pathname) (path pattern)
      (uiop:directory-files path pattern)))

  (declare file-exists? (String -> Boolean))
  (define (file-exists? path)
    "True if path exists and is a file. See uiop:file-exists-p."
    (lisp Boolean (path)
      (cl:if (uiop:file-exists-p path)
             True
             False)))

  (declare directory-exists? (String -> Boolean))
  (define (directory-exists? path)
    "True if path exists and is a directory. See uiop:directory-exists-p."
    (lisp Boolean (path)
      (cl:if (uiop:directory-exists-p path)
             True
             False)))

  (declare exists? (String -> Boolean))
  (define (exists? file-or-directory)
    "True if path exists."
    (or (file-exists? file-or-directory)
        (directory-exists? file-or-directory))))
