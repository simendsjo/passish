(ctnicks:defpackage #:passish
  (:use #:coalton
        #:coalton-prelude
        #:passish/utils)
  (:local-nicknames (:env #:passish/env)
                    (:fs #:passish/fs))
  (:export #:all-passwords
           #:find-passwords
           #:passfile-lines))
(cl:in-package :passish)

(coalton-toplevel
  (define +password-key+ "__PASSISH_SECRET__")
  (define +password-store-gpg-opts+ (with-default (make-list)
                                      (env:get-list "PASSWORD_STORE_GPG_OPTS")))

  (define +gpg-opts+ (append +password-store-gpg-opts+
                             (make-list "--quiet" "--yes" "--compress-algo=none" "--no-encrypt-to")))

  (define +prefix+
    (pipe (alt (env:get "PASSWORD_STORE_DIR") (env:get "HOME"))
          (with-default "~")
          (fn (x) (string:concat x "/.password-store/"))
          ;; We do this to canonicalize, e.g. replace \\foo with /foo and still
          ;; keep it as a string
          fs:string->pathname
          fs:pathname->string))

  (declare all-passwords (String -> List String))
  (define (all-passwords prefix)
    "Returns a list of all passwords, i.g. +PREFIX+/prefix/**/*.gpg. +PREFIX+
and .gpg are stripped from the output."
    (pipe (fs:directory-files prefix "**/*.gpg")
          (map fs:pathname->string)
          (map (string:strip-suffix ".gpg"))
          (choose id)
          (map (without-prefix +prefix+))))

  (declare key->path (String -> String))
  (define (key->path key)
    "Returns the path to the password file for the given key. E.g. foo/bar
returns +PREFIX+/foo/bar.gpg."
    (fold string:concat "" (make-list +prefix+ key ".gpg")))

  (declare run-program ((List String) -> String))
  (define (run-program args)
    "Runs a program, and returns the output. See uiop:run-program."
    (lisp String (args)
      (cl:let ((result (uiop:run-program args :output '(:string :stripped t))))
        (cl:if (cl:string= result "")
               (cl:error "Command failed: ~a" args)
               result))))

  (declare run-gpg ((List String) -> String))
  (define (run-gpg args)
    "Runs gpg with the given arguments. See run-program."
    (run-program (fold append (make-list) (make-list (make-list "gpg") +gpg-opts+ args))))

  (declare passfile-lines (String -> (List String)))
  (define (passfile-lines key)
    "Returns all lines from the password file for the given key."
    (let passfile = (key->path key))
    (unless (fs:file-exists? passfile)
      (return (make-list)))
    (let content = (run-gpg (make-list "--decrypt" passfile)))
    (let lines = (split #\newline content))
    lines)

  (declare passfile-data (String -> (hashtable:HashTable String String)))
  (define (passfile-data key)
    "Returns a hashtable containing the password and metadata for the given
key."
    ;; First line is password, rest is key: value pairs
    (let lines = (passfile-lines key))
    (when (list:null? lines)
      (return (hashtable:new)))
    (let pass = (with-default "" (head lines)))
    (let meta = (with-default (make-list) (tail lines)))
    (fold (fn (table kv)
            (hashtable:set! table (tuple:fst kv) (tuple:snd kv))
            table)
          (let ((table (hashtable:new)))
            (hashtable:set! table +password-key+ pass)
            table)
          (map (fn (x)
                 (let i = (with-default (string:length x) (string:substring-index ": " x)))
                 (let key = (string:substring x 0 i))
                 (let value = (string:substring x (+ i 2) (string:length x)))
                 (Tuple key value))
               meta)))

  (declare passfile-contains (String -> String -> Boolean))
  (define (passfile-contains pattern text)
    "True if the given pattern is found in the given text. See cl:search and
cl:equalp."
    (lisp Boolean (pattern text)
      (cl:if (cl:search pattern text :test 'cl:equalp)
             True
             False)))

  (declare find-passwords (List String -> List String))
  (define (find-passwords patterns)
    "Find all passwords where the password file contains any of the given
patterns."
    (pipe (all-passwords +prefix+)
          (filter (fn (pass) (any ((flip passfile-contains) pass) patterns)))
          remove-duplicates
          list:sort)))
