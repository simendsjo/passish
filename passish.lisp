(ctnicks:defpackage #:passish
  (:use #:coalton
        #:coalton-prelude
        #:passish/utils)
  (:local-nicknames (:env #:passish/env)
                    (:fs #:passish/fs)))
(cl:in-package :passish)

(coalton-toplevel
  (define +password-key+ "__PASSISH_PASSWORD__")
  (define +password-store-gpg-opts+ (with-default (make-list)
                                      (env:get-list "PASSWORD_STORE_GPG_OPTS")))

  (define +gpg-opts+ (append +password-store-gpg-opts+
                             (make-list "--quiet" "--yes" "--compress-algo=none" "--no-encrypt-to")))

  (define +prefix+ (string:concat (with-default "~" (alt (env:get "PASSWORD_STORE_DIR")
                                                         (env:get "HOME")))
                                  "/.password-store/"))

  ;; FIXME: Doesn't search through subfolders
  (declare all-passwords (Unit -> List String))
  (define (all-passwords)
    (pipe (fs:directory-files +prefix+ "**/*.gpg")
          (map fs:pathname->string)
          (map (string:strip-suffix ".gpg"))
          (choose id)
          (map (without-prefix +prefix+))))

  (declare key->path (String -> String))
  (define (key->path key)
    (fold string:concat "" (make-list +prefix+ key ".gpg")))

  (declare run-program ((List String) -> String))
  (define (run-program args)
    (lisp String (args)
      (cl:let ((result (uiop:run-program args :output '(:string :stripped t))))
        (cl:if (cl:string= result "")
               (cl:error "Command failed: ~a" args)
               result))))

  (declare run-gpg ((List String) -> String))
  (define (run-gpg args)
    ;; TODO: Why does it encode using base64 encoding, and then decode when echoing?
    ;; Should we support the same, or is it unneccessary as we don't run in bash?
    (run-program (fold append (make-list) (make-list (make-list "gpg") +gpg-opts+ args))))

  (declare passfile-lines (String -> (List String)))
  (define (passfile-lines key)
    (let passfile = (key->path key))
    (unless (fs:file-exists? passfile)
      (return (make-list)))
    (let content = (run-gpg (make-list "--decrypt" passfile)))
    (let lines = (split #\newline content))
    lines)

  (declare passfile-data (String -> (hashtable:HashTable String String)))
  (define (passfile-data key)
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
    (lisp Boolean (pattern text)
      (cl:if (cl:search pattern text :test 'cl:equalp)
             True
             False)))

  (declare find-passwords (List String -> List String))
  (define (find-passwords patterns)
    (pipe (all-passwords)
          (filter (fn (pass) (any ((flip passfile-contains) pass) patterns)))
          remove-duplicates
          list:sort)))
