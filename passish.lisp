;; #+passish-quicklisp-loaded(cl:format cl:t "passish libraries already loaded. Skipping")
;; #-passish-quicklisp-loaded(cl:progn
;;   (ql:quickload (cl:list :coalton :alexandria :serapeum :trivial-package-local-nicknames :split-sequence :trivial-clipboard :clingon))
;;   (cl:push :passish-quicklisp-loaded cl:*features*))

(cl:pushnew :deploy-console cl:*features*)

;; Temporary package only to host the custom defpackage
(cl:defpackage #:ctnicks
  (:export #:defpackage))
(cl:in-package :ctnicks)
(cl:defmacro defpackage (cl:&rest args)
  `(cl:defpackage
     ,@args
     (:local-nicknames
      (#:types #:coalton-library/types)
      (#:hash #:coalton-library/hash)
      (#:bits #:coalton-library/bits)
      (#:math #:coalton-library/math)
      (#:char #:coalton-library/char)
      (#:string #:coalton-library/string)
      (#:tuple #:coalton-library/tuple)
      (#:optional #:coalton-library/optional)
      (#:list #:coalton-library/list)
      (#:result #:coalton-library/result)
      (#:cell #:coalton-library/cell)
      (#:vector #:coalton-library/vector)
      (#:slice #:coalton-library/slice)
      (#:hashtable #:coalton-library/hashtable)
      (#:st #:coalton-library/monad/state)
      (#:iter #:coalton-library/iterator)
      (#:sys #:coalton-library/system))))

(ctnicks:defpackage #:passish/utils
  (:use #:coalton
        #:coalton-library/classes
        #:trivial-package-local-nicknames
        #:split-sequence)
  (:export #:set-clipboard
           #:get-clipboard))

(cl:in-package :passish/utils)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare set-clipboard (String -> Unit))
  (define (set-clipboard text)
    (lisp Unit (text)
      (trivial-clipboard:text text)
      Unit))

  (declare get-clipboard (Unit -> String))
  (define (get-clipboard)
    (lisp String ()
      (trivial-clipboard:content))))

(ctnicks:defpackage #:passish/env
  (:use #:coalton
        #:coalton-library/classes
        #:trivial-package-local-nicknames
        #:passish/utils
        #:split-sequence)
  (:local-nicknames (:list #:coalton-library/list))
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
    (.> get
        (map (list:split #\space))))

  (declare get-paths (String -> (Optional (List String))))
  (define get-paths
    (.> get
        (map (list:split #\:)))))

(ctnicks:defpackage #:passish/fs
  (:use #:coalton
        #:coalton-library/classes
        #:trivial-package-local-nicknames
        #:passish/utils
        #:split-sequence)
  (:export #:file-exists?
           #:directory-exists?))

(cl:in-package :passish/fs)

(coalton-toplevel
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

(ctnicks:defpackage #:passish
  (:use #:coalton
        #:coalton-library/classes
        #:trivial-package-local-nicknames
        #:passish/utils
        #:split-sequence)
  (:local-nicknames (:env #:passish/env))
  (:local-nicknames (:env #:passish/env)
   (:fs #:passish/fs)
   (:list #:coalton-library/list)
   (:string #:coalton-library/string)
   (:hashtable #:coalton-library/hashtable)
   (:tuple #:coalton-library/tuple)))
(cl:in-package :passish)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define +password-key+ "__PASSISH_PASSWORD__")
  (define +password-store-gpg-opts+ (with-default (make-list)
                                      (env:get-list "PASSWORD_STORE_GPG_OPTS")))

  (define +gpg-opts+ (list:append +password-store-gpg-opts+
                                  (make-list "--quiet" "--yes" "--compress-algo=none" "--no-encrypt-to")))

  (define +prefix+ (string:concat (with-default "~" (alt (env:get "PASSWORD_STORE_DIR")
                                                         (env:get "HOME")))
                                  "/.password-store"))

  (declare key->path (String -> String))
  (define (key->path key)
    (fold string:concat "" (make-list +prefix+ "/" key ".gpg")))

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
    (run-program (fold list:append (make-list) (make-list (make-list "gpg") +gpg-opts+ args))))

  (declare passfile-lines (String -> (List String)))
  (define (passfile-lines key)
    (let passfile = (key->path key))
    (unless (fs:file-exists? passfile)
      (return (make-list)))
    (let content = (run-gpg (make-list "--decrypt" passfile)))
    (let lines = (list:split #\newline content))
    lines)

  (declare passfile-data (String -> (hashtable:HashTable String String)))
  (define (passfile-data key)
    ;; First line is password, rest is key: value pairs
    (let lines = (passfile-lines key))
    (when (list:null? lines)
      (return (hashtable:new)))
    (let pass = (with-default "" (list:head lines)))
    (let meta = (with-default (make-list) (list:tail lines)))
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
               meta))))

;; Original pass help text:
;; Usage:
;;     pass init [--path=subfolder,-p subfolder] gpg-id...
;;         Initialize new password storage and use gpg-id for encryption.
;;         Selectively reencrypt existing passwords using new gpg-id.
;;     pass [ls] [subfolder]
;;         List passwords.
;;     pass find pass-names...
;;         List passwords that match pass-names.
;;     pass [show] [--clip[=line-number],-c[line-number]] pass-name
;;         Show existing password and optionally put it on the clipboard.
;;         If put on the clipboard, it will be cleared in 45 seconds.
;;     pass grep [GREPOPTIONS] search-string
;;         Search for password files containing search-string when decrypted.
;;     pass insert [--echo,-e | --multiline,-m] [--force,-f] pass-name
;;         Insert new password. Optionally, echo the password back to the console
;;         during entry. Or, optionally, the entry may be multiline. Prompt before
;;         overwriting existing password unless forced.
;;     pass edit pass-name
;;         Insert a new password or edit an existing password using emacsclient --create-frame --tty.
;;     pass generate [--no-symbols,-n] [--clip,-c] [--in-place,-i | --force,-f] pass-name [pass-length]
;;         Generate a new password of pass-length (or 25 if unspecified) with optionally no symbols.
;;         Optionally put it on the clipboard and clear board after 45 seconds.
;;         Prompt before overwriting existing password unless forced.
;;         Optionally replace only the first line of an existing file with a new password.
;;     pass rm [--recursive,-r] [--force,-f] pass-name
;;         Remove existing password or directory, optionally forcefully.
;;     pass mv [--force,-f] old-path new-path
;;         Renames or moves old-path to new-path, optionally forcefully, selectively reencrypting.
;;     pass cp [--force,-f] old-path new-path
;;         Copies old-path to new-path, optionally forcefully, selectively reencrypting.
;;     pass git git-command-args...
;;         If the password store is a git repository, execute a git command
;;         specified by git-command-args.
;;     pass help
;;         Show this text.
;;     pass version
;;         Show version information.

(cl:defun show/handler (cmd)
  (cl:let* ((file (cl:nth 0 (clingon:command-arguments cmd)))
            (clip? (clingon:opt-is-set-p cmd :clip))
            (clip-value (cl:when clip?
                          (clingon:getopt cmd :clip)))
            (lines (passfile-lines file))
            (selected (cl:if clip?
                             (cl:nth (cl:- clip-value 1) lines)
                             lines)))
    (cl:if clip?
           (cl:progn
             (set-clipboard selected)
             (cl:format cl:t "Copied ~a to clipboard.~%" file))
           (cl:format cl:t "~{~a~%~}" selected))))

(cl:defun show/command ()
  (clingon:make-command
   :name "show"
   :description "Show existing password and optionally put it on the clipboard. If put on the clipboard, it will be cleared in 45 seconds."
   :handler #'show/handler
   :options (cl:list
             (clingon:make-option
              :integer
              :description "Line number in the password file to copy to the clipboard. 0 means the whole file."
              :short-name #\c
              :long-name "clip"
              :key :clip))))

(cl:defun patch-args-for-clip (args cl:&aux (result (cl:copy-list args)))
  (cl:dolist (spec (cl:list (cl:cons "-c" "-c1")
                            (cl:cons "--clip" "--clip=1"))
                   result)
    (cl:let* ((old (cl:car spec))
              (new (cl:cdr spec))
              (opt (cl:member old result :test 'cl:equal))
              (val (cl:cadr opt))
              (int? (cl:when val (cl:parse-integer val :radix 10 :junk-allowed cl:t))))
      (cl:when (cl:and opt (cl:not int?))
        (cl:nsubst new old result :test 'cl:equal)))))

(cl:defun show/main ()
  (clingon:make-command
   :name "pass"
   :handler (cl:lambda (cmd)
              (clingon:print-usage-and-exit cmd cl:t))
   :sub-commands (cl:list
                  (show/command))))

(cl:defun main ()
  (cl:let ((app (show/main))
           (args (patch-args-for-clip (uiop:command-line-arguments))))
    (clingon:run app args)))
