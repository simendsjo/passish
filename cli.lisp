(cl:defpackage #:passish/cli
  (:documentation "CLI for passish")
  (:use :cl))

(in-package :passish/cli)
(cl:pushnew :deploy-console cl:*features*)

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

(defun show/handler (cmd)
  (let* ((arg (nth 0 (clingon:command-arguments cmd)))
         (passfile (concatenate 'string passish::+prefix+ arg)))
    (cond
      ((uiop:directory-exists-p passfile)
       (format t "~{~a~%~}" (passish:all-passwords passfile)))
      ((not (uiop:file-exists-p (concatenate 'string passfile ".gpg")))
       (format t "Error: ~a is not in the password store.~%" arg))
      ((clingon:opt-is-set-p cmd :clip)
       (passish/utils:set-clipboard (nth (- (clingon:getopt cmd :clip) 1)
                                         (passish:passfile-lines arg)))
       (format t "Copied ~a to clipboard. CLIPBOARD CLEARING NOT YET IMPLEMENTED.~%" arg))
      (t
       (format t "~{~a~%~}" (passish:passfile-lines arg))))))

(defun show/command ()
  (clingon:make-command
   :name "show"
   :aliases '("ls" "list")
   :description "Show existing password and optionally put it on the clipboard."
   :long-description "When matching a path, all passwords recursively under that path are shown. When matching a password file, that file is used."
   :handler #'show/handler
   :options (list
             (clingon:make-option
              :integer
              :description "Line number in the password file to copy to the clipboard. Without an argument, it's the same as -c1. Does not yet clear the clipboard!"
              :short-name #\c
              :long-name "clip"
              :key :clip))))

(defun find/handler (cmd)
  (format cl:t "~{~a~%~}" (passish:find-passwords (clingon:command-arguments cmd))))

(defun find/command ()
  (clingon:make-command
   :name "find"
   :aliases '("search")
   :description "List passwords that match pass-names."
   :long-description "Lists all passwords that case-insensive match any of the supplied parts of the name or path."
   :handler #'find/handler
   :options '()))

(defun patch-args-for-clip (args &aux (result (copy-list args)))
  (dolist (spec (list (cons "-c" "-c1")
                      (cons "--clip" "--clip=1"))
                result)
    (let* ((old (car spec))
           (new (cdr spec))
           (opt (member old result :test 'equal))
           (val (cadr opt))
           (int? (when val (parse-integer val :radix 10 :junk-allowed t))))
      (when (and opt (not int?))
        (nsubst new old result :test 'equal)))))

(defun show/main ()
  (clingon:make-command
   :name "passish"
   :description "A password manager."
   :long-description "A partial implementation of the pass (password-store) command line utility. Mostly to get an implementation working on Windows where bash can be problematic. Use at your own risk!"
   :handler (lambda (cmd)
              (clingon:print-usage-and-exit cmd t))
   :sub-commands (list
                  (show/command)
                  (find/command))))

(defun main ()
  (let ((app (show/main))
        (args (patch-args-for-clip (uiop:command-line-arguments))))
    (clingon:run app args)))
