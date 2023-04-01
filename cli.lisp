(cl:pushnew :deploy-console cl:*features*)

(cl:defpackage #:passish/cli
  (:use :cl))

(in-package :passish/cli)
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
  (let* ((file (nth 0 (clingon:command-arguments cmd)))
            ;; TODO: If "file" is a subpath, print all files in this subpath
            (lines (passish::passfile-lines file))
            (clip? (clingon:opt-is-set-p cmd :clip))
            (clip-value (when clip?
                          (clingon:getopt cmd :clip)))
            (selected (if clip?
                             (nth (- clip-value 1) lines)
                             lines)))
    (if clip?
           (progn
             (passish/utils::set-clipboard selected)
             (format t "Copied ~a to clipboard.~%" file))
           (format t "~{~a~%~}" selected))))

(defun show/command ()
  (clingon:make-command
   :name "show"
   :aliases '("ls" "list")
   :description "Show existing password and optionally put it on the clipboard. If put on the clipboard, it will be cleared in 45 seconds."
   :handler #'show/handler
   :options (list
             (clingon:make-option
              :integer
              :description "Line number in the password file to copy to the clipboard. 0 means the whole file."
              :short-name #\c
              :long-name "clip"
              :key :clip))))

;; TODO: calling find-passwords outside a coalton block isn't safe (I think),
;; but calling in a coalton block doesn't return the list, but a function.
;; TODO: This only outputs very raw output without the tree style
(defun find/handler (cmd)
  (format cl:t "~{~a~%~}" (passish::find-passwords (clingon:command-arguments cmd))))

(defun find/command ()
  (clingon:make-command
   :name "find"
   :aliases '("search")
   :description "List passwords that match pass-names."
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
   :name "pass"
   :handler (lambda (cmd)
              (clingon:print-usage-and-exit cmd t))
   :sub-commands (list
                  (show/command)
                  (find/command))))

(defun main ()
  (let ((app (show/main))
           (args (patch-args-for-clip (uiop:command-line-arguments))))
    (clingon:run app args)))
