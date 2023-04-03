(ctnicks:defpackage #:passish/utils
  (:documentation "Utility functions which doesn't have package of its own.")
  (:use #:coalton
        #:coalton-prelude)
  (:export #:choose
           #:without-prefix
           #:without-suffix
           #:set-clipboard
           #:get-clipboard))

(cl:in-package :passish/utils)

(coalton-toplevel
  ;; Probably exists under a different name
  ;; iter:unwrappable! is pretty close and more general
  (declare choose ((:a -> Optional :b) -> (List :a) -> (List :b)))
  (define (choose f xs)
    "Keeps all elements of xs where (f x) is Some, and unwraps the result."
    (pipe xs
          (map f)
          (filter some?)
          (map unwrap)))

  ;; This might be useful in the coalton library
  (declare without-prefix (String -> String -> String))
  (define (without-prefix prefix text)
    "Returns text without the prefix if it exists."
    (with-default text (string:strip-prefix prefix text)))

  ;; This might be useful in the coalton library
  (declare without-suffix (String -> String -> String))
  (define (without-suffix suffix text)
    "Returns text without the suffix if it exists."
    (with-default text (string:strip-suffix suffix text)))

  (declare set-clipboard (String -> Unit))
  (define (set-clipboard text)
    (lisp Unit (text)
      (trivial-clipboard:text text)
      Unit))

  (declare get-clipboard (Unit -> String))
  (define (get-clipboard)
    (lisp String ()
      (trivial-clipboard:content))))
