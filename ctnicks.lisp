(named-readtables:in-readtable coalton:coalton)

(cl:defpackage #:ctnicks
  (:documentation "Dummy package to host the custom defpackage macro. Only
  exists to give shortnames to coalton-library/packages.")
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
