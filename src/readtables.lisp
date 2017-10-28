(in-package :bria.readtables)

(unless (named-readtables:find-readtable :cl-interpol)
  (named-readtables:defreadtable :cl-interpol
    (:merge :standard)
    (:dispatch-macro-char #\# #\? #'interpol::interpol-reader)))
