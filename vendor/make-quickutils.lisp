(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :alist-plist
               :curry
               :maxf
               :once-only
               :rcurry
               :read-file-into-string
               :with-gensyms

               )
  :package "BRIA.QUICKUTILS")
