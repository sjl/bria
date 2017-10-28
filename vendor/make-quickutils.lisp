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
               :symb
               :with-gensyms

               )
  :package "BRIA.QUICKUTILS")
