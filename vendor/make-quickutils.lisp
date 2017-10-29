(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :alist-plist
               :curry
               :ensure-gethash
               :ensure-list
               :maxf
               :once-only
               :rcurry
               :read-file-into-string
               :symb
               :with-gensyms

               )
  :package "BRIA.QUICKUTILS")
