(in-package :cl-user)

(defun tttt ()
  (pprint "Welcome hello test...")
  )

(save-lisp-and-die "test_lispexe" :executable t :toplevel 'tttt)
