;; Load all files needed within lisp with (asdf:load-system "escad")

;; (in-package :asdf)

(defsystem "escad"
   :description "escad: **E**xpandable **S**ymbolic **C**omputer **A**ided **D**escription."
   :version "0.0.1"
   :author "Markus Kollmar <markuskollmar@onlinehome.de>"
   :licence "GNU Affero General Public License as published by the Free Software Foundation, version 3"
   :components ((:file "packages")
                (:file "escad" :depends-on ("packages"))))
