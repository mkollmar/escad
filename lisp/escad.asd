;; Copyright (C) 2022 Markus Kollmar (email: markuskollmar@onlinehome.de)
;;
;; This file is part of ESCAD.
;;
;; ESCAD is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; ESCAD is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with ESCAD.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Load all files needed within lisp with (asdf:load-system "escad")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
;(defpackage :de.markus-herbert-kollmar.escad-system (:use :asdf :cl))
;(in-package :de.markus-herbert-kollmar.escad-system)

(asdf:defsystem "escad"
   :description "escad: **E**xpandable **S**ymbolic **C**omputer **A**ided **D**escription."
   :version "0.1.0"
   :author "Markus Kollmar <markuskollmar@onlinehome.de>"
   :license "GNU Affero General Public License as published by the Free Software Foundation, version 3"
					;:serial t ; "serial" means following components are depending in a serial way from top to bottom (saves :depending-on writing)
   :depends-on ("hunchentoot" "com.inuoe.jzon") ; http server, secure json library
   :components ((:file "package")
		(:file "utils")
		(:file "network")
		(:file "escad"))
		;(:file "main" :depends-on ("package" "network" "utils")))
   :build-operation "asdf:program-op"
   :build-pathname "./"
   :entry-point "init-escad")
