;; Copyright (C) 2020 Markus Kollmar
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "COMMON-LISP-USER")
(defpackage :de.markus-herbert-kollmar.escad.generator
  (:use :common-lisp :escad)
  (:nicknames :escad.generator)
  (:export :cause_trace)
  (:shadow #:cos)
  (:documentation "Generate/edit objects in graph depending on requested function."))

(in-package :de.markus-herbert-kollmar.escad.generator)
(load "lib/pregexp-master/pregexp.lisp" :print nil)

;; USER CONFIG START
;(defparameter *default-parameter* "?")
;; USER CONFIG END

(defun cause_trace (expansion-symbol-name)
  "expansion-symbol-name ->
Import dot (graphviz) into actual view. Do this if symbol will be activated!"
  (let ((symptoms-list (gsa expansion-symbol-name "symptoms")) (cause ""))
    (when (= (length symptoms-list) 2)
      (setq cause (asr (nth 0 symptoms-list) (nth 1 symptoms-list)))
      (nr nil expansion-symbol-name cause))
    (format t "A possible cause (no warranty!) could be ~a." cause)))
