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
(defpackage :de.markus-herbert-kollmar.escad.import
  (:use :common-lisp :escad)
  (:nicknames :escad.import)
  (:export :import-dot)
  (:shadow #:cos)
  (:documentation "Import extern graph-data to current view."))

(in-package :de.markus-herbert-kollmar.escad.import)
(load "lib/pregexp-master/pregexp.lisp")

;; USER CONFIG START
(defparameter *default-view-author* "?")
(defparameter *default-view-copyright* "?")
;; USER CONFIG END

(defun stringify (object)
  "Translates object to its string form or ensures that a string or nil is returned in case of failure."
  (cond ((symbolp object) (write-to-string object))
	((stringp object) object)
	(t nil)))


(defun import-dot (expansion-symbol-name &optional (filename "escad_test.dot"))
  "expansion-symbol-name [relative-file-name] -> current-symbol
Import dot (graphviz) into actual view. Do this if symbol will be activated!"
  (let ((absolute-filename (concatenate 'string *escad-view-dir* filename)))
    (with-open-file (in absolute-filename :direction :input :if-does-not-exist nil)
      (when in
	(do* ((line (read-line in) (read-line in nil 'eof))
	      (parsed-list '()))
	      ((eq line 'eof) "Reached end of file.")
	  (setq parsed-list (pregexp-match "(\\w+)[\\s]+->[\\s]+(\\w+).*" line))
	  (when (nth 1 parsed-list)
	    (ns (stringify (nth 1 parsed-list)))
	    (ns (stringify (nth 2 parsed-list)))
	    (format t "~S" parsed-list)
	    (nr nil (stringify (nth 1 parsed-list)) (stringify (nth 2 parsed-list)))))
	(close in)))))
