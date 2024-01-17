;;;; Copyright (C) 2021, 2022, 2023 Markus Kollmar
;;;;
;;;; This file is part of ESCAD.
;;;;
;;;; ESCAD is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; ESCAD is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License
;;;; along with ESCAD.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "COMMON-LISP-USER")
(defpackage :de.markus-herbert-kollmar.escad.std
  (:use :common-lisp :escad)
  (:nicknames :std) ; for other taxonomy put taxonomy-base before, e.g. for schema.org: schema-org.std.
  (:export :webserver :list-taxonomies :list-functions)
  (:documentation "Standard escad schema expansion for general usage not just for a particular domain. It can include taxonomies for webserver, checks, mounting, canonizing (view) or grouping (symbols)."))

(in-package :de.markus-herbert-kollmar.escad.std)

;;; Global symbols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *version* "0.1.0" "Semantic version of this expansion. Should less or be the same as current escad-version in order to state it is compatible to escad.")


;;; Taxonomy classes and methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass webserver (escad:sym)
  ((parameter
    :initarg :parameter
    :documentation "String supplied to the activate command: <start>, <stop>."
    :initform "start"
    :accessor parameter)
   (representation
    :initarg :representation
    :documentation "String with state of server: <online>, <offline>."
    :accessor representation
    :initform "offline")
   (taxonomy
    :initarg :taxonomy
    :documentation "String."
    :initform "std:webserver")))

(defmethod activate ((obj webserver))
  (with-slots ((rep representation) (par parameter)) obj
    (format t "activate webserver: rep<~s> par<~s> !" rep par)
    (cond
      ((and (string= rep "offline") (string= par "start"))
       (progn (escad:start-gui-server)
	      (setf (representation obj) "online")))
      ((and (string= rep "online") (string= par "stop"))
       (progn (escad:stop-gui-server)
	      (setf (representation obj) "offline")))
      (t nil))))

;;; Expansion management functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-taxonomies ()
  "Return list of all external taxonomies in this package."
  (let ((classes))
    (do-external-symbols (s (find-package "STD"))
      (when (find-class s nil)
        (push s classes)))
    classes))


(defun list-functions ()
  "Return list of all external functions in this package."
  (let ((functions))
    (do-external-symbols (s (find-package "STD"))
      (when (fboundp s)
        (push s functions)))
    functions))

(setf (slot-value (escad:s "_message") 'representation) (string 'std))
