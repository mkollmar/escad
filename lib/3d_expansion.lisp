;; Copyright (C) 2019, 2020 Markus Kollmar
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
(defpackage :de.markus-herbert-kollmar.3d
  (:use :common-lisp :escad)
  (:nicknames :3d)
  (:export :generate_x3d)
;  (:shadow #:cos)
  (:documentation "Describes 3D data with symbols and supports generating of liberate open x3d-file-format."))

(in-package :de.markus-herbert-kollmar.3d)


;; USER CONFIG START
(defparameter *x3d-header* "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE X3D PUBLIC \"ISO//Web3D//DTD X3D 3.3//EN\" \"http://www.web3d.org/specifications/x3d-3.3.dtd\">
<X3D profile='Interchange' version='3.3' xmlns:xsd='http://www.w3.org/2001/XMLSchema-instance' xsd:noNamespaceSchemaLocation='http://www.web3d.org/specifications/x3d-3.3.xsd'>
  <head>
    <meta content='X3D Test' name='title'/>
    <meta content='X3D' name='description'/>
    <meta content='Markus' name='creator'/>
    <meta content='Copyright Markus Kollmar' name='rights'/>
    <meta content='escad.3d' name='generator'/>
  </head>
  <Scene>
    <Background skyColor='1 1 1'/>
    <!--Viewpoint description='Book View' orientation='-0.747 -0.624 -0.231 1.05' position='-1.81 3.12 2.59'/-->
")
;; USER CONFIG END

(defun 

(defun generate_scad (expansion-symbol-name &optional (filename "view.scad"))
  "expansion-symbol-name [relative-file-name] -> filename
Export view to a textual dot (graphviz) file with name <view.dot>, which is viewable by a text editor.
Do this if symbol will be activated!"
  (let ((absolute-filename (concatenate 'string *escad-view-dir* filename)))
    (with-open-file (out absolute-filename :direction :output :if-exists :supersede)
		    (with-standard-io-syntax
		     (princ *x3d-header* out) (write-char #\newline out)
		     (princ "digraph schematic {" out) (write-char #\newline out)
		     (dolist (name (ls :exclude-taxonomy '("escad.symbol._escad" "escad.symbol._thisView")))
		       (princ (concatenate 'string name ";") out)
		       (write-char #\newline out))
		     (dolist (name (lr))
		       (with-slots (comment ref_from ref_to taxonomy) (gethash name escad::*relations*)
				   (loop for i from 0 to (- (length ref_from) 1) do
					 (princ (concatenate 'string (string (nth i ref_from))
							     " -> "
							     (string (nth i ref_to))
							     ";") out)
					 (write-char #\newline out))))
		     (write-char #\newline out)
		     (princ "}" out)))
    absolute-filename))

(defun generate_x3d (expansion-symbol-name &optional (filename "view.x3d"))
  "expansion-symbol-name [relative-file-name] -> filename
Export view to a textual dot (graphviz) file with name <view.dot>, which is viewable by a text editor.
Do this if symbol will be activated!"
  (let ((absolute-filename (concatenate 'string *escad-view-dir* filename)))
    (with-open-file (out absolute-filename :direction :output :if-exists :supersede)
		    (with-standard-io-syntax
		     (princ *x3d-header* out) (write-char #\newline out)
		     (princ "digraph schematic {" out) (write-char #\newline out)
		     (dolist (name (ls :exclude-taxonomy '("escad.symbol._escad" "escad.symbol._thisView")))
		       (princ (concatenate 'string name ";") out)
		       (write-char #\newline out))
		     (dolist (name (lr))
		       (with-slots (comment ref_from ref_to taxonomy) (gethash name escad::*relations*)
				   (loop for i from 0 to (- (length ref_from) 1) do
					 (princ (concatenate 'string (string (nth i ref_from))
							     " -> "
							     (string (nth i ref_to))
							     ";") out)
					 (write-char #\newline out))))
		     (write-char #\newline out)
		     (princ "}" out)))
    absolute-filename))
