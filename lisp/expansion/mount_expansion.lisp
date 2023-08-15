;; Copyright (C) 2021 Markus Kollmar
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
(defpackage :de.markus-herbert-kollmar.escad.standard
  (:use :common-lisp :escad)
  (:nicknames :escad.standard)
  ;(:export :make-statement :do-ask :do-finish :check-answer)
  ;(:shadow #:cos)
  (:documentation "Standard expansions for checks, canonizing or sets."))

(in-package :de.markus-herbert-kollmar.escad.standard)

;; USER CONFIG START
(defparameter *std-version* "0.1" "Version.")
;; USER CONFIG END

