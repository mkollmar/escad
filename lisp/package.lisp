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
;; Define escad package and things to export for expansions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(in-package :cl-user)
;(in-package "COMMON-LISP-USER")


(defpackage :de.markus-herbert-kollmar.escad
  (:use :common-lisp)
  (:nicknames :escad)
  (:shadow :exp)
  (:export :attributes :comment :label :representation :taxonomy :rel :ref_from :ref_to
	   :ad :adp :apc :ara :as :asa :asp :asr :asw :aup :cs :cls :cpv :get-copyright-info :gra :grp :gsa :gsp :gtd :help :help-command :help-tutorial
	   :le :los :lov :lr :ls :lsc :lt :mr :ms :nr :ns :r :rp :rr :rra :rs :rsa :s :sc :sp :ss :sav :tv :vs
	   :call-expansion-function :file-data2string
	   :*escad-lib-dir* :*escad-view-dir* :*escad_tmp_file*)
  (:documentation "Expandable Symbolic Computer Aided Description."))
