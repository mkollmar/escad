;Copyright: Markus Kollmar
(in-package :cl-user)
;(in-package "COMMON-LISP-USER")


(defpackage :de.markus-herbert-kollmar.escad
  (:use :common-lisp)
  (:nicknames :escad)
  (:shadow #:cos :exp)
  (:export :attributes :comment :taxonomy :rel :ref_from :ref_to
	   :ad :adp :apc :ara :as :asa :asp :asr :asw :aup :cs :cls :cos :cpv :get-copyright-info :gra :grp :gsa :gsp :gtd :help :help-command :help-tutorial
	   :le :los :lov :lr :ls :lsc :lta :mr :ms :nr :ns :r :rp :rr :rra :rs :rsa :s :sc :sp :ss :sav :tv :vs
	   :call-expansion-function :file-data2string
	   :*escad-lib-dir* :*escad-view-dir* :*escad_tmp_file*)
  (:documentation "Expandable Symbolic Computer Aided Description."))

(load "escad.lisp")
