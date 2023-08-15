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
(defpackage :de.markus-herbert-kollmar.escad.flow
  (:use :common-lisp :escad)
  (:nicknames :escad.flow)
  (:export :make-statement :do-ask :do-finish :check-answer)
  (:shadow #:cos)
  (:documentation "Provides flow mechanism to current view. This mechanism provides things like decision-tree."))

(in-package :de.markus-herbert-kollmar.escad.flow)
;(load "lib/pregexp/pregexp-master/pregexp.lisp")

;; USER CONFIG START
(defparameter *flow-data* '() "Data to store data for this flow.")
(defparameter *flow-type* nil "How this flow is interpreted.")
(defparameter *activated-symbols* '() "List that holds already activated symbols (to deny activation more than once).")
(defparameter *flow-input-fun* nil "Symbol name of function which is called to evaluate/register user answer.")
;; USER CONFIG END


(defun make-statement (symbol-name)
  "symbol-name -> current-symbol
Print out start-message or make initial start procedures."
  (let ((flow-input-type (gsa symbol-name "flow-input-type"))
	(next-symbol (car (rp (car (sp symbol-name 'ref_to)) 'ref_to)))
	(flow-message (gsa symbol-name "escad.attribute.string-rep")))
    (cond ((eq flow-input-type :counting)
	   (setq *flow-input-fun* 'increment-counter)
	   (setq *flow-type* :counting))
	  (t 'void-function))
    (cs next-symbol)
    flow-message))

(defun do-ask (symbol-name)
  "symbol-name -> current-symbol
Print out question to user."
  (format nil "~a" (gsa symbol-name "escad.attribute.string-rep"))
  ; show possible answer(s):
  (nth 3 (lsc symbol-name)))

(defun check-answer (symbol-name)
  "symbol-name -> current-symbol
Do things depending on registered *flow-input-fun*."
  (unless (already-activated-p symbol-name)
    (funcall *flow-input-fun* symbol-name)
    (cs symbol-name)
    (nth 3 (lsc symbol-name))))

(defun do-finish (symbol-name)
  "symbol-name -> result-string
Returny string with result of flow."
  (let ((result ""))
    (cond ((eq *flow-type* :counting)
	   (loop for (key value) on *flow-data* by #'cddr do
		(concatenate 'string result (format t "Result: ~a: ~a | " key value))))
	  (t "Sorry not able to get result"))
    result))

(defun already-activated-p (symbol-name)
  "symbol-name -> result-p
Is true if the given symbol-name is already activated, nil otherwise."
  (let ((activated nil))
    (if (member symbol-name *activated-symbols* :test #'string=)
      (setq activated t)
      (push symbol-name *activated-symbols*))
    activated))

(defun increment-counter (symbol-name)
  "symbol-name ->
Increment given counter. If not existing then create."
  (let ((counter-name (gsa symbol-name "flow-counter")))
    (incf (getf *flow-data* counter-name 0))))

(defun void-function (symbol-name)
  "symbol-name ->
Function does nothing."
  t)
