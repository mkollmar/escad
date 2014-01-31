;; Copyright (C) 2011, 2012, 2013 Markus Kollmar
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

(
(
(:name "escad")
(:author "Markus Kollmar")
(:doc "Standard taxonomy for escad. Note that if you create or use your own, you may not easily interchange data.")
(:license "GNU GPL 3")
(:version 1))
(
(:taxonomy "escad.attribute.homepage" :doc "Homepage with a nearer description of the object." :type "STRING")
(:taxonomy "escad.attribute._escad.config.defaultTaxonomy" :doc "" :type "STRING")
(:taxonomy "escad.attribute._escad.config.defaultWeight" :doc "" :type "STRING")
(:taxonomy "escad.attribute.symbols" :doc "List with symbols which are related to the symbol." :type "SYMBOL_LIST")
(:taxonomy "escad.symbol" :doc "base (universal) symbol")
(:taxonomy "escad.symbol._escad.export.dot" :doc "exports view to graphviz dot format, viewable in internetbrowser."
	   :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.expansion.export :function "export2dot"
	   :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.svg" :doc "exports view to a SVG graphic, viewable in internetbrowser."
	   :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.expansion.export :function "export2svg"
	   :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.report.txt" :doc "Tries to extract the basic information of view (handy for learning)."
	   :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.expansion.report2txt :function "report2txt"
	   :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.input_file" :doc "File which may take a expansion as input for further action.")
(:taxonomy "escad.symbol._escad.output_file" :doc "File which may take a expansion as output for further action.")
(:taxonomy "escad.symbol._escad.group" :doc "groups other symbols. mathematically it is a aequivalenz-relation.")
(:taxonomy "escad.symbol._escad.config" :doc "Allows config of escad via attributes.")
(:taxonomy "escad.symbol.human.man" :doc "man")
(:taxonomy "escad.symbol.human.woman" :doc "woman")
(:taxonomy "escad.symbol.petri_net.net" :doc "net")
(:taxonomy "escad.symbol.petri_net.place" :doc "place")
(:taxonomy "escad.symbol.process" :doc "process")
(:taxonomy "escad.symbol.thing" :doc "thing")
(:taxonomy "escad.symbol.topic" :doc "topic")
(:taxonomy "escad.relation" :doc "Basic, universal relation.")
(:taxonomy "escad.relation.human.has_child" :doc "A human (man or woman) has genetic child")
(:taxonomy "escad.relation.petri_net.transition" :doc "transition")
))