;; Copyright (C) 2011, 2012, 2013, 2014, 2019 Markus Kollmar
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
;;
;; DATA-STRUCTURE-DESCRIPTION:
;;   :taxonomy "escad.attribute" -> :doc = explains what the attribute describes,
;;                                  :type = specifies type of attribute as NUMBER, STRING, STRING_LIST (for input parsing).
;;   :taxonomy "escad.relation"  -> :doc = explains what the relation describes.
;;   :taxonomy "escad.symbol"    -> :doc = explains what the symbol describes,
;;                                  :expansion = file-name of expansion relative to lib-dir of escad,
;;                                  :package = package of a expansion,
;;                                  :function = function of a expansion which to call when symbol is activated,
;;                                  :license = license of expansion data and code.
;;;;

(
(
(:name "escad")
(:author "Markus Kollmar")
(:doc "Standard taxonomy for escad. Note that if you create or use your own, you may not easily interchange data.
Doc-strings of the taxonomies begin with [] field, which tells - if necessary, separated with comma - following things:
E -> means a Expansion is using this symbol by symbol-activation with as-command to execute a function.
S -> means this symbol/relation should be insert only one time (Single) in the schematic.")
(:license "GNU GPL 3")
(:version 1))
(
(:taxonomy "escad.attribute.label" :doc "Text as a (nicer) second name for the symbol (not used to reference symbol!)." :type "STRING")
(:taxonomy "escad.attribute.url" :doc "Link (URL) to a information source." :type "STRING")
(:taxonomy "escad.attribute.default_taxonomy" :doc "" :type "STRING")
(:taxonomy "escad.attribute.default_weight" :doc "Number from -1 .. 0 .. 1" :type "NUMBER")
(:taxonomy "escad.attribute.excluded_symbols" :doc "List with symbols which should not be used by a expansion." :type "STRING_LIST")
(:taxonomy "escad.attribute.subtopic_relation" :doc "Defines which relation-taxonomy will be interpreted as subtopic indicator" :type "STRING")

(:taxonomy "escad.relation" :doc "Root, universal relation.")
(:taxonomy "escad.relation.has_child" :doc "A human (man or woman) has genetic child.")
(:taxonomy "escad.relation.has_subtopic" :doc "subtopic.")
(:taxonomy "escad.relation.transition" :doc "Transition from one state to another.")
(:taxonomy "escad.relation.3d.placed_at" :doc "Place object directly at another object or with some offset.")

(:taxonomy "escad.symbol" :doc "Root, universal symbol.")
(:taxonomy "escad.symbol._escad" :doc "[S] Escad related things (settings,...).")
(:taxonomy "escad.symbol._thisView" :doc "[S] Related things to current view (name, author,...).")
(:taxonomy "escad.symbol._escad.export.dot" :doc "[E] exports view to graphviz dot format, viewable in internetbrowser."
	   :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2dot"
	   :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.svg" :doc "[E] exports view to a SVG graphic, viewable in internetbrowser."
	   :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2svg"
	   :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.report" :doc "[S,E] General settings for report symbols.")
(:taxonomy "escad.symbol._escad.report.html" :doc "[E] Tries to extract the basic information of view (handy for learning)."
	   :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.report :function "report2html"
	   :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.report.txt" :doc "[E] Tries to extract the basic information of view (handy for learning)."
	   :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.report :function "report2txt"
	   :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.group" :doc "groups other symbols. mathematically it is a aequivalenz-relation.")
(:taxonomy "escad.symbol.3d.generate.x3d" :doc "[E] Generates 3D File in liberate X3D-Format (XML based, can viewed with mordern browser)."
	   :expansion "3d_expansion.lisp" :package :de.markus-herbert-kollmar.escad.3d :function "generate_x3d"
	   :license "GNU GPL 3")
(:taxonomy "escad.symbol.human" :doc "Human.")
(:taxonomy "escad.symbol.human.male" :doc "Human male.")
(:taxonomy "escad.symbol.human.female" :doc "Human female.")
(:taxonomy "escad.symbol.petri_net" :doc "net in petri-net.")
(:taxonomy "escad.symbol.petri_net.place" :doc "place in petri-net.")
(:taxonomy "escad.symbol.process" :doc "A process/algorithm.")
(:taxonomy "escad.symbol.state" :doc "A state.")
(:taxonomy "escad.symbol.thing" :doc "Physical (touchable) thing.")
(:taxonomy "escad.symbol.topic" :doc "Something you can talk about.")
(:taxonomy "escad.symbol.3d.piece" :doc "2D or 3D object in 3D space.")
))
