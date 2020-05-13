;; Copyright (C) 2011, 2012, 2013, 2014, 2019, 2020 Markus Kollmar
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
;;                                  :type = specifies type of attribute as FLOAT, INTEGER, STRING, STRING_LIST (for input parsing).
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
(:doc "Standard taxonomy for escad. Note that if you create or use your own, you may not able to easily interchange data.
Doc-strings of the taxonomies begin with [] field, which contained KEYS tell - if necessary, separated with comma - following things
(note the KEYS of attributes, relation or symbols can have same KEY but mean different things):
++ATTRIBUTE:
F -> Type <f>loat.
I -> Type <i>nteger.
S -> Type <s>tring.
++RELATION:
U -> Relation is treated as a <U>ndirected relation.
D -> Relation is treated as a <D>irected relation.
B -> Relation is treated as a <B>idirected relation.
++SYMBOL:
E -> <E>xpansion is using this symbol by symbol-activation with as-command to execute a function.
S -> Symbol/relation should be insert only one time (<s>ingle) in the schematic."
)
(:license "GNU Affero GPL")
(:version 2020apr03))
(
(:taxonomy "escad.attribute.author" :doc "Name of author." :type "STRING")
(:taxonomy "escad.attribute.copyright" :doc "Copyright." :type "STRING")
(:taxonomy "escad.attribute.string-rep" :doc "[S] Explicit <rep>resentation of object as <string>. When printing the symbol this value will be used instead symbol-comment." :type "STRING")
(:taxonomy "escad.attribute.label" :doc "Text as a (nicer) second name for the symbol which don't need to be uniq (not used to reference symbol!)." :type "STRING")
(:taxonomy "escad.attribute.url" :doc "Link (URL) to a information source." :type "STRING")
(:taxonomy "escad.attribute.2d-polar-coords" :doc "Polar coords like 45@10 (means 45 degrees and length 10%) for diagramms etc.." :type "STRING")
(:taxonomy "escad.attribute.x-coord" :doc "[S] x coordinate with unit." :type "STRING")
(:taxonomy "escad.attribute.y-coord" :doc "[S] y coordinate with unit." :type "STRING")

(:taxonomy "escad.relation" :doc "[U] Root, universal relation (lowest fallback).")
(:taxonomy "escad.relation.is_answer" :doc "[D] Answer of a flow-question (decission-tree).")
(:taxonomy "escad.relation.has_child" :doc "[D] Person has genetic/law child.")
(:taxonomy "escad.relation.has_subtopic" :doc "[D] Topic has a subtopic.")
(:taxonomy "escad.relation.related" :doc "[B] Universal bidirected relation.")
(:taxonomy "escad.relation.transition" :doc "[D] Transition from one state to another.")
(:taxonomy "escad.relation.3d.placed_at" :doc "[D] Place object directly at another object or with some offset.")

(:taxonomy "escad.symbol" :doc "Root, universal symbol (lowest fallback).")
(:taxonomy "escad.symbol.human" :doc "Human.")
(:taxonomy "escad.symbol.human.female" :doc "Genetic female human.")
(:taxonomy "escad.symbol.human.male" :doc "Genetic male human.")
(:taxonomy "escad.symbol.petri_net.net" :doc "Petri-net network.")
(:taxonomy "escad.symbol.petri_net.place" :doc "Petri-net place.")
(:taxonomy "escad.symbol.process" :doc "A process/algorithm.")
(:taxonomy "escad.symbol.state" :doc "A state.")
(:taxonomy "escad.symbol.thing" :doc "Physical (touchable) thing.")
(:taxonomy "escad.symbol.topic" :doc "Something you can talk about.")
(:taxonomy "escad.symbol.3d.generate.x3d" :doc "[E] Generates 3D File in liberate X3D-Format (XML based, can viewed with mordern browser)." :expansion "3d_expansion.lisp" :package :de.markus-herbert-kollmar.escad.3d :function "generate_x3d" :license "GNU GPL 3")
(:taxonomy "escad.symbol.3d.piece" :doc "2D or 3D object in 3D space.")
(:taxonomy "escad.symbol._escad" :doc "[S] Escad related things (settings,...).")
(:taxonomy "escad.symbol._escad.export.dot" :doc "[E] exports view to graphviz dot format, viewable in internetbrowser." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2dot" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.svg" :doc "[E] exports view to a SVG graphic (viewable in a viewer or some internet-browsers)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2svg" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.pedigree.svg" :doc "[E] exports view, interpreted as pedigree, to a SVG graphic (viewable in a viewer or some internet-browsers)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export-pedigree2svg" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.mindmap.svg" :doc "[E] exports view, interpreted as mindmap, to a SVG graphic (viewable in a viewer or some internet-browsers)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export-mindmap2svg" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.flow.answer" :doc "Give input as flow-answer." :expansion "flow_expansion.lisp" :package :de.markus-herbert-kollmar.escad.flow :function "check-answer" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.flow.end" :doc "end of flow (results), activate to see results." :expansion "flow_expansion.lisp" :package :de.markus-herbert-kollmar.escad.flow :function "do-finish" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.flow.question" :doc "[E] flow-question. Activate this to see question." :expansion "flow_expansion.lisp" :package :de.markus-herbert-kollmar.escad.flow :function "do-ask" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.flow.start" :doc "[E,S] flow-start. Activate this symbol to start flow or to reset existing flow-state (delete all results)." :expansion "flow_expansion.lisp" :package :de.markus-herbert-kollmar.escad.flow :function "make-statement" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.group" :doc "groups other symbols. mathematically it is a aequivalenz-relation.")
(:taxonomy "escad.symbol._escad.import.dot" :doc "[E] imports graphviz dot format to escad." :expansion "import_expansion.lisp" :package :de.markus-herbert-kollmar.escad.import :function "import-dot" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.report" :doc "[S,E] General settings for report symbols.")
(:taxonomy "escad.symbol._escad.report.html" :doc "[E] Tries to extract the basic information of view (handy for learning)." :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.report :function "report2html" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.report.txt" :doc "[E] Tries to extract the basic information of view (handy for learning)." :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.report :function "report2txt" :license "GNU GPL 3")
(:taxonomy "escad.symbol._view" :doc "[S,E] Related things to current view (name, author,...). By activation export view to format for browserclient." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2svg4browserclient" :license "GNU GPL 3")
))
