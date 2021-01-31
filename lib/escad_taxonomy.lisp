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
L -> Type <l>ist.
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
(:version 2020nov01))
(
(:taxonomy "escad.attribute.author" :doc "Name of author." :type "STRING")
(:taxonomy "escad.attribute.color" :doc "[S] Color for this symbol: red, blue, yellow, green, black(default)." :type "STRING")
(:taxonomy "escad.attribute.copyright" :doc "Copyright." :type "STRING")
(:taxonomy "escad.attribute.dimension" :doc "Float dimension (diameter, angle)." :type "FLOAT")
(:taxonomy "escad.attribute.filename_relative" :doc "Filename in relative form (e.g. \"view-0.pdf\"), that means no absolute directory specification." :type "STRING")
(:taxonomy "escad.attribute.label" :doc "Text/tag as a (nicer) second name for the symbol which don't need to be uniq (not used to reference symbol!)." :type "STRING")
(:taxonomy "escad.attribute.string-rep" :doc "[S] Explicit <rep>resentation of object as <string>. When printing the symbol this value will be used instead symbol-comment." :type "STRING")
(:taxonomy "escad.attribute.symptoms" :doc "[S] Medical symptoms." :type "STRING")
(:taxonomy "escad.attribute.url" :doc "Link (URL) to a information source." :type "STRING")
(:taxonomy "escad.attribute.2d-polar-coords" :doc "Polar coords like 45@10 (means 45 degrees and length 10%) for diagramms etc.." :type "STRING")
(:taxonomy "escad.attribute.float-list" :doc "list with floating number." :type "LIST")
(:taxonomy "escad.attribute.x-coord" :doc "[S] x coordinate with unit." :type "STRING")
(:taxonomy "escad.attribute.y-coord" :doc "[S] y coordinate with unit." :type "STRING")

(:taxonomy "escad.relation" :doc "[U] Undirected universal relation (lowest fallback).")
(:taxonomy "escad.relation.cause" :doc "[D] Source leads to target.")
(:taxonomy "escad.relation.depends_on" :doc "[D] Source depends on target.")
(:taxonomy "escad.relation.has_child" :doc "[D] Person has genetic/law child.")
(:taxonomy "escad.relation.has_subtopic" :doc "[D] Topic has a subtopic.")
(:taxonomy "escad.relation.have_met" :doc "[B] Persons have met each other physically.")
(:taxonomy "escad.relation.is_answer" :doc "[D] Answer of a flow-question (decission-tree).")
(:taxonomy "escad.relation.is_correct" :doc "[D] Answer or route/path is correct/prefered.")
(:taxonomy "escad.relation.related" :doc "[B] Universal bidirected relation.")
(:taxonomy "escad.relation.transition" :doc "[D] Transition from one state to another.")
(:taxonomy "escad.relation.3d.placed_at" :doc "[D] Place object directly at another object or with some offset.")

(:taxonomy "escad.symbol" :doc "Root, universal symbol (lowest fallback).")
(:taxonomy "escad.symbol.date" :doc "A date like 2020-10-22.")
(:taxonomy "escad.symbol.desease" :doc "Illnes or malfunction of life or plant.")
(:taxonomy "escad.symbol.doc.begin" :doc "Documentation.")
(:taxonomy "escad.symbol.doc.image" :doc "Documentation.")
(:taxonomy "escad.symbol.doc.level0" :doc "Documentation.")
(:taxonomy "escad.symbol.doc.level1" :doc "Documentation.")
(:taxonomy "escad.symbol.doc.level2" :doc "Documentation.")
(:taxonomy "escad.symbol.event.positive_corona_test" :doc "Got currently positive corona test result.")
(:taxonomy "escad.symbol.ICD-10-GM_V2020" :doc "Desease classification in german modification, version 2020 (see https://www.dimdi.de).")
(:taxonomy "escad.symbol.ICD-10-GM_V2020.E65" :doc "Lokalisierte Adipositas.")
(:taxonomy "escad.symbol.human" :doc "Human.")
(:taxonomy "escad.symbol.human.female" :doc "Genetic female human.")
(:taxonomy "escad.symbol.human.male" :doc "Genetic male human.")
(:taxonomy "escad.symbol.location" :doc "Name or Adress of a physical place like a city.")
(:taxonomy "escad.symbol.person" :doc "Name of a person.")
(:taxonomy "escad.symbol.petri_net.net" :doc "Petri-net network.")
(:taxonomy "escad.symbol.petri_net.place" :doc "Petri-net place.")
(:taxonomy "escad.symbol.pos1,5m^2" :doc "Position in an area of 1,5m^2.")
(:taxonomy "escad.symbol.process" :doc "A process/algorithm.")
(:taxonomy "escad.symbol.state" :doc "A state.")
(:taxonomy "escad.symbol.thing" :doc "Physical (touchable) thing.")
(:taxonomy "escad.symbol.time_period" :doc "A clock-time period like 13:00-14:00.")
(:taxonomy "escad.symbol.topic" :doc "Something you can talk about.")
(:taxonomy "escad.symbol.2d.points" :doc "2D sketch (list of points).")
(:taxonomy "escad.symbol.3d.generate.x3d" :doc "[E] Generates 3D File in liberate X3D-Format (XML based, can viewed with mordern browser)." :expansion "3d_expansion.lisp" :package :de.markus-herbert-kollmar.escad.3d :function "generate_x3d" :license "GNU GPL 3")
(:taxonomy "escad.symbol.3d.cad.extrusion" :doc "2D sketch extrusion gives 3d object.")
(:taxonomy "escad.symbol.3d.cad.object" :doc "2D or 3D object in 3D space.")
(:taxonomy "escad.symbol.3d.points" :doc "3D sketch (list of points).")
(:taxonomy "escad.symbol._escad" :doc "[S] Escad related things (settings,...).")
(:taxonomy "escad.symbol._escad.export" :doc "[E] exports view to graphviz dot format (default)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2dot" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.dot" :doc "[E] exports view to graphviz dot format." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2dot" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.pdf" :doc "[E] exports view to a PDF." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2pdf" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.svg" :doc "[E] exports view to a SVG graphic (viewable in a viewer or some internet-browsers)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2svg" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.pedigree.svg" :doc "[E] exports view, interpreted as pedigree, to a SVG graphic (viewable in a viewer or some internet-browsers)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export-pedigree2svg" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.quiz.svg" :doc "[E] exports view, interpreted as quiz, to a SVG graphic (viewable in a viewer or some internet-browsers)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export-quiz2svg" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.mindmap.svg" :doc "[E] exports view, interpreted as mindmap, to a SVG graphic (viewable in a viewer or some internet-browsers)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export-mindmap2svg" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.flow.answer" :doc "Give input as flow-answer." :expansion "flow_expansion.lisp" :package :de.markus-herbert-kollmar.escad.flow :function "check-answer" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.flow.end" :doc "end of flow (results), activate to see results." :expansion "flow_expansion.lisp" :package :de.markus-herbert-kollmar.escad.flow :function "do-finish" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.flow.question" :doc "[E] flow-question. Activate this to see question." :expansion "flow_expansion.lisp" :package :de.markus-herbert-kollmar.escad.flow :function "do-ask" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.flow.start" :doc "[E,S] flow-start. Activate this symbol to start flow or to reset existing flow-state (delete all results)." :expansion "flow_expansion.lisp" :package :de.markus-herbert-kollmar.escad.flow :function "make-statement" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.group" :doc "groups other symbols. mathematically it is a aequivalenz-relation.")
(:taxonomy "escad.symbol._escad.import.dot" :doc "[E] imports graphviz dot format to escad." :expansion "import_expansion.lisp" :package :de.markus-herbert-kollmar.escad.import :function "import-dot" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.generator.cause_trace" :doc "[E] Depending of a model graph, derive additional info to given data-graph." :expansion "generator_expansion.lisp" :package :de.markus-herbert-kollmar.escad.generator :function "cause_trace" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.generator.2d-layouter" :doc "[E] Create symbols with placing it in 2D space." :expansion "generator_expansion.lisp" :package :de.markus-herbert-kollmar.escad.generator :function "2d-layouter" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.report" :doc "[S,E] General settings for report symbols.")
(:taxonomy "escad.symbol._escad.report.html" :doc "[E] Tries to extract the basic information of view (handy for learning)." :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.report :function "report2html" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.report.pdf" :doc "[E] TODO! (defaults to text)." :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.report :function "report2txt" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.report_corona_trace.pdf" :doc "[E] Makes trace." :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.report :function "report_corona_trace2pdf" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.report.txt" :doc "[E] Tries to extract the basic information of view (handy for learning)." :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.report :function "report2txt" :license "GNU GPL 3")
(:taxonomy "escad.symbol._view" :doc "[S,E] Related things to current view (name, author,...). By activation export view to format for browserclient." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2svg4browserclient" :license "GNU GPL 3")
))
