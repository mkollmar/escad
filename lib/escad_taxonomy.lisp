;; Copyright (C) 2011, 2012, 2013, 2014, 2019, 2020, 2021 Markus Kollmar
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
(:doc "Taxonomy for escad. This description is only meant for editing this taxonomy. Note that if you create or use your own, you may not able to easily interchange data.")
(:license "GNU Affero GPL")
(:version 2021may01))
(

(:taxonomy "escad.relation" :doc "Undirected universal relation (lowest fallback)." :alternative nil :opposite nil :ref-from nil :ref-to nil :type "undirected")
(:taxonomy "escad.relation.cause" :doc "Source leads to target.")
(:taxonomy "escad.relation.depends_on" :doc "Source depends on target.")
(:taxonomy "escad.relation.has_child" :doc "Person has genetic/law child.")
(:taxonomy "escad.relation.has_subtopic" :doc "Topic has a subtopic.")
(:taxonomy "escad.relation.have_met" :doc "Persons have met each other physically.")
(:taxonomy "escad.relation.is_answer" :doc "Answer of a flow-question (decission-tree).")
(:taxonomy "escad.relation.is_correct" :doc "Answer or route/path is correct/prefered.")
(:taxonomy "escad.relation.related" :doc "Universal bidirected relation.")
(:taxonomy "escad.relation.transition" :doc "Transition from one state to another.")
(:taxonomy "escad.relation.3d.placed_at" :doc "Place object directly at another object or with some offset.")

(:taxonomy "escad.symbol" :count nil :doc "Root, universal symbol (lowest fallback).")
(:taxonomy "escad.symbol.angle" :doc "Float angle (length, diameter) in degrees." :type "FLOAT")
(:taxonomy "escad.symbol.author" :doc "Name of author." :type "STRING")
(:taxonomy "escad.symbol.color" :doc "[S] Color for this symbol: red, blue, yellow, green, black(default)." :type "STRING")
(:taxonomy "escad.symbol.copyright" :doc "Copyright." :type "STRING")
(:taxonomy "escad.symbol.date" :doc "A date like 2020-10-22.")
(:taxonomy "escad.symbol.desease" :doc "Illnes or malfunction of life or plant.")
(:taxonomy "escad.symbol.dimension" :doc "Float dimension (length, diameter) in metres." :type "FLOAT")
(:taxonomy "escad.symbol.doc.begin" :doc "Documentation.")
(:taxonomy "escad.symbol.doc.image" :doc "Documentation.")
(:taxonomy "escad.symbol.doc.level0" :doc "Documentation.")
(:taxonomy "escad.symbol.doc.level1" :doc "Documentation.")
(:taxonomy "escad.symbol.doc.level2" :doc "Documentation.")
(:taxonomy "escad.symbol.event.positive_corona_test" :doc "Got currently positive corona test result.")
(:taxonomy "escad.symbol.filename_relative" :doc "Filename in relative form (e.g. \"view-0.pdf\"), that means no absolute directory specification." :type "STRING")
(:taxonomy "escad.symbol.float-list" :doc "list with floating number." :type "LIST")
(:taxonomy "escad.symbol.human" :doc "Human.")
(:taxonomy "escad.symbol.human.female" :canonical T :doc "Genetic female human." :ref-from nil :ref-to nil :opposite "escad.symbol.human.male")
(:taxonomy "escad.symbol.human.male" :doc "Genetic male human.")
(:taxonomy "escad.symbol.label" :doc "Text/tag as a (nicer) second name for the symbol which don't need to be uniq (not used to reference symbol!)." :type "STRING")
(:taxonomy "escad.symbol.location" :doc "Name or Adress of a physical place like a city.")
(:taxonomy "escad.symbol.person" :doc "Name of a person.")
(:taxonomy "escad.symbol.petri_net.net" :doc "Petri-net network.")
(:taxonomy "escad.symbol.petri_net.place" :doc "Petri-net place.")
(:taxonomy "escad.symbol.pos1,5m^2" :doc "Position in an area of 1,5m^2.")
(:taxonomy "escad.symbol.process" :doc "A process/algorithm.")
(:taxonomy "escad.symbol.state" :doc "A state.")
(:taxonomy "escad.symbol.symptoms" :doc "[S] Medical symptoms." :type "STRING")
(:taxonomy "escad.symbol.thing" :doc "Physical (touchable) thing.")
(:taxonomy "escad.symbol.time_period" :doc "A clock-time period like 13:00-14:00.")
(:taxonomy "escad.symbol.topic" :doc "Something you can talk about.")
(:taxonomy "escad.symbol.url" :doc "Link (URL) to a information source." :type "STRING")
(:taxonomy "escad.symbol.x-coord" :doc "x coordinate with unit." :type "STRING")
(:taxonomy "escad.symbol.y-coord" :doc "y coordinate with unit." :type "STRING")
(:taxonomy "escad.symbol.2d.points" :doc "2D sketch (list of points).")
(:taxonomy "escad.symbol.2d-polar-coords" :doc "Polar coords like 45@10 (means 45 degrees and length 10%) for diagramms etc.." :type "STRING")
(:taxonomy "escad.symbol.3d.generate.x3d" :doc "Generates 3D File in liberate X3D-Format (XML based, can viewed with mordern browser)." :expansion "3d_expansion.lisp" :package :de.markus-herbert-kollmar.escad.3d :function "generate_x3d" :license "GNU GPL 3")
(:taxonomy "escad.symbol.3d.cad.extrusion" :doc "2D sketch extrusion gives 3d object.")
(:taxonomy "escad.symbol.3d.cad.object" :doc "2D or 3D object in 3D space.")
(:taxonomy "escad.symbol.3d.points" :doc "3D sketch (list of points).")
(:taxonomy "escad.symbol._escad" :doc "Escad related things (settings,...).")
(:taxonomy "escad.symbol._escad.export" :doc "exports view to graphviz dot format (default)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2dot" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.dot" :doc "exports view to graphviz dot format." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2dot" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.pdf" :doc "exports view to a PDF." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2pdf" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.svg" :doc "exports view to a SVG graphic (viewable in a viewer or some internet-browsers)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2svg" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.pedigree.svg" :doc "exports view, interpreted as pedigree, to a SVG graphic (viewable in a viewer or some internet-browsers)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export-pedigree2svg" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.quiz.svg" :doc "exports view, interpreted as quiz, to a SVG graphic (viewable in a viewer or some internet-browsers)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export-quiz2svg" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.export.mindmap.svg" :doc "exports view, interpreted as mindmap, to a SVG graphic (viewable in a viewer or some internet-browsers)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export-mindmap2svg" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.flow.answer" :doc "Give input as flow-answer." :expansion "flow_expansion.lisp" :package :de.markus-herbert-kollmar.escad.flow :function "check-answer" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.flow.end" :doc "end of flow (results), activate to see results." :expansion "flow_expansion.lisp" :package :de.markus-herbert-kollmar.escad.flow :function "do-finish" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.flow.question" :doc "flow-question. Activate this to see question." :expansion "flow_expansion.lisp" :package :de.markus-herbert-kollmar.escad.flow :function "do-ask" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.flow.start" :doc "flow-start. Activate this symbol to start flow or to reset existing flow-state (delete all results)." :expansion "flow_expansion.lisp" :package :de.markus-herbert-kollmar.escad.flow :function "make-statement" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.group" :doc "groups other symbols. mathematically it is a aequivalenz-relation.")
(:taxonomy "escad.symbol._escad.import.dot" :doc "imports graphviz dot format to escad." :expansion "import_expansion.lisp" :package :de.markus-herbert-kollmar.escad.import :function "import-dot" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.generator.cause_trace" :doc "Depending of a model graph, derive additional info to given data-graph." :expansion "generator_expansion.lisp" :package :de.markus-herbert-kollmar.escad.generator :function "cause_trace" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.generator.2d-layouter" :doc "Create symbols with placing it in 2D space." :expansion "generator_expansion.lisp" :package :de.markus-herbert-kollmar.escad.generator :function "2d-layouter" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.report" :doc "General settings for report symbols.")
(:taxonomy "escad.symbol._escad.report.html" :doc "Tries to extract the basic information of view (handy for learning)." :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.report :function "report2html" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.report.pdf" :doc "TODO! (defaults to text)." :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.report :function "report2txt" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.report_corona_trace.pdf" :doc "Makes trace." :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.report :function "report_corona_trace2pdf" :license "GNU GPL 3")
(:taxonomy "escad.symbol._escad.report.txt" :doc "Tries to extract the basic information of view (handy for learning)." :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.report :function "report2txt" :license "GNU GPL 3")
(:taxonomy "escad.symbol._view" :doc "Related things to current view (name, author,...). By activation export view to format for browserclient." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2svg4browserclient" :license "GNU GPL 3")

(:taxonomy "BFO2.symbol.continuant" :doc "BFO (https://basic-formal-ontology.org/).")
(:taxonomy "BFO2.symbol.continuant.dependent" :doc "BFO (https://basic-formal-ontology.org/), e.g. attributes like roles, functions...")
(:taxonomy "BFO2.symbol.continuant.independent" :doc "BFO (https://basic-formal-ontology.org/), e.g. objects, parts of objects, collection of objects, systems...")
(:taxonomy "BFO2.symbol.occurrent" :doc "BFO (https://basic-formal-ontology.org/), e.g. processes, beginnings,...")

(:taxonomy "ICD-10-GM_V2020.symbol" :doc "Desease classification in german modification, version 2020 (see https://www.dimdi.de).")
(:taxonomy "ICD-10-GM_V2020.symbol.E65" :doc "Lokalisierte Adipositas.")
))
