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
(:version "2021may02")
(:include "schema-org_taxonomy.lisp")
)

(

:attribute (
(:taxonomy "escad.angle" :doc "Float angle (length, diameter) in degrees." :type "FLOAT")
(:taxonomy "escad.author" :doc "Name of author." :type "STRING")
(:taxonomy "escad.color" :doc "[S] Color for this symbol: red, blue, yellow, green, black(default)." :type "STRING")
(:taxonomy "escad.copyright" :doc "Copyright." :type "STRING")
(:taxonomy "escad.dimension" :doc "Float dimension (length, diameter) in metres." :type "FLOAT")
(:taxonomy "escad.filename_relative" :doc "Filename in relative form (e.g. \"view-0.pdf\"), that means no absolute directory specification." :type "STRING")
(:taxonomy "escad.label" :doc "Text/tag as a (nicer) second name for the symbol which don't need to be uniq (not used to reference symbol!)." :type "STRING")
(:taxonomy "escad.string-rep" :doc "[S] Explicit <rep>resentation of object as <string>. When printing the symbol this value will be used instead symbol-comment." :type "STRING")
(:taxonomy "escad.symptoms" :doc "[S] Medical symptoms." :type "STRING")
(:taxonomy "escad.url" :doc "Link (URL) to a information source." :type "STRING")
(:taxonomy "escad.2d-polar-coords" :doc "Polar coords like 45@10 (means 45 degrees and length 10%) for diagramms etc.." :type "STRING")
(:taxonomy "escad.float-list" :doc "list with floating number." :type "LIST")
(:taxonomy "escad.x-coord" :doc "[S] x coordinate with unit." :type "STRING")
(:taxonomy "escad.y-coord" :doc "[S] y coordinate with unit." :type "STRING")
)

:relation (
(:taxonomy "escad" :doc "Undirected universal relation (lowest fallback)." :alternative nil :opposite nil :ref-from nil :ref-to nil :type "undirected")
(:taxonomy "escad.cause" :doc "Source leads to target.")
(:taxonomy "escad.depends_on" :doc "Source depends on target.")
(:taxonomy "escad.has_child" :doc "Person has genetic/law child.")
(:taxonomy "escad.has_subtopic" :doc "Topic has a subtopic.")
(:taxonomy "escad.have_met" :doc "Persons have met each other physically.")
(:taxonomy "escad.is_answer" :doc "Answer of a flow-question (decission-tree).")
(:taxonomy "escad.is_correct" :doc "Answer or route/path is correct/prefered.")
(:taxonomy "escad.related" :doc "Universal bidirected relation.")
(:taxonomy "escad.transition" :doc "Transition from one state to another.")
(:taxonomy "escad.3d.placed_at" :doc "Place source-object directly at another target-object or with some offset.")
)

:symbol (
(:taxonomy "escad" :count nil :doc "Root, universal symbol (lowest fallback).")
(:taxonomy "escad.angle" :doc "Float angle (length, diameter) in degrees." :type "FLOAT")
(:taxonomy "escad.author" :doc "Name of author." :type "STRING")
(:taxonomy "escad.color" :doc "[S] Color for this symbol: red, blue, yellow, green, black(default)." :type "STRING")
(:taxonomy "escad.copyright" :doc "Copyright." :type "STRING")
(:taxonomy "escad.date" :doc "A date like 2020-10-22.")
(:taxonomy "escad.desease" :doc "Illnes or malfunction of life or plant.")
(:taxonomy "escad.dimension" :doc "Float dimension (length, diameter) in metres." :type "FLOAT")
(:taxonomy "escad.doc.begin" :doc "Documentation.")
(:taxonomy "escad.doc.image" :doc "Documentation.")
(:taxonomy "escad.doc.level0" :doc "Documentation.")
(:taxonomy "escad.doc.level1" :doc "Documentation.")
(:taxonomy "escad.doc.level2" :doc "Documentation.")
(:taxonomy "escad.encoding" :doc "Interpretation of bits to charset like UTF-8.")
(:taxonomy "escad.event.positive_corona_test" :doc "Got currently positive corona test result.")
(:taxonomy "escad.filename_relative" :doc "Filename in relative form (e.g. \"view-0.pdf\"), that means no absolute directory specification." :type "STRING")
(:taxonomy "escad.float-list" :doc "list with floating number." :type "LIST")
(:taxonomy "escad.human" :doc "Human.")
(:taxonomy "escad.human.female" :canonical T :doc "Genetic female human." :ref-from nil :ref-to nil :opposite "escad.symbol.human.male")
(:taxonomy "escad.human.male" :doc "Genetic male human.")
(:taxonomy "escad.label" :doc "Text/tag as a (nicer) second name for the symbol which don't need to be uniq (not used to reference symbol!)." :type "STRING")
(:taxonomy "escad.location" :doc "Name or Adress of a physical place like a city.")
(:taxonomy "escad.person" :doc "Name of a person.")
(:taxonomy "escad.petri_net.net" :doc "Petri-net network.")
(:taxonomy "escad.petri_net.place" :doc "Petri-net place.")
(:taxonomy "escad.pos1,5m^2" :doc "Position in an area of 1,5m^2.")
(:taxonomy "escad.process" :doc "A process/algorithm.")
(:taxonomy "escad.state" :doc "A state.")
(:taxonomy "escad.symptoms" :doc "[S] Medical symptoms." :type "STRING")
(:taxonomy "escad.thing" :doc "Physical (touchable) thing.")
(:taxonomy "escad.time_period" :doc "A clock-time period like 13:00-14:00.")
(:taxonomy "escad.topic" :doc "Something you can talk about.")
(:taxonomy "escad.url" :doc "Link (URL) to a information source." :type "STRING")
(:taxonomy "escad.x-coord" :doc "x coordinate with unit." :type "STRING")
(:taxonomy "escad.y-coord" :doc "y coordinate with unit." :type "STRING")
(:taxonomy "escad.2d.points" :doc "2D sketch (list of points).")
(:taxonomy "escad.2d-polar-coords" :doc "Polar coords like 45@10 (means 45 degrees and length 10%) for diagramms etc.." :type "STRING")
(:taxonomy "escad.3d.generate.x3d" :doc "Generates 3D File in liberate X3D-Format (XML based, can viewed with mordern browser)." :expansion "3d_expansion.lisp" :package :de.markus-herbert-kollmar.escad.3d :function "generate_x3d" :license "GNU GPL 3")
(:taxonomy "escad.3d.cad.extrusion" :doc "2D sketch extrusion gives 3d object.")
(:taxonomy "escad.3d.cad.object" :doc "2D or 3D object in 3D space.")
(:taxonomy "escad.3d.points" :doc "3D sketch (list of points).")
(:taxonomy "escad.escad" :doc "Escad related things (settings,...).")
(:taxonomy "escad.export" :doc "exports view to graphviz dot format (default)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2dot" :license "GNU GPL 3")
(:taxonomy "escad.export.dot" :doc "exports view to graphviz dot format." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2dot" :license "GNU GPL 3")
(:taxonomy "escad.export.pdf" :doc "exports view to a PDF." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2pdf" :license "GNU GPL 3")
(:taxonomy "escad.export.svg" :doc "exports view to a SVG graphic (viewable in a viewer or some internet-browsers)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2svg" :license "GNU GPL 3")
(:taxonomy "escad.export.pedigree.svg" :doc "exports view, interpreted as pedigree, to a SVG graphic (viewable in a viewer or some internet-browsers)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export-pedigree2svg" :license "GNU GPL 3")
(:taxonomy "escad.export.quiz.svg" :doc "exports view, interpreted as quiz, to a SVG graphic (viewable in a viewer or some internet-browsers)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export-quiz2svg" :license "GNU GPL 3")
(:taxonomy "escad.export.mindmap.svg" :doc "exports view, interpreted as mindmap, to a SVG graphic (viewable in a viewer or some internet-browsers)." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export-mindmap2svg" :license "GNU GPL 3")
(:taxonomy "escad.escad.flow.answer" :doc "Give input as flow-answer." :expansion "flow_expansion.lisp" :package :de.markus-herbert-kollmar.escad.flow :function "check-answer" :license "GNU GPL 3")
(:taxonomy "escad.escad.flow.end" :doc "end of flow (results), activate to see results." :expansion "flow_expansion.lisp" :package :de.markus-herbert-kollmar.escad.flow :function "do-finish" :license "GNU GPL 3")
(:taxonomy "escad.escad.flow.question" :doc "flow-question. Activate this to see question." :expansion "flow_expansion.lisp" :package :de.markus-herbert-kollmar.escad.flow :function "do-ask" :license "GNU GPL 3")
(:taxonomy "escad.escad.flow.start" :doc "flow-start. Activate this symbol to start flow or to reset existing flow-state (delete all results)." :expansion "flow_expansion.lisp" :package :de.markus-herbert-kollmar.escad.flow :function "make-statement" :license "GNU GPL 3")
(:taxonomy "escad.escad.group" :doc "groups other symbols. mathematically it is a aequivalenz-relation.")
(:taxonomy "escad.escad.import.dot" :doc "imports graphviz dot format to escad." :expansion "import_expansion.lisp" :package :de.markus-herbert-kollmar.escad.import :function "import-dot" :license "GNU GPL 3")
(:taxonomy "escad.escad.generator.cause_trace" :doc "Depending of a model graph, derive additional info to given data-graph." :expansion "generator_expansion.lisp" :package :de.markus-herbert-kollmar.escad.generator :function "cause_trace" :license "GNU GPL 3")
(:taxonomy "escad.escad.generator.2d-layouter" :doc "Create symbols with placing it in 2D space." :expansion "generator_expansion.lisp" :package :de.markus-herbert-kollmar.escad.generator :function "2d-layouter" :license "GNU GPL 3")
(:taxonomy "escad.escad.report" :doc "General settings for report symbols.")
(:taxonomy "escad.report.html" :doc "Tries to extract the basic information of view (handy for learning)." :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.report :function "report2html" :license "GNU GPL 3")
(:taxonomy "escad.report.pdf" :doc "TODO! (defaults to text)." :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.report :function "report2txt" :license "GNU GPL 3")
(:taxonomy "escad.report_corona_trace.pdf" :doc "Makes trace." :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.report :function "report_corona_trace2pdf" :license "GNU GPL 3")
(:taxonomy "escad.report.txt" :doc "Tries to extract the basic information of view (handy for learning)." :expansion "report_expansion.lisp" :package :de.markus-herbert-kollmar.escad.report :function "report2txt" :license "GNU GPL 3")
(:taxonomy "escad.view" :doc "Related things to current view (name, author,...). By activation export view to format for browserclient." :expansion "export_expansion.lisp" :package :de.markus-herbert-kollmar.escad.export :function "export2svg4browserclient" :license "GNU GPL 3")

(:taxonomy "BFO2.continuant" :doc "BFO (https://basic-formal-ontology.org/).")
(:taxonomy "BFO2.continuant.dependent" :doc "BFO (https://basic-formal-ontology.org/), e.g. attributes like roles, functions...")
(:taxonomy "BFO2.continuant.independent" :doc "BFO (https://basic-formal-ontology.org/), e.g. objects, parts of objects, collection of objects, systems...")
(:taxonomy "BFO2.occurrent" :doc "BFO (https://basic-formal-ontology.org/), e.g. processes, beginnings,...")

(:taxonomy "ICD-10-GM_V2020" :doc "Desease classification in german modification, version 2020 (see https://www.dimdi.de).")
(:taxonomy "ICD-10-GM_V2020.E65" :doc "Lokalisierte Adipositas.")
)

)
)
