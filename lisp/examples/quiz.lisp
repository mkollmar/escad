; Create schematic to test svg-quiz export-expansion.
(ns "Symbollayoutgenerator" :attributes '("x-coord%" 50 "y-coord%" 50
					  "2d-layout" '(4
"Ich"   "bin"      "müde"     "."
""      "werde"    "Hunger"   "!"
""      "habe"     ""         "")) :taxonomy "escad.symbol._escad.generator.2d-layouter")
(as "Symbollayoutgenerator")
(rs "Symbollayoutgenerator")

(with-taxonomy
(nr "1" "Projektmanagement" "Projekt" :taxonomy "escad.relation.has_subtopic")
(nr "2" "Projektmanagement" "Psychologie")
(nr "3" "Projektmanagement" "Beziehung")
