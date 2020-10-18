; Create schematic to test svg-mindmap export-expansion.
(ns "Projektmanagement" :attributes '("x-coord%" 50 "y-coord%" 50) :weight 1)
(ns "Psychologie" :attributes '("x-coord%" 30 "y-coord%" 60) :weight 0.5)
(ns "Projekt" :attributes '("x-coord%" 70 "y-coord%" 60) :weight 0.5)
(ns "Beziehung" :attributes '("x-coord%" 50 "y-coord%" 70) :weight 0.7)
(ns "make_mindmap!" :taxonomy "escad.symbol._escad.export.mindmap.svg")
(nr "1" "Projektmanagement" "Projekt" :taxonomy "escad.relation.has_subtopic")
(nr "2" "Projektmanagement" "Psychologie")
(nr "3" "Projektmanagement" "Beziehung")
