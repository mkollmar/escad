; CAD in three dimensions. Draw a christmas tree in three dimensions.

(ns "Stamm-Form" :taxonomy "escad.symbol.2d.points" :attributes '("escad.attribute.float-list" '(5 0  0 5  -5 0  0 -5)))
(ns "Ast-unten-Form" :taxonomy "escad.symbol.2d.points" :attributes '("escad.attribute.float-list" '(5 0  0 5  -5 0  0 -5)))
(ns "Ast-mitte-Form" :taxonomy "escad.symbol.2d.points" :attributes '("escad.attribute.float-list" '(5 0  0 5  -5 0  0 -5)))
(ns "Ast-oben-Form" :taxonomy "escad.symbol.2d.points" :attributes '("escad.attribute.float-list" '(5 0  0 5  -5 0  0 -5)))
(ns "Ast-Mitte-Form" :taxonomy "escad.symbol.2d.points" :attributes '("escad.attribute.float-list" '(5 0  0 5  -5 0  0 -5)))

(ns "Stamm" :taxonomy "escad.symbol.3d.cad.extrusion" :attributes '("escad.attribute.dimension" 95  "escad.attribute.color" "Tan"))
(ns "Stamm" :taxonomy "escad.symbol.2d.points" :attributes '("filename_relative" "corona_trace"))

(ns "Christmas-tree" :taxonomy "escad.symbol.3d.cad.object" :attributes '("filename_relative" "christmas_tree"))

(nr nil "extrude0" "Stamm-Form" :taxonomy "escad.relation.depends_on")



(nr nil "extrude0" "14:00-15:00")



(nr nil "extrude0" "14:00-15:00")



(as "Christmas-tree")
