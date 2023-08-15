; Make pedigree-tree for test purposes:
(cls)
(ns "Vater") (ns "Ich") (ns "Kind") (ns "Mutter")
(ns "report_expansion" :comment "generate text output of view" :taxonomy "escad.symbol._escad.report.txt")
(ns "make_PDF" :comment "generate PDF of view" :taxonomy "escad.symbol._escad.export.pdf")
(ns "export_pedigree_expansion" :attributes '("root" "Mutter") :comment "generate svg output of view interpreted as pedigree." :taxonomy "escad.symbol._escad.export.pedigree.svg")
(nr "hatKind1" "Vater" "Ich" :taxonomy "escad.relation.has_child")
(nr "hatKind2" "Ich" "Kind" :taxonomy "escad.relation.has_child")
(nr "hatKind3" "Mutter" "Ich" :taxonomy "escad.relation.has_child")
(nr "hatEnkel" "Mutter" "Kind")
(nr "hatOma" "Kind" "Mutter")

(ls)
(as "make_PDF")
