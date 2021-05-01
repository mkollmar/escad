;;;; Example to generate PDF from a generated latex document with variables.
;;;; Editor: Markus Kollmar
;;;; Date  : 01may2021

;; Make trigger:
(ns "Make_PDF" :taxonomy "escad.doc.pdf")

;; Set some administrative things:
(ns "Author" :taxonomy "escad.person" :representation "Markus")
(ns "Initial_release" :taxonomy "escad.date" :representation "2021 may 1")
(ns "Init_Tex_Doc" :taxonomy "escad.doc.tex_article" :comment "creates tex-project sceleton designed for latexmk")
(ns "tex_file" :taxonomy "escad.filename_relative" :representation "test.tex")
(ns "pdf_file" :taxonomy "escad.filename_relative" :representation "test.pdf")
(nr "1" :taxonomy "escad" :ref_from "tex_file" :ref_to "Init_Tex_Doc" :comment "Tell the tex-project the filename for main-file.")

;; Set the document-content:
(ns "1" :taxonomy "escad.doc.part" :representation "Dies ist eine Überschrift")
(ns "1.1" :taxonomy "escad.doc.paragraph" :comment "This is a (text-) container.")
(ns "t1" :taxonomy "escad.doc.text" :representation "Siehe die Abbildung")
(ns "r1" :taxonomy "escad.doc.ref" :representation "fig:test")
(ns "t2" :taxonomy "escad.doc.text" :representation " die alles zeigt. Der Autor ist ")
(ns "t3" :taxonomy "escad.doc.text" :representation ".")
(ns "f1" :taxonomy "escad.doc.tikzfigure" :representation "\draw [gray] (0,0) to [out=50,in=195] (5,3); \draw [red] (5,3) to [out=-120,in=165] (8,8); \node[circle, fill=orange!30] (test) at (2,2) {++T1++};")
(ns "l1" :taxonomy "escad.doc.label" :representation "fig:test")
(ns "c1" :taxonomy "escad.doc.caption" :representation "Figure shows details of the process.")

;; Activate trigger (to generate tex and then PDF):
(as "Init_Tex_Doc")
(as "Make_PDF")
