; Try to use learned things for current questions, e.g. to give some possible causes of a desease. Note that this may not be a 100% sure thing! No guarantee is given! You have to visit a doctor to find a valid cause!

; clear current view:
(cls)
; generate small graph with some causes of deseases as model-graph:
(ns "Bakterien")
(ns "Entzuendungsreaktion" :taxonomy "escad.symbol.desease")
(ns "Viren")
(ns "Medikamente")
(ns "Herzmuskelentzuendung")
(ns "Strahlentherapie")
(ns "Herzschwaeche")
(ns "Muedigkeit")
(ns "Fluessigkeit in Lunge")
(ns "Herzklopfen")
(ns "Lungenentzuendung")
(ns "Koronare Herzkrankheit")
(ns "Jahrelanger Bluthochdruck")
(ns "Brustschmerzen")
(ns "Fehlerhafte Immunreaktionen")
(ns "Atemlosigkeit")

(nr nil "Bakterien" "Entzuendungsreaktion" :taxonomy "escad.relation.cause")
(nr nil "Viren" "Entzuendungsreaktion" :taxonomy "escad.relation.cause")
(nr nil "Entzuendungsreaktion" "Herzmuskelentzuendung" :taxonomy "escad.relation.cause")
(nr nil "Viren" "Herzmuskelentzuendung" :taxonomy "escad.relation.cause")
(nr nil "Medikamente" "Herzmuskelentzuendung" :taxonomy "escad.relation.cause")
(nr nil "Strahlentherapie" "Herzmuskelentzuendung" :taxonomy "escad.relation.cause")
(nr nil "Fehlerhafte Immunreaktionen" "Herzmuskelentzuendung" :taxonomy "escad.relation.cause")
(nr nil "Herzmuskelentzuendung" "Herzklopfen" :taxonomy "escad.relation.cause")
(nr nil "Herzmuskelentzuendung" "Muedigkeit" :taxonomy "escad.relation.cause")
(nr nil "Herzmuskelentzuendung" "Atemlosigkeit" :taxonomy "escad.relation.cause")
(nr nil "Herzmuskelentzuendung" "Brustschmerzen" :taxonomy "escad.relation.cause")
(nr nil "Herzmuskelentzuendung" "Herzschwaeche" :taxonomy "escad.relation.cause")
(nr nil "Lungenentzuendung" "Atemlosigkeit" :taxonomy "escad.relation.cause")
(nr nil "Herzschwaeche" "Atemlosigkeit" :taxonomy "escad.relation.cause")
(nr nil "Koronare Herzkrankheit" "Herzschwaeche" :taxonomy "escad.relation.cause")
(nr nil "Jahrelanger Bluthochdruck" "Herzschwaeche" :taxonomy "escad.relation.cause")
(nr nil "Herzschwaeche" "Fluessigkeit in Lunge" :taxonomy "escad.relation.cause")

; copy original view and do the following things in second view, to keep the original view:
(cpv)
(tv)

; first of all show what we have:
(ns "export_to_pdf" :attributes '("filename_relative" "ll1.pdf") :taxonomy "escad.symbol._escad.export.pdf")
(as "export_to_pdf")

; Prepare trace with the symptoms:
(ns "Trace!" :comment "No warranty for wrong causes!" :attributes '("symptoms" ("Muedigkeit" "Fluessigkeit in Lunge")) :taxonomy "escad.symbol._escad.generator.cause_trace")
(asa "export_to_pdf" '("filename_relative" "ll2.pdf"))
(as "export_to_pdf")

; make trace:
(as "Trace!")
(asa "export_to_pdf" '("filename_relative" "ll3.pdf"))
(as "export_to_pdf")
