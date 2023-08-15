; Create schematic to test flow-expansion.
(ns "Start flow!" :comment "To start this mathematic test flow activate this symbol." :attributes '("escad.attribute.string-rep" "Start of a mathematic test." "flow-input-type" :counting) :taxonomy "escad.symbol._escad.flow.start")
(ns "Q1" :comment "activate to get question and possible answers" :attributes '("escad.attribute.string-rep" "5 + 6 * 7 = ?") :taxonomy "escad.symbol._escad.flow.question")
(ns "A1" :comment "activate to select that answer as correct" :attributes '("escad.attribute.string-rep" "47" "flow-counter" :correct) :taxonomy "escad.symbol._escad.flow.answer")
(ns "A2" :comment "activate to select that answer as correct" :attributes '("escad.attribute.string-rep" "77" "flow-counter" :wrong) :taxonomy "escad.symbol._escad.flow.answer")
(ns "Result" :comment "activate to get result of mathematic test" :taxonomy "escad.symbol._escad.flow.end")
(nr nil "Start flow!" "Q1")
(nr nil "Q1" "A1")
(nr nil "Q1" "A2")
(nr nil "A1" "Result")
(nr nil "A2" "Result")
(cs "Start flow!")