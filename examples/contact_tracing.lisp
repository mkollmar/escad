; Contact tracing in order to control diseases like the corona virus.

(ns "13oct2020" :taxonomy "escad.symbol.date")
(ns "14oct2020" :taxonomy "escad.symbol.date")
(ns "15oct2020" :taxonomy "escad.symbol.date")
(ns "16oct2020" :taxonomy "escad.symbol.date")

(ns "Place_A" :taxonomy "escad.symbol.pos1,5m^2")
(ns "Place_B" :taxonomy "escad.symbol.pos1,5m^2")

(ns "14:00-15:00" :taxonomy "escad.symbol.time_period")
(ns "15:00-16:00" :taxonomy "escad.symbol.time_period")

(ns "Person_A" :taxonomy "escad.symbol.person")
(ns "Person_B" :taxonomy "escad.symbol.person")
(ns "Person_C" :taxonomy "escad.symbol.person")
(ns "Person_D" :taxonomy "escad.symbol.person")

(ns "Corona_TEST_A" :taxonomy "escad.symbol.event.positive_corona_test")

(ns "trace contact!" :attributes '("filename_relative" "corona_trace") :taxonomy "escad.symbol._escad.report_corona_trace.pdf")

(nr nil "13oct2020" "Place_A")
(nr nil "Place_A" "14:00-15:00")
(nr nil "Place_A" "15:00-16:00")
(nr nil "14:00-15:00" "Person_A")
(nr nil "14:00-15:00" "Person_B")
(nr nil "15:00-16:00" "Person_C")
(nr nil "Person_B" "Corona_TEST_A")
(nr nil "13oct2020" "Corona_TEST_A")
(nr nil "trace contact!" "Corona_TEST_A")

(as "trace contact!")
