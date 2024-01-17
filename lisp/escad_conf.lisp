;;;; escad configuration (common-lisp)

;;; Activate web gui:
(cs "_web" :taxonomy "std:webserver" :representation "offline" :comment "Provides functionality to start or stop the builtin webserver for the graphical user interface.")
(s "_web" :parameter "start")
(as "_web")
