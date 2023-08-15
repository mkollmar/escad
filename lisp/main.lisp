;; Copyright (C) 2022 Markus Kollmar (email: markuskollmar@onlinehome.de)
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
;;
;; -------------------------------------
;; START ESCAD IN CLI:
;;   sbcl --load main.lisp net-lisp
;;
;; START ESCAD IN EMACS SLIME-MODE:
;;   OPEN FILE main.lisp AND EVALUATE FOLLOWING:
;;     +SBCL (require 'asdf)
;;     #+SBCL (require 'sb-bsd-sockets)
;;     (push "./" asdf:*central-registry*)
;;   THEN COMPILE/LOAD THE FILE main.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

; setting path if all code not in current dir:
; (setf asdf:*central-registry*
;       (list* '*default-pathname-defaults*
;              #p"/home/BLABLA/XY"
					;              asdf:*central-registry*)) ; set which directories ASDF search for systems
#+SBCL (require 'asdf) ; before asdf-load we need the module loaded
#+SBCL (require 'sb-bsd-sockets) ; before asdf-load we need the module loaded
(push "./" asdf:*central-registry*) ; old way to tell asdf where escad.asd (system-definitions are located
;(asdf:make "escad")  ; compile and load escad
; optional for older systems?: (asdf:operate 'asdf:load-op 'escad)
(asdf:load-system "escad")

(push "/usr/share/common-lisp/systems/" asdf:*central-registry*)
(asdf:load-system "hunchentoot") ; webserver

;(load "package.lisp")
;(load "utils.lisp")
;(load "network.lisp")
;(load "escad.lisp")

(in-package :de.markus-herbert-kollmar.escad)

;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN
;;;;;;;;;;;;;;;;;;;;;;;
(init-escad)
(cond ((string= (cadr (get-cmdline-args)) "net-lisp")
       (lisp_over_network))
      ((string= (cadr (get-cmdline-args)) "gui-tk")
       (load "./lib/ltk/ltk.lisp")
       (load "./escad-gui-tk.lisp"))
      ;((string= (cadr (get-cmdline-args)) "net-json_rpc")
      ;(handler-bind ((escad-internal-error #'skip-json_rpc-request))
      ;(json-rpc_over_network)))
      )
(pprint "Welcome and thanks for using escad!  :-)")
(pprint "Please type now '(in-package :escad)' to switch to escad package for typing any escad command.")
(pprint "Type then (help) to see short info about available escad commands.")


; ACESS WITH "HTTP://LOCALHOST:4242/"
; an acceptor handles multiple http requests (chmod -R 775 www).
(defparameter *server-acceptor* (make-instance 'hunchentoot:easy-acceptor
        :port 4242
        :document-root (truename "../ui/web/")))

(hunchentoot:start *server-acceptor*)

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(hunchentoot:stop *server-acceptor*)
