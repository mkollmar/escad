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
;;   sbcl --load main.lisp
;;
;; START ESCAD IN EMACS SLIME-MODE:
;;   (load "~/quicklisp/setup.lisp")
;;   OPEN FILE main.lisp AND EVALUATE FOLLOWING:
;;     +SBCL (require 'asdf)
;;     #+SBCL (require 'sb-bsd-sockets)
;;     (push "./" asdf:*central-registry*)
;;   THEN COMPILE/LOAD THE FILE main.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:load-system "escad")

(in-package :de.markus-herbert-kollmar.escad)

;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN
;;;;;;;;;;;;;;;;;;;;;;;
(init-escad)
