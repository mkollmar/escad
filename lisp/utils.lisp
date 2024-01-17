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
;; Practical utility/helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :de.markus-herbert-kollmar.escad)

;;; USER CONFIG START
(defparameter *escad-config-file* "./escad_conf.lisp" "The path with the initial configuration file.")
(defparameter *escad-workspace-dir* "../workspace/" "This directory will be used to save and load files if no directory is specified.")
(defparameter *escad-expansion-dir* "./expansion/" "This directory will be used mostly to look for expansions and taxonomies.")
(defparameter *escad_tmp_file* "escad4567.tmp" "Temporary file used mostly for export-functions.")
(defparameter *escad-server-port* 4242 "Port number of escad web gui.")
(defparameter *escad-server-host* "127.0.0.1" "Host address string.")
;; USER CONFIG END


(defparameter *current-stream* *STANDARD-OUTPUT* "Output stream for some output commands.")
(defparameter *symbols* (make-hash-table :test 'equal) "Stores all symbols.")
(defparameter *relations* (make-hash-table :test 'equal) "Stores all relations.")
(defparameter *escad_version* "0.1.0" "Version number (according to semantic versioning) of a official released escad.")
(defparameter *escad_file_format* 0)
(defparameter *escad-max-obj-count* 999 "Maximal count of automatic generated relations or symbols.")


(defun end-escad ()
  ""
  #+allegro(excl:exit)
  #+sbcl(sb-ext:quit)
  #+clisp(ext:quit)
  #-(or allegro sbcl clisp)(error "don't know how to quit."))

(defun escad-debug-message (string)
  "Writes string to stderr and flush."
  (format *error-output* "DEBUG: ~S " string)
  (finish-output))

(defun file-data2string (path)
  "file-name-string -> string
read all contents of file into a string"
  (with-open-file (stream path)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string)))

(defun get-cmdline-args ()
  "<Get> <c>o<mdline> <arg>ument<s> in a list."
  (uiop:command-line-arguments)
  ;; (or 
  ;;  #+CLISP ext:*args*
  ;;  #+SBCL sb-ext:*posix-argv*
  ;;  #+LISPWORKS system:*line-arguments-list*
  ;;  #+CMU extensions:*command-line-words*
  ;;  nil)
  )

(defun get-date-string ()
  "-> ymd"
  (multiple-value-bind (sec min hr d m y a b c) (get-decoded-time)
    (return-from get-date-string (format nil "~a~a~a" y m d))))

(defun join-string-list (string-list)
    "Concatenates a list of strings and puts ', ' between the elements."
    (format nil "~{~A~^, ~}" string-list))

(defun key-value2json (alist)
  "Write assoc-list to object in json-format."
  (format nil "{~a}"
	  (join-string-list
	   (mapcar (lambda (cons)
		     (format nil "~a:~a" (car cons) (cdr cons)))
		   alist))))

(defun lsort (llist)
  "Sort a list of lists according to their length."
  (map 'list (function cdr)
       (sort (map 'vector (lambda (list) (cons (length list) list)) llist)
             (function <)
	     :key (function car))))


(defun skip-json_rpc-request (condition)
  (invoke-restart 'skip-json_rpc-request))

(defun system-shell (command_string &optional arg_string_list)
  "command_string [argument_string_list] -> T | nil
Executes given command_string in a system shell. If command suceeds give back T, otherwise nil."
  (or
   #+clisp (not (car (multiple-value-list (sys::shell (concatenate 'string command_string (format nil "~{~A~}" arg_string_list))))))
   #+SBCL (sb-impl::process-exit-code
     (sb-ext:run-program  
      "/bin/sh"
      (list  "-c" (concatenate 'string command_string " " (format nil "~{~a ~}" arg_string_list)))
      :input nil :output *standard-output*))
   
   ;#+SBCL (= 0 (sb-ext:process-exit-code (sb-ext:run-program command_string arg_string_list :search t :output *standard-output*)))))
   ))


(defun prompt-integer (prompt)
  "Prompt for integer, if none valid entered use 0."
  (or (parse-integer (prompt-string prompt) :junk-allowed t) 0))

(defun prompt-string (prompt)
  "Prompt for string."
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun taxonomy2pkg (taxonomy-string)
  "Takes a taxonomy-string with possible package designator from relation or symbol and extracts the package name."
  (let ((tmp (split-sequence:split-sequence #\: taxonomy-string)))
    (if (= (length tmp) 1)
	nil  ; no package designator in th taxonomy string
	(car tmp))))

(defun taxonomy2sym (taxonomy-string)
  "Takes a taxonomy-string with possible package designator from relation or symbol and extracts the symbol name."
  (car (reverse (split-sequence:split-sequence #\: taxonomy-string))))

(defun taxonomy2intern (taxonomy-string)
  "Takes a taxonomy-string with possible package designator from relation or symbol and interns symbol (e.g. ready to call make-instance with that name)."
  (let ((my-package (taxonomy2pkg taxonomy-string)))
    (if (> (length my-package) 0)
	(intern (string-upcase (taxonomy2sym taxonomy-string)) (string-upcase my-package))
	(intern (string-upcase (taxonomy2sym taxonomy-string))))))

(defun TODO ()
  (error "Sorry, this function is not implemented yet, but will work in one of the next escad versions. :-("))
