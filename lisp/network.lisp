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
;; SOCKET functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :de.markus-herbert-kollmar.escad)

;#+CLISP (defparameter *escad-server-host* "127.0.0.1" "Host address in CLISP-format")
;#+SBCL  (defparameter *escad-server-host* #(127 0 0 1) "Host address in SBCL-format")
;(defparameter *escad-server-port* 3000 "Port number (5000)")

;;;; Hunchentoot server for escad web-gui
;; ACESS WITH "HTTP://LOCALHOST:4242/"
;; an acceptor handles multiple http requests (chmod -R 775 www).
(asdf:load-system "hunchentoot") ; webserver
(defparameter *server-acceptor* (make-instance 'hunchentoot:easy-acceptor
        :port 4242
        :document-root (truename "../web/")))

(defun start-gui-server ()
  "Start hunchentoot web server for escad gui."
  (hunchentoot:start *server-acceptor*)
  (hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
    (setf (hunchentoot:content-type*) "text/plain")
    (com.inuoe.jzon:parse "{\"hey\": 3}")
    (format nil "Hey~@[ ~A~]!" name)))

(defun stop-gui-server ()
  "Stop hunchentoot web server for escad gui."
  (hunchentoot:stop *server-acceptor*))


;;;; Lisp over socket (old code, currently not used)
(defun lisp_over_network ()
  "Process single client requests forever. If one client exits connection, open another waiting. Connection is persistent."
   #+CLISP (let ((server (socket:socket-server *escad-server-port* :interface *escad-server-host*)))
    (format t "~&Waiting for a lisp-code connection on ~S:~D~%"
	    (socket:socket-server-host server) (socket:socket-server-port server))
    (unwind-protect
      ;; infinite loop, terminate with Control+C
      (loop (with-open-stream (socket (socket:socket-accept server))
	(multiple-value-bind (local-host local-port) (socket:socket-stream-local socket)
			     (multiple-value-bind (remote-host remote-port)
						  (socket:socket-stream-peer socket)
						  (format T "~&Connection: ~S:~D -- ~S:~D~%"
							  remote-host remote-port local-host
							  local-port)))
	;; loop is terminated when the remote host closes the connection or on EXT:EXIT
	(loop (when (eq :eof (socket:socket-status (cons socket :input))) (return))
	   (print (eval (read socket)) socket)
	   ;; flush everything left in socket
	   (loop :for c = (read-char-no-hang socket nil nil) :while c)
	   (terpri socket))))
      ;; make sure server is closed
      (socket:socket-server-close server)))
   
   #+SBCL (let ((server (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
        ; Set socket options
        (setf (sb-bsd-sockets:sockopt-reuse-address server) t)
        (setf (sb-bsd-sockets:non-blocking-mode server) t)

        ; Bind to an address
        (sb-bsd-sockets:socket-bind server *escad-server-host* *escad-server-port*)
        (sb-bsd-sockets:socket-listen server 1)
        (loop
            (let ((connection (sb-bsd-sockets:socket-accept server)))
	      (sleep 1) ; without sleep high cpu usage!?
	      (when connection
		(handle-connection connection)))
	   )
	))

#+SBCL (defun read-from-connection (connection)
     "Read data from a connection."
     (multiple-value-bind (buffer length) (sb-bsd-sockets:socket-receive connection nil 1024)
         (let (data)
             (if (> length 0)
                 (subseq buffer 0 length)
                 data))))

#+SBCL (defun handle-connection (connection)
	 "Handle an incoming connection."
	 (loop (unless (sb-bsd-sockets:socket-open-p connection) (return))
	    (let ((data (read-from-connection connection)))
	      (sleep 1) ; without sleep high cpu usage!?
	      (if data
		  (sb-bsd-sockets:socket-send connection (write-to-string (eval (read-from-string data))) nil)
		  (sb-bsd-sockets:socket-close connection))
       	      )))

#+SBCL (defun handle-multiple-connection (connection)
	 "Handle incoming connection(s)."
    (sb-thread:make-thread
        (lambda ()
            ; Handle our connection in this lambda, which is run in a new thread
            (loop
                (let ((data (read-from-connection connection)))
                    ; Do something with the data received
                    ))
            (sb-bsd-sockets:socket-close connection)
	    )))

