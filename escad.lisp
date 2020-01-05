;; Copyright (C) 2011, 2012, 2013, 2014, 2019 Markus Kollmar (email: markuskollmar@onlinehome.de)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "COMMON-LISP-USER")
(defpackage :de.markus-herbert-kollmar.escad
  (:use :common-lisp :system)
  (:nicknames :escad)
  (:shadow #:cos :exp)
  (:export :attributes :comment :taxonomy :rel :ref_from :ref_to
	   :ad :adp :apc :ara :as :asa :asp :aup :cs :cls :cos :get-copyright-info :gra :grp :gsa :gsp :help :help-command :help-tutorial
	   :le :los :lov :lr :ls :lta :mr :ms :nr :ns :r :rp :rr :rra :rs :rsa :s :sc :sp :ss :sav :tv
	   :call-expansion-function :file-data2string
	   :*escad-lib-dir* :*escad-view-dir* :*escad_tmp_file*)
  (:documentation "Expandable Symbolic Computer Aided Description."))
(in-package :de.markus-herbert-kollmar.escad)


;; USER CONFIG START
(defparameter *escad-view-dir* "./public/" "This directory will be used to save and load view's if no directory is specified.")
(defparameter *escad-lib-dir* "./lib/" "This directory will be used to look for expansions and the standard escad-taxonomy.")
(defparameter *escad-external-lib-dir* "./external_helpers/" "This directory will be used to look for code written from others.")
(defparameter *escad-server-host* "127.0.0.1" "Host name")
(defparameter *escad-server-port* 3000 "Port number (5000)")
(defparameter *escad-taxonomy-file* "./lib/escad_taxonomy.lisp" "Actual valid taxonomy-tree for whole insertable symbols and relations.")
(defparameter *escad_tmp_file* "./escad4567.tmp" "Temporary file used mostly for export-functions.")
;; USER CONFIG END


;; Load external code
(load (concatenate 'string *escad-external-lib-dir* "ST-JSON-master/st-json.lisp"))
(use-package :st-json)
;; End loading external code


(defclass obj ()
  ((attributes
    :initarg :attributes
    :documentation "Association list like '((KEY . VALUE) (KEY2 . VALUE2)) with attributes, which specify things of object nearer in a KEY-VALUE way. Some attributes control data-slot."
    :initform '()
    :reader attributes)
   (comment
    :documentation "Description or notices to a object."
    :initarg :comment
    :initform "")
   (taxonomy
    :documentation "Classificates the object into a defined taxonomy-scheme to indicate basic meaning of the object."
    :initarg :taxonomy
    :reader taxonomy)
   (weight
    :initarg :weight
    :documentation "Number from -1 to 1 that indicates the rated weight/importance of object. 1 means the object is 100% that weight, whereby -1 means the opposite. 0 is neutral."
    :initform '())))

(defclass sym (obj)
  ((ref_to
    :initarg :ref_to
    :documentation "List with relation-names, which indicate the relations which go to another symbol (but need not to be directed!)."
    :initform '())   
   (ref_from
    :initarg :ref_from
    :documentation "List with relation-names, which indicate the relations which comes from another symbol (but need not to be directed!)."
    :initform '())))

(defclass rel (obj)
  ((ref_to
    :initarg :ref_to
    :documentation "List with one symbol-name that indicate to which symbol the relation points.")
   (ref_from
    :initarg :ref_from
    :documentation "List with one symbol-name that indicate from which symbol the relation starts.")))

(define-condition escad-internal-error (error)
  ((error-text
    :initarg :error-text
    :reader error-text)))

(defmethod print-object ((object obj) *current-stream*)
  (print-unreadable-object (object *current-stream* :type t)
			   (with-slots (comment weight) object
				       (format *current-stream*
					       "comment: ~s weight: ~s"
					        comment weight))))

(defmethod print-object ((object sym) *current-stream*)
  (print-unreadable-object (object *current-stream* :type t)
			   (with-slots (attributes comment taxonomy ref_to ref_from weight) object
				       (format *current-stream*
					       "attributes: ~s comment: ~s taxonomy: ~s ref_to: ~s ref_from: ~s weight: ~s"
					        attributes comment taxonomy ref_to ref_from weight))))

(defmethod print-object ((object rel) *current-stream*)
  (print-unreadable-object (object *current-stream* :type t)
			   (with-slots (attributes comment taxonomy ref_to ref_from view weight) object
				       (format *current-stream*
					       "attributes: ~s comment: ~s taxonomy: ~s ref_to: ~s ref_from: ~s weight: ~s"
					       attributes comment taxonomy ref_to ref_from weight))))

(defparameter *current-stream* *STANDARD-OUTPUT*)
(defparameter *symbols1* (make-hash-table :test 'equal) "Symbols for first view.")
(defparameter *symbols2* (make-hash-table :test 'equal) "Symbols for second view.")
(defparameter *symbols* *symbols1* "Pointer to current symbol-hash.")
(defparameter *relations1* (make-hash-table :test 'equal))
(defparameter *relations2* (make-hash-table :test 'equal))
(defparameter *relations* *relations1* "Pointer to current relation-hash.")
(defvar *current_symbol1* nil)
(defvar *current_symbol2* nil)
(defvar *current_symbol* *current_symbol1*)
(defparameter *escad_version* 0)
(defparameter *escad_file_format* 0)
(defparameter *taxonomy* nil "Whole available taxonomy-tree of escad for symbols, relations and attributes.")


;;;;
;; helper-functions
(defun call-expansion-function (taxonomy-name expansion-function &optional arguments)
  "taxonomy-name-string expansion-function-name-string -> return-value-from-expansion_function
call expansion function."
  (load (concatenate 'string *escad-lib-dir* (get-taxo-prop taxonomy-name :expansion)))
  (use-package (get-taxo-prop taxonomy-name :package))
  (funcall (read-from-string (get-taxo-prop taxonomy-name :function)) (list arguments)))

(defun escad-debug-message (string)
  "Writes string to stderr and flush."
  (format *error-output* "DEBUG: ~S " string)
  (finish-output)
)

(defun file-data2string (path)
  "file-name-string -> string
read all contents of file into a string"
  (with-open-file (stream path)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string)))

(defun get-taxonomy-item (taxonomy-name)
   "taxonomy-string -> taxonomy-item-list"
  (dolist (taxonomy-item (cadr *taxonomy*))
    (if (string= taxonomy-name (getf taxonomy-item :taxonomy))
	(return-from get-taxonomy-item taxonomy-item)))
  nil)

(defun get-taxo-prop (taxonomy-name property)
  "taxonomy-string :property-key -> type-string
<get> <taxo>nomy <prop>erty of the taxonomy."
  (let ((item (get-taxonomy-item taxonomy-name)))
    (if item
      (getf item property)
      nil)))

(defun skip-json_rpc-request (condittion)
  (invoke-restart 'skip-json_rpc-request))

(defun init-escad ()
  (init-views)
  (load-taxonomy)
  (cond ((string= (car ext::*args*) "net-lisp") (lisp_over_network))
	((string= (car ext::*args*) "net-json_rpc") (handler-bind ((escad-internal-error #'skip-json_rpc-request))
								  (json-rpc_over_network))))
  (pprint "Welcome and thanks for using escad!  :-)")
  (pprint "If you are new to escad and need help:")
  (pprint "Type now '(in-package :escad)' to get into escad namespace.")
  (pprint "Then type '(help)' to get further info about your escad, and what you can do with it."))

(defun init-views ()
  "Initialize escad view."
  (ns "_escad" :attributes '((url . "testurl")) :comment "default symbol" :weight 0 :taxonomy "escad.symbol._escad") (ns "_view") (s "_view" :taxonomy "escad.symbol._thisView") (tv)
  (ns "_escad" :attributes '((url . "testurl")) :comment "default symbol" :weight 0 :taxonomy "escad.symbol._escad") (ns "_view") (s "_view" :taxonomy "escad.symbol._thisView")
  (cs "_escad"))

(defun join-string-list (string-list)
    "Concatenates a list of strings and puts ', ' between the elements."
    (format nil "~{~A~^, ~}" string-list))

(defun key-value2json (alist)
  "Write assoc-list to object in json-format."
  (format nil "{~a}" (join-string-list (mapcar (lambda (cons)
						 (format nil "~a:~a" (car cons) (cdr cons)))
					       alist))))

(defun lsort (llist)
  "Sort a list of lists according to their length."
  (map 'list (function cdr)
       (sort (map 'vector (lambda (list) (cons (length list) list)) llist)
             (function <)
	     :key (function car))))

(defun load-taxonomy (&optional (taxonomy-filename *escad-taxonomy-file*))
  "Load in taxonomy-tree information."
  (with-open-file (in taxonomy-filename)
		  (with-standard-io-syntax
		   (setf *taxonomy* (read in)))))

(defun make-test-schematic ()
  "Test commands."
  (ns "Vater") (ns "Ich") (ns "Kind") (ns "Mutter")
  (ns "report-expansion" :taxonomy "escad.symbol._escad.report.txt")
  (nr "hatKind1" "Vater" "Ich")
  (nr "hatKind2" "Ich" "Kind")
  (nr "hatKind3" "Mutter" "Ich")
  (nr "hatEnkel" "Mutter" "Kind")
  (nr "hatOma" "Kind" "Mutter"))

(defun make-test-schematic2 ()
  "Test commands."
  (ns "Vater") (ns "Ich")
  (nr "hatKind1" "Vater" "Ich"))

(defun make-tutorial-schematic ()
  "Deletes current view and creates schematic for internal tutorial."
  (cls)
  (ns "mother") (ns "father") (ns "myself")
  (nr "has_child[1]" "mother" "myself") (nr "has_child[2]" "father" "myself") (nr "are_married" "father" "mother") (nr "hatKind1" "Vater" "Ich"))

(defun path (path-list &optional (max-depth 10))
  "path-list [depth] -> list of flat path(s)
List all directed <path>s outgoing from given path-list with last retrieved (newest) symbol first.
Makes a list of flat path ((A B C A) (C B)) - no nested structures. Optional give a maximum iteration depth."
  (let ((start-sym '()) (result '()))
    (dolist (given-path path-list)  ; process all given paths
      (when (and (not result) (> (length given-path) 1))   ; if given path ends, save it and give it back again
	(setq result (cons given-path result)))
      (setq start-sym (car given-path))
	(with-slots ((ref_to-rels ref_to)) (gethash start-sym *symbols*)  ; get all outgoing relations
	  (when ref_to-rels
	    (dolist (ref_name ref_to-rels)  ; process all relations
	      (when ref_name
		(with-slots ((ref-to-list ref_to)) (gethash ref_name *relations*)  ; get all outgoing symbols
		(when ref-to-list
		  (dolist (symbol ref-to-list)
		    (if (and (> (- max-depth 1) 0)
			     (not (member symbol given-path :test #'string=)))  ; test if it is possible set new relation and symbol
		      (multiple-value-bind (sym sym-exists) (gethash symbol *symbols*)
			(when sym-exists
			  (setq result (nconc (path (list (nconc (list symbol ref_name) given-path)) (- max-depth 1))
					      result))))
		      (setq result (cons (nconc (list symbol ref_name) given-path)
					 result)))))))))))
    result))

(defun prompt-integer (prompt)
  "Prompt for integer, if none valid entered use 0."
  (or (parse-integer (prompt-string prompt) :junk-allowed t) 0))

(defun prompt-string (prompt)
  "Prompt for string."
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun TODO ()
  (error "Sorry, this function is not implemented yet, but will work in one of the next escad versions. :-("))


;;;;
;; USER-COMMANDS:
(defun ad ()
  "-> diameter longest-path
<a>nalyze <d>iameter (length of longest path) of current view."
  (let ((path-list '()) (longest-path '()))
    (dolist (symbol (ls))
      (setq path-list (cons (list symbol) path-list)))
    (setq longest-path (car (reverse (lsort (path path-list)))))
    (values (list-length longest-path) longest-path)))

(defun adp (&optional start)
  "[start_symbol_name] -> list with paths
<a>nalyze all <d>irected <p>aths from every symbol or from the given start symbol."
  (path '(())))

(defun aap ()
  "-> path-list
<a>nalyze <a>ll <p>ath's of current view."
  (let ((path-list '()))
    (dolist (symbol (ls))
      (setq path-list (cons (list symbol) path-list)))
    (path path-list)))

(defun apc (path symbol-list)
  "path symbol-name-list -> t/nil
<A>nalyze whether <p>ath <c>ontains all the given symbols."
  (dolist (symbol symbol-list)
    (if (not (member symbol path :test #'string=))
	(return-from apc nil)))
  t)

(defun ara (symbol-name attribute-taxonomy value)
  "relation-name ->
<A>dd <r>elation <a>ttributes depending of key."
  (multiple-value-bind (rel rel-exists) (gethash relation-name *relations*)
		       (if rel-exists
			 (with-slots ((a attributes)) (gethash relation-name *relations*)
				     (setf a (remove attribute-taxonomy a :key #'car :test #'string=))  ; remove some possibly existing key
				     (setf a (acons attribute-taxonomy value a))
				     a))
		       nil))

(defun as (&optional (symbol "_escad"))
  "[symbol-name] ->
<a>ctivate <s>ymbol in current view.
What happens depends on the taxonomy of the symbol. Many symbols print out a string as their contents.
Symbols which represent expansions will execute the configured function of the expansion."
  (let* ((taxonomy-name (getf (gsp (s symbol)) :taxonomy))
	 (expansion-file (get-taxo-prop taxonomy-name :expansion))
	 (expansion-package (get-taxo-prop taxonomy-name :package))
	 (taxonomy-documentation (get-taxo-prop taxonomy-name :doc))
	 (expansion-function (get-taxo-prop taxonomy-name :function)))
    (cond
     (expansion-file  ; this seems to be expansion
      (load (concatenate 'string *escad-lib-dir* expansion-file))
      (use-package expansion-package)
      (funcall (read-from-string expansion-function) (list symbol))
      (pprint taxonomy-documentation))
     (t (pprint taxonomy-documentation)))))

(defun asa (symbol-name attribute-taxonomy value)
  "symbol-name attrib-taxonomy attrib-val ->
<A>dd <s>ymbol <a>ttributes depending of key."
  (multiple-value-bind (sym sym-exists) (gethash symbol-name *symbols*)
		       (if sym-exists
			 (with-slots ((a attributes)) (gethash symbol-name *symbols*)
				     (setf a (remove attribute-taxonomy a :key #'car :test #'string=))  ; remove some possibly existing key
				     (setf a (acons attribute-taxonomy value a))
				     a))
		       nil))

(defun asp (symbol1 symbol2)
  "symbol-name1 symbol-name2 -> path-list
<a>nalyze <s>hortest <p>ath between 2 given symbols."
  (let ((path-list (path (list (list symbol1)))) (symbols (list symbol1 symbol2)) (result '()) (shortest-length nil) (shortest '()))
    (dolist (path path-list)
      (if (apc path symbols)
	  (setq result (cons path result))))
    (setq shortest-length (list-length (car (lsort result))))
    (dolist (path result)
      (if (= (list-length path) shortest-length)  ; collect all, if there are more shortest path's
	  (setq shortest (cons path shortest))))
    shortest))

(defun cs (&optional (symbol "_escad_"))
  "[symbol-name ->
Set <c>urrent <s>ymbol in current view."
  (setf *current_symbol* symbol))

(defun cls ()
  "<cl>ear <s>chematic. This removes ALL symbols and relations in current view!"
  (clrhash *symbols*) (clrhash *relations*) (init-views)
  t)

(defun cos (symbols)
 "symbol-name-list -> symbol-name-list
<co>py given <s>ymbol(s) with their relations to the other view. This overwrites some existing objects with same name!"
  (let ((syms_src  (if (eq *symbols* *symbols1*) *symbols1* *symbols2*))
	(syms_dest (if to-other-view-p (if (eq *symbols* *symbols1*) *symbols2* *symbols1*) syms_src))
	(rels_src  (if (eq *relations* *relations1*) *relations1* *relations2*))
	(rels_dest (if to-other-view-p (if (eq *relations* *relations1*) *relations2* *relations1*) rels_src)))
    (dolist (name symbols)
      (multiple-value-bind (sym sym-exists-p) (gethash name syms_dest)
			   (if sym-exists-p
			       (print "Overwrite symbol!"))
			   (setf (gethash name syms_dest) sym)))
    (dolist (name relations)
      (multiple-value-bind (rel rel-exists-p) (gethash name rels_dest)
			   (if rel-exists-p
			       (print "Overwrite relation!"))
			   (setf (gethash name rels_dest) rel))))
  symbols)

(defun get-copyright-info ()
  ""
(pprint "What i am allowed to do?
------------------------
COPYRIGHT on ESCAD has Markus Kollmar <markuskollmar@onlinehome.de>.
ESCAD is licensed under: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.
If you want a other license (like for commercial purposes), please contact Markus Kollmar <markuskollmar@onlinehome.de>."))

(defgeneric gobj (obj))

(defmethod gobj ((symbol sym))
  "symbol-object -> property-list
<G>et all data of symbol <o>bjekt."
  (with-slots ((attributes1 attributes) (comment1 comment) (taxonomy1 taxonomy)
	       (ref_to1 ref_to) (ref_from1 ref_from) (weight1 weight)) symbol
	       (list :attributes attributes1 :comment comment1 :ref_to ref_to1 :ref_from ref_from1 :taxonomy taxonomy1 :weight weight1)))

(defun gsdump ()
  "-> list of data-lists without keys just values in following order: name attributes comment taxonomy ref_to ref_from weight.
<G>et all data of <s>ymbols in current schematic <dump>ed."
  (let ((result '()))
    (dolist (symbol-name (ls))
      (push (with-slots ((attributes1 attributes) (comment1 comment) (taxonomy1 taxonomy)
			 (ref_to1 ref_to) (ref_from1 ref_from) (weight1 weight)) (gethash symbol-name *symbols*)
			 (list symbol-name attributes1 comment1 taxonomy1 ref_to1 ref_from1 weight1))
	    result))
    result))

(defun gra (relation-name attribute-taxonomy)
  "relation-name attriute-key ->
<G>et <r>elation <a>ttributes depending of given attribute-key."
  (multiple-value-bind (rel rel-exists) (gethash relation-name *relations*)
		       (if rel-exists
			 (with-slots ((a attributes)) (gethash relation-name *relations*)
				     (cdr (assoc attribute-taxonomy a :test #'string=)))
			 nil)))

(defun grp (relation)
  "relation-object -> property-list
<g>et <r>elation <p>roperties as result in a property list."
  (if (eq (type-of relation) 'REL)
      (with-slots ((comment1 comment) (taxonomy1 taxonomy)
		   (ref_from1 ref_from) (ref_to1 ref_to)) relation
		   (list :comment comment1 :taxonomy taxonomy1
			 :ref_from ref_from1 :ref_to ref_to1))
    nil))

(defun gsa (symbol-name attribute-key)
  "symbol-name attribute-key ->
<G>et <s>ymbol <a>ttributes depending of given attribute-key."
  (multiple-value-bind (sym sym-exists) (gethash symbol-name *symbols*)
		       (if sym-exists
			 (with-slots ((a attributes)) (gethash symbol-name *symbols*)
				     (cdr (assoc attribute-taxonomy a :test #'string=)))
			 nil)))

(defun gsp (symbol)
  "symbol-object -> property-list
<g>et <s>ymbol <p>roperties as result in a property list."
  (if (eq (type-of symbol) 'SYM)
      (with-slots ((comment1 comment) (taxonomy1 taxonomy)) symbol
		  (list :comment comment1 :taxonomy taxonomy1))
    nil))

(defmacro help ()
  "-> command-list
Print overview of escad, meaning of terms and all available commands."
(pprint
"escad allows you to create, edit, analyze and view graphs. There can be expansions for all domains you want model as graph.
Call (help-command 'Command-name) or (help) for more help.
Call (help-tutorial <step>) where step is a number starting from 0 (beginnging) to get a interactive tutorial to get a feeling of escad's basics.
Try this tutorial if you new.  :-)

COPYRIGHT on ESCAD has Markus Kollmar <markuskollmar@onlinehome.de>.
ESCAD is licensed under: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

Definitions of terms:
 * ATTRIBUTES: universal usable property of a object additional to the standard attributes (each symbol may contain different or none).
 * CONTEXT: The set of symbols and relations which are directly related to a symbol or relation.
 * EXPANSION: Concept to adapt escad for a special domain-specific purpose. It is similar like a app for your smartphone.
 * OBJECT-NAME: Is no property, it is it's idendity-string.
 * OBJECT: SYMBOL or RELATION.
 * PATH-LIST: List with list(s) of symbol-relation-symbol-relation... names which are connected (at least a symbol must be given):
              '((Sym1 Rel2) (SymX)).
 * RELATION: object which represents/models a relationship between SYMBOLS.
 * SCHEMATIC: combinations of symbols and relations (can also describe a graph) in a view which can also contain references to other VIEWS.
 * SYMBOL: object which represents/models something (state, thing, process,...) the editor want. 
 * URI (see rfc3986): specifies what kind of physic media the data (e.g. view) is stored.
 * VIEW: referable place (server, file, device, memory...) which contains SCHEMATIC data. the two escad view's at runtime are referenced as
         escad:1 and escad:2. Schematics on http server is escad:http://www.domain.org/file, for local file escad:file://dir/file.

Commands grouped depending on function:

 * SHOW   attributes: gra, gsa
 * EDIT   attributes: ara, asa, r, s
 * REMOVE attributes: rra, rsa
 * SHOW      context: lr, ls, sc, ssel
 * EDIT      context: as, cs
 * INSERT    objects: nr, ns
 * REMOVE    objects: rr, rs
 * SHOW      objects: r, s, rp, sp
 * EDIT      objects: r, s
 * RENAME    objects: mr, ms
 * SHOW   properties: grp, gsp
 * SHOW     taxonomy: lta
 * LOAD         view: los, lov
 * SAVE         view: sav
 * CHANGE       view: tv
 * CLEAR        view: cls
 * ANALYSE      view: aap, ad, adp, apc, asp
Following commands (collected in a list) are currently available:")
`(let ((lst ()))
  (do-external-symbols (s (find-package :escad)) (push s lst))
  (sort lst (lambda (x y) (string< (symbol-name x) (symbol-name y))))))

(defmacro help-command (&optional name)
  "Prints help for given command (macro/function)."
  `(documentation ,name 'function)
)

(defmacro help-tutorial (&optional (step 0))
  "[tutorial-page-number] ->
Print a defined part of a tutorial which covers all basic usage of escad."
  `(cond
    ((= ,step 0)
(format t "Hello, welcome to the escad tutorial. Hope you have fun! :-) Let's start immediately! First try to type
'(ls)'.
This shows you all symbols on the schematic. You may expect none, but instead you should see one.
There is nothing wrong with this. It is a symbol which escad creates automatically. With this symbol you can configure your escad environment
for the current session. It also show command-history, status- and error-messages of escad.
To see more about this symbol, type
'(as \"_escad\")'.
This 'activates' the symbol. You can do this with every symbol. But depending on the taxonomy of the symbol, there is different behaviour.
E.g. expansion-symbols will do their configured work if you activate them.
But most of the symbols print some text, which is the meaningfull (content) of the symbol."))
    ((= ,step 1)
(format t "Now we want begin to make something usefull. We want create a small and simple family-tree. Just type following line:
'(make-tutorial-schematic)'
which automatically executes following commands:
'(ns \"mother\")'
'(ns \"father\")'
'(ns \"myself\")'
'(nr \"has_child[1]\" \"mother\" \"myself\")'
'(nr \"has_child[2]\" \"father\" \"myself\")'
'(nr \"are_married\" \"father\" \"mother\")'
That is all for the moment. We have created our little family-tree:
3 Symbols which represents mother, father and ourself.
3 Relations describes that we are the child of mother and father, and that our parents are married.
However escad has many more things so that you can model things more correctly and in detail. But for now we are satisfied with it.
"))
    ((= ,step 2)
(format t "After creating this schematic, we want get a feeling, how it looks like. escad provides many tools to do that. First of all you can
show all relations and symbols with:
'(lr)'
'(ls)'
To analyze and print all path's use:
'(aap)'
A path in escad is a list which is filled interchanged with symbol and with related relation.
To get a feeling how big a schematic is, there is a value called DIAMETER. It is the amount of longest path:
'(ad)'
This should print you 5, that means the biggest amount of combined relations and symbols in our view is 5.
To get the shortest path between 2 symbols use:
'(asp \"mother\" \"father\")'
"))
    ((= ,step 3)
(format t "This may all be nice, but we want also a printing of our family-tree. Some things are better seen in a graphical way.
Luckily escad has a tool for you. If you are curious, you probably will search for such a command. But i must admit there is none!
So how can we do such things? You always may have heard about expansions. Exactly that is our solution.
Since escad is a tool to describe schematics, you have no direct command to do this. Instead you insert another symbol, which represents
a 'exporter' tool. Those symbol you can 'activate' with 'as'-command again. Then it invokes the expansion which the symbols represents
and exports your schematic. Easy - isn't it?
This is true for all expansions! This is the basic princip you work in escad. Nothing much more than inserting symbols and relations
and setting those.
Of course there need first to exist a expansion programmed for your need. Currently there are not much expansions (of course escad works
also without expansions). But escad grows and you can programm (or let programm)
expansions for your need, since escad programm-code is free!

To show all available exporting-related symbol-taxonomies, use this:
'(lta \"escad.symbol._escad.export\")'
It looks good, SVG is a graphic-format you can mostly view in your browser and it is also good vector-graphic quality.
To use this export-symbol you have to insert it in your current schematic. It will show up there as a new symbol - but do not
panic, it will not be shown on our graphical family-tree. So type:
'(ns \"Make-SVG-Graphic\" : taxonomy \"_escad.export.svg\")'
"))
    ((= ,step 4)
(format t "Now you should first save our view (this family-tree) on the file-system of your computer with:
'(sav)'
To generate your svg-graphic just activate our expansion-related symbol with
'(as \"Make-SVG-Graphic\")'
This produces a svg-file in a settable directory - standard is the directory where your escad.lisp file is.
To whatch the svg file, simple turn on an actual web-browser with svg-support and search the file.
"))
    ((= ,step 5)
(format t "Now you got the very basic idea of escad. Try to explore it by yourself. The online-documentation will perhaps help you:
'(help)'
prints you a overview and the explanation of some escad-terms and also all available commands.
You get a help to every escad command by:
'(help-command 'COMMAND_NAME)'
Have fun with escad and stay tuned, it will get more improvements in the future.
If you have some suggestions, just write to the author of escad.
Thanks. :-)
"))
    (t (princ "Type (help-tutorial N) where N is a number beginning with 0, to describe which tutorial step you want see."))))

(defun los (file_name)
  "file-name ->
<lo>ad <s>cript and work at current schematic."
  (load file_name))

(defun lov (file_name)
  "file-name -> T
<lo>ad <v>iew from file into memory. All current symbols and relations will be deleted!"
  (let ((input '()) header symbols relations)
    (with-open-file (in (concatenate 'string *escad-view-dir* file_name))
		    (with-standard-io-syntax
		     (setf input (read in))))
    (setf header (elt input 0))
    (setf symbols (elt input 1))
    (setf relations (elt input 2))
    (if (= (cdr (assoc 'escad_version header)) 0)
      (unless (= (cdr (assoc 'escad_file_format header)) 0)
	(error "Wrong file format!"))
      (error "The escad version, that wrotes the file, has no load-support in this escad version!"))
    (clrhash *symbols*)
    (clrhash *relations*)
    (dolist (x symbols)
      (setf (gethash (cdr (assoc 'name x)) *symbols*)
	    (make-instance 'sym :comment (cdr (assoc 'comment x)) :attributes (cdr (assoc 'attributes x))
			   :ref_from (cdr (assoc 'ref_from x)) :ref_to (cdr (assoc 'ref_to x))
			   :taxonomy (cdr (assoc 'taxonomy x)))))
    (dolist (x relations)
      (setf (gethash (cdr (assoc 'name x)) *relations*)
	    (make-instance 'sym :comment (cdr (assoc 'comment x)) :attributes (cdr (assoc 'attributes x))
			   :ref_from (cdr (assoc 'ref_from x)) :ref_to (cdr (assoc 'ref_to x))
			   :taxonomy (cdr (assoc 'taxonomy x))))))
  T)

(defun lr (&optional filter)
  "<L>ist all <r>elations in current schematic."
  (let ((rels '()))
    (maphash #'(lambda (k v) (push k rels)) *relations*)
    rels))

(defun ls (&key (exclude-taxonomy nil exclude-p))
  "[exclude-taxonomy_string-list -> symbol-list
<L>ist all <s>ymbols in current schematic."
  (let ((all-syms '()) (return-syms '()) (exclude-syms '()))
    (maphash #'(lambda (k v) (push k all-syms)) *symbols*)
    (if exclude-p
	(set-difference all-syms (loop for taxonomy in exclude-taxonomy
				       append (ssel all-syms :taxonomy taxonomy)) :test #'string=)  ; return only symbols not in exclude list
      all-syms)  ; return all symbols if no exclude clause
    ))

(defun lta (&optional filter)
  "[filter-string] -> list of all, or matching the filter-string, taxonomy-strings
<L>ist <t>axonomys currently loaded and <a>vailable for schematic."
  (let ((taxonomys '()))
    (if filter
	(dolist (taxonomy-item (cadr *taxonomy*))
	  (if (search filter (getf taxonomy-item :taxonomy))
	      (push (getf taxonomy-item :taxonomy) taxonomys)))
      (dolist (taxonomy-item (cadr *taxonomy*))
	(push (getf taxonomy-item :taxonomy) taxonomys)))
    taxonomys))

(defun mr (name new_name)
  "relation-name -> relation-name
<m>ove <r>elation - which means to change it's full-name, which can thus also change it's view)."
  (setf (gethash new_name *relations*) (gethash name *relations*))
  (remhash name *relations*)
  new_name)

(defun ms (name new_name)
  "name new-name -> symbol-object
<m>ove <s>ymbol - which means to change it's full-name, which can thus also change it's view)."
  (setf (gethash new_name *symbols*) (gethash name *symbols*))
  (remhash name *symbols*)
  (gethash new_name *symbols*))

(defun nr (name ref_from ref_to &key attributes comment (taxonomy "escad.relation") weight)
  "relation-name ref_from ref_to [comment taxonomy weight] -> relation-object
Create <n>ew directed (default) <r>elation with given name and possible additional values in schematic.
To make this relation undirected or bidirected, set the correct taxonomy."
  (multiple-value-bind (rel rel-exists) (gethash name *relations*)
		       (if rel-exists
			   nil
			 (progn
			   (with-slots ((ref_from1 ref_from)) (gethash ref_to *symbols*)
				       (pushnew name ref_from1))
			   (with-slots ((ref_to2 ref_to)) (gethash ref_from *symbols*)
				       (pushnew name ref_to2))
			   (setf (gethash name *relations*)
				 (make-instance 'rel :attributes attributes :comment comment
						:ref_to (list ref_to) :ref_from (list ref_from) :taxonomy taxonomy))))))

(defun ns (name &key attributes comment (taxonomy "escad.symbol") weight)
  "symbol-name [attributes comment taxonomy weight] -> symbol-object
Create a <n>ew <s>ymbol with given name and possible additional values in schematic."
  (multiple-value-bind (sym sym-exists) (gethash name *symbols*)
		       (if sym-exists
			   nil
			 (setf (gethash name *symbols*)
			       (make-instance 'sym :attributes attributes :comment comment :taxonomy taxonomy)))))

(defun r (name &key attributes comment ref_from ref_to taxonomy)
  "relation-name [attributes comment ref_from ref_to taxonomy] -> nil
Sets <r>elation properties (slots) given by key - if any given - and returns relation, or 'nil' if not existent."
  (multiple-value-bind (rel rel-exists) (gethash name *relations*)
		       (if rel-exists
			 (with-slots ((comment1 comment) (ref_from1 ref_from) (ref_to1 ref_to)
				      (taxonomy1 taxonomy)) (gethash name *relations*)
				      (if comment  (setf comment1 comment))
				      (if ref_from (push ref_from ref_from1))
				      (if ref_to   (push ref_to ref_to1))
				      (if (and taxonomy (not taxonomy1)) (setf taxonomy1 taxonomy))  ; not overwrite existing taxonomy
				      (gethash name *relations*))
			 nil)))

(defun rp (relation-name property-name)
  "relation-name property-name -> property-string
Get <r>elation <p>roperty as result."
  (slot-value (r relation-name) property-name))

(defun rr (name)
  "relation-name -> relation-name
<R>emove <r>elation."
  (with-slots ((rel-ref_from ref_from) (rel-ref_to ref_to)) (gethash name *relations*)
	      (dolist (x rel-ref_from)
		(with-slots ((sym-ref_to ref_to)) (gethash x *symbols*)
			    (setf sym-ref_to (remove name sym-ref_to :count 1))))
	      (dolist (x rel-ref_to)
		(with-slots ((sym-ref_from ref_from)) (gethash x *symbols*)
			    (setf sym-ref_from (remove name sym-ref_from :count 1)))))
  (remhash name *relations*)
  name)

(defun rra (relation-name attribute-taxonomy)
  "relation-name ->
<R>emove <r>elation <a>ttributes depending of key."
  (multiple-value-bind (rel rel-exists) (gethash relation-name *relations*)
		       (if rel-exists
			 (with-slots ((a attributes)) (gethash relation-name *relations*)
				     (setf a (remove attribute-taxonomy a :key #'car :test #'string=))
				     a))
		       nil))

(defun rs (name)
  "symbol-name -> symbol-name
<R>emove <s>ymbol."
  (with-slots ((sym-ref_from ref_from) (sym-ref_to ref_to)) (gethash name *symbols*)
	      (dolist (x sym-ref_from)
		(rr x))
	      (dolist (y sym-ref_to)
		(rr y)))
  (remhash name *symbols*)
  name)

(defun rsa (symbol-name attribute-taxonomy)
  "symbol-name ->
<R>emove <s>ymbol <a>ttributes depending of key."
  (multiple-value-bind (sym sym-exists) (gethash symbol-name *symbols*)
		       (if sym-exists
			 (with-slots ((a attributes)) (gethash symbol-name *symbols*)
				     (setf a (remove attribute-taxonomy a :key #'car :test #'string=))
				     a))
		       nil))

(defun s (name &key attributes comment taxonomy)
  "symbol-name ->
Sets <s>ymbol attributes, comment or taxonomy depending of key - if any given - and returns symbol, or 'nil' if not existent."
  (multiple-value-bind (sym sym-exists) (gethash name *symbols*)
		       (if sym-exists
			 (with-slots ((attributes1 attributes) (comment1 comment) (taxonomy1 taxonomy)) (gethash name *symbols*)
				     (if attributes (setf attributes1 attributes))
				     (if comment (setf comment1 comment))
				     (if taxonomy (setf taxonomy1 taxonomy))
				     (gethash name *symbols*))
			 nil)))

(defun sav (&optional (file_name "view.escad"))
  "[file-name] ->
<sa>ve current <v>iew in a specified file in a user-dir (which is predefined by escad-admin)."
  (let ((output (make-array 3 :fill-pointer 0)) (symbols '()) (relations '()))
    (vector-push (list (cons 'escad_version *escad_version*) (cons 'escad_file_format *escad_file_format*)) output)
    (dolist (name (ls))
      (with-slots (attributes comment ref_from ref_to taxonomy weight) (gethash name *symbols*)
		  (push (list (cons 'attributes attributes) (cons 'name name) (cons 'comment comment) (cons 'taxonomy taxonomy)
			      (cons 'ref_from ref_from) (cons 'ref_to ref_to) (cons 'weight weight)) symbols)))
    (vector-push symbols output)
    (dolist (name (lr))
      (with-slots (attributes comment ref_from ref_to taxonomy weight) (gethash name *relations*)
		  (push (list (cons 'attributes attributes) (cons 'name name) (cons 'comment comment) (cons 'taxonomy taxonomy)
			      (cons 'ref_from ref_from) (cons 'ref_to ref_to) (cons 'weight weight)) relations)))
    (vector-push relations output)
    (with-open-file (out (concatenate 'string *escad-view-dir* file_name)
			 :direction :output
			 :if-exists :supersede)
		    (with-standard-io-syntax
		     (print output out)))))

(defun sc (&optional (name *current_symbol*))
  "[symbol-name] -> ()
List <c>ontext of the current or a given <s>ymbol (default is current)."
  (let ((in-relsym '()) (out-relsym '()))
    (with-slots (ref_from ref_to) (gethash name *symbols*)
		(dolist (in-rel ref_from)
		  (with-slots (ref_from ref_to) (gethash in-rel *relations*)
				  (loop for i from 0 to (- (length ref_to) 1) do
					(if (eq ref_to name)
					    (push (cons in-rel ref_from) in-relsym)))))
		
		(dolist (out-rel ref_to)
		  (with-slots (ref_from ref_to) (gethash out-rel *relations*)
			      (loop for i from 0 to (- (length ref_from) 1) do
				    (if (eq ref_from name)
					(push (cons out-rel ref_to) out-relsym))))))
    ; ((("rel_in_1" . "sym1")) (("rel_out" . "sym2") ("rel2_out" . "ssym"))):
    (list in-relsym out-relsym)))

(defun sp (symbol-name property-name)
  "symbol-name-string property-name-symbol -> property-string
get <s>ymbol <p>roperty as result."
  (slot-value (s symbol-name) property-name))

(defun ssel (symbols &key (name nil name-p) (comment nil comment-p) (ref_from nil ref_from-p) (ref_to nil ref_to-p)
		     (taxonomy nil taxonomy-p))
  "symbol-names-list [name comment ref_from ref_to taxonomy] -> (symbol-names)
<s>ymbol <sel>ector: create list of symbols out from given symbol-list which match all given conditions."
  (let ((sym_set '()))
    (dolist (sname symbols)
      (with-slots ((comment_ comment) (ref_from_ ref_from) (ref_to_ ref_to) (taxonomy_ taxonomy)) (gethash sname *symbols*)
		  (if (and (if name-p     (string-equal name sname) t)
			   (if comment-p  (string-equal comment comment_) t)
			   (if ref_from-p (not (set-exclusive-or ref_from ref_from_)) t)
			   (if ref_to-p   (not (set-exclusive-or ref_to ref_to_)) t)
			   (if taxonomy-p (string-equal taxonomy taxonomy_) t))
		      (push sname sym_set))))
    sym_set))

(defun tv ()
  "Escad has two views, each representing a own schematic, so <t>oggle from one <v>iew to the second."
  (if (eq *symbols* *symbols1*)
      (progn
	(setf *symbols* *symbols2*)
	(setf *relations* *relations2*)
	(setf *current_symbol* *current_symbol2*)
	0)
      (progn
	(setf *symbols* *symbols1*)
	(setf *relations* *relations1*)
	(setf *current_symbol* *current_symbol1*)
	1)))

;;;;;;;;;;;;;;;;;;;;;
;; Network connection
(defun lisp_over_network ()
  "Process client requests forever. If one client exits connection, open another waiting. Connection is persistent."
  (let ((server (socket:socket-server *escad-server-port* :interface *escad-server-host*)))
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
      (socket:socket-server-close server))))

;;;;;;;
; MAIN
(init-escad)
