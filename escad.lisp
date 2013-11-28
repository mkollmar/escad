;; Copyright (C) 2011, 2012, 2013 Markus Kollmar
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

;;;;
;; TODO:
;;   - Support URI in symbol-names, relation-names and view-reference-names.
;;   - RDF export/import support via expansion.
;;   - querys based on special symbols and tied relations in escad.
;;;

(in-package "COMMON-LISP-USER")
(defpackage :de.markus-herbert-kollmar.escad
  (:use :common-lisp :system)
  (:nicknames :escad)
  (:shadow #:cos :exp)
  (:export :ad :adp :apc :as :asp :aup :cs :cls :cos :get-copyright-info :grp :gsp :help :help-command :help-tutorial :le :lea :lov :lr :ls
	   :lta :mr :ms :nr :ns :r :rr :rs :s :sc :ss :sav :tv)
  (:documentation "Expandable Symbolic Computer Aided Description."))
(in-package :de.markus-herbert-kollmar.escad)


;; USER CONFIG START
(defparameter *escad-view-dir* "./")
(defparameter *escad-attribute-file* "./lib/attribute.lisp")
(defparameter *escad-expansion-file* "./lib/expansion.lisp")
(defparameter *escad-taxonomy-file* "./lib/taxonomy.lisp")
(defparameter *escad_tmp_file* "./escad4567.tmp")
;; USER CONFIG END


(defclass obj ()
  ((comment
    :documentation "Description or notices to a object."
    :initarg :comment
    :initform "")
   (data
    :documentation "List with contents depending on taxonomy, which explains object in more detail."
    :initarg :data
    :initform '())
   (view
    :documentation "URI describing the location of the data. Escad view in memory has URI escad:1 or escad:2."
    :initarg :view
    :initform '())
   (weight
    :initarg :weight
    :documentation "Number from -100 to 100 that indicates the rated weight/importance of object."
    :initform '())))

(defclass sym (obj)
  ((taxonomy
    :initarg :taxonomy
    :initform "escad.symbol"
    :reader taxonomy)
   (attributes
    :initarg :attributes
    :documentation "List with attributes, which specify things of symbol nearer."
    :initform '()
    :reader attributes)
   (ref_to
    :initarg :ref_to
    :documentation "List with relation-names, which indicate the relations which go to another symbol (but need not to be directed!)."
    :initform '())   
   (ref_from
    :initarg :ref_from
    :documentation "List with relation-names, which indicate the relations which comes from another symbol (but need not to be directed!)."
    :initform '())))

(defclass rel (obj)
  ((taxonomy
    :initarg :taxonomy
    :initform "escad.relation"
    :reader taxonomy)
   (attributes
    :initarg :attributes
    :documentation "Association list with attributes, which specify things of relation as: (KEY VALUE)."
    :initform '()
    :reader attributes)
   (ref_to
    :initarg :ref_to
    :documentation "List with one symbol-name that indicate to which symbol the relation points.")
   (ref_from
    :initarg :ref_from
    :documentation "List with one symbol-name that indicate from which symbol the relation starts.")))

(defmethod print-object ((object obj) *current-stream*)
  (print-unreadable-object (object *current-stream* :type t)
			   (with-slots (comment data view weight) object
				       (format *current-stream*
					       "comment: ~s data: ~s view: ~s weight: ~s"
					        comment data view weight))))

(defmethod print-object ((object sym) *current-stream*)
  (print-unreadable-object (object *current-stream* :type t)
			   (with-slots (attributes comment data taxonomy ref_to ref_from view weight) object
				       (format *current-stream*
					       "attributes: ~s comment: ~s data: ~s taxonomy: ~s ref_to: ~s ref_from: ~s view: ~s weight: ~s"
					        attributes comment data taxonomy ref_to ref_from view weight))))

(defmethod print-object ((object rel) *current-stream*)
  (print-unreadable-object (object *current-stream* :type t)
			   (with-slots (attributes comment data taxonomy ref_to ref_from view weight) object
				       (format *current-stream*
					       "attributes: ~s comment: ~s data: ~s taxonomy: ~s ref_to: ~s ref_from: ~s view: ~s weight: ~s"
					       attributes comment data taxonomy ref_to ref_from view weight))))

(defparameter *current-stream* *STANDARD-OUTPUT*)
(defparameter *symbols1* (make-hash-table :test 'equal))
(defparameter *symbols2* (make-hash-table :test 'equal))
(defparameter *symbols* *symbols1*)
(defparameter *relations1* (make-hash-table :test 'equal))
(defparameter *relations2* (make-hash-table :test 'equal))
(defparameter *relations* *relations1*)
(defvar *current_symbol1* nil)
(defvar *current_symbol2* nil)
(defvar *current_symbol* *current_symbol1*)
(defparameter *escad_version* 0)
(defparameter *escad_file_format* 0)
(defparameter *attributes* nil "Whole available attribute-tree of escad for symbols and relations.")
(defparameter *taxonomy* nil "Whole available taxonomy-tree of escad for symbols and relations.")
(defparameter *expansions* nil "What expansion are currently available.")


;;;;
;; helper-functions
(defun escad-debug-message (string)
  "Writes string to stderr and flush."
  (format *error-output* "DEBUG: ~S " string)
  (finish-output)
)

(defun get-taxonomy-item (taxonomy-name)
   "taxonomy-string -> taxonomy-item-list"
  (dolist (taxonomy-item (cadr *taxonomy*))
    (if (string= taxonomy-name (getf taxonomy-item :taxonomy))
	(return-from get-taxonomy-item taxonomy-item)))
  nil)

(defun get-taxonomy-type (taxonomy-name)
  "taxonomy-string -> type-string
get the type of the taxonomy"
  (let ((item (get-taxonomy-item taxonomy-name)))
    (if item
      (getf item :data-type)
      nil)))

(defun init-escad ()
  (init-views)
  (load-taxonomy)
  (load-attributes)
  (load-expansion-info)
  (pprint "Welcome and thanks for using escad!  :-)")
  (pprint "If you are new to escad and need help:")
  (pprint "Type now '(in-package :escad)' to get into escad namespace.")
  (pprint "Then type '(help)' to get further info about your escad, and what you can do with it."))

(defun init-views ()
  "Initialize escad view."
  (ns "_escad_") (s "_escad_" :data '(:author "?") :taxonomy "escad.symbol.escad.environment") (tv)
  (ns "_escad_") (s "_escad_" :data '(:author "?") :taxonomy "escad.symbol.escad.environment") (tv)  (cs "_escad_"))

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

(defun load-attributes (&optional (attributes-filename *escad-attribute-file*))
  "Load in all available attributes information."
  (with-open-file (in attributes-filename)
		  (with-standard-io-syntax
		   (setf *attributes* (read in)))))

(defun load-expansion-info (&optional (expansion-filename *escad-expansion-file*))
  "Load in expansion information."
  (with-open-file (in expansion-filename)
		  (with-standard-io-syntax
		   (setf *expansions* (read in)))))

(defun load-expansion (&optional (expansion-filename *escad-expansion-file*))
  "Load in expansion functions."
  (with-open-file (in expansion-filename)
		  (with-standard-io-syntax
		   (setf *expansions* (read in)))))

(defun load-taxonomy (&optional (taxonomy-filename *escad-taxonomy-file*))
  "Load in taxonomy-tree information."
  (with-open-file (in taxonomy-filename)
		  (with-standard-io-syntax
		   (setf *taxonomy* (read in)))))

(defun make-test-schematic ()
  "Test commands."
  (ns "Vater") (ns "Ich") (ns "Kind") (ns "Mutter")
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
  (TODO))

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

(defun as (&optional (symbol "_escad_"))
  "[symbol-name] ->
<a>ctivate <s>ymbol in current view.
What happens depends on the taxonomy of the symbol. Many symbols print out a string as their contents.
Symbols which represent expansions will execute the configured function of the expansion."
  (let* ((taxonomy-name (getf (gsp (s symbol)) :taxonomy)) (taxonomy-type (get-taxonomy-type taxonomy-name)))
    (cond
     ((string= "STRING" taxonomy-type)  ; print out string (used by many symbols)
      (car (getf (gsp (s symbol)) :data)))
     ((string= "VALUE-LIST" taxonomy-type)  ; print value list (mostly used by group-symbols)
      (getf (gsp (s symbol)) :data))
     ((string= "KEY_VALUE-LIST" taxonomy-type)  ; print key-values (mostly used by configuration-symbols)
      (getf (gsp (s symbol)) :data))
     ((string= "CALL-LIST" taxonomy-type)  ; used to call expansions
      (apply (car (getf (gsp (s symbol)) :data)) (rest (getf (gsp (s symbol))))))
     (t "nil"))))

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

(defun aup (&optional start)
  "<a>nalyze <u>ndirected <p>ath."
  (TODO))

(defun cs (&optional (symbol "_escad_"))
  "[symbol-name ->
Set <c>urrent <s>ymbol in current view."
  (setf *current_symbol* symbol))

(defun cls ()
  "<cl>ear <s>chematic. This removes ALL symbols and relations in current view!"
  (clrhash *symbols*)
  (clrhash *relations*)
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

(defun grp (relation)
  "relation-object -> property-list
<g>et <r>elation <p>roperties as result in a property list."
  (if (eq (type-of relation) 'REL)
      (with-slots ((data1 data) (comment1 comment) (taxonomy1 taxonomy)
		   (ref_from1 ref_from) (ref_to1 ref_to)) relation
		   (list :comment comment1 :data data1 :taxonomy taxonomy1
			 :ref_from ref_from1 :ref_to ref_to1))
    nil))

(defun gsp (symbol)
  "symbol-object -> property-list
<g>et <s>ymbol <p>roperties as result in a property list."
  (if (eq (type-of symbol) 'SYM)
      (with-slots ((data1 data) (comment1 comment) (taxonomy1 taxonomy)) symbol
		  (list :comment comment1 :data data1 :taxonomy taxonomy1))
    nil))

(defmacro help ()
  "-> command-list
Print overview of escad, meaning of terms and all available commands."
(pprint
"escad allows you to create, edit and view graphs. There can be expansions for all domains you want model as graph.
Call (help-command 'Command-name) or (help) for more help.
Call (help-tutorial <step>) where step is a number starting from 0 (beginnging) to get a interactive tutorial to get a feeling of escad's basics.
Try this tutorial if you new - you will create a nice family-tree there.  :-)

COPYRIGHT on ESCAD has Markus Kollmar <markuskollmar@onlinehome.de>.
ESCAD is licensed under: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

Definitions of terms:
 * CONTEXT: The set of symbols and relations which are directly related to a symbol or relation.
 * DATA: universal usable property of a object depending on it's taxonomy.
 * EXPANSION: Concept to adapt escad for a special domain-specific purpose. It is similar like a app for your smartphone.
              Used expansions appear as symbols in your schematic too. Expansions must at least provide 2 methods:
                1. a '(help)' method which gives a help string about aim of the expansion, usage and all methods and options.
                2. a '(compatible ESCAD_VERSION_string)' method, which should signal true if expansion is compatible with given escad version.
 * OBJECT-PROPERTY: All information a OBJECT contains. It's name is no property, it is it's idendity.
 * OBJECT: SYMBOL or RELATION.
 * PATH-LIST: List with list(s) of symbol-relation-symbol-relation... names which are connected (at least a symbol must be given):
              '((Sym1 Rel2) (SymX)).
 * RELATION: object which represents/models a relationship between SYMBOLS.
 * SCHEMATIC: combinations of symbols and relations (can also describe a graph) in a view which can also contain references to other VIEWS.
 * SYMBOL: object which represents/models something (state, thing, process,...) the editor want. 
 * URI (see rfc3986): specifies what kind of physic media the data (e.g. view) is stored.
 * VIEW: referable place (server, file, device, memory...) which contains SCHEMATIC data. the two escad view's at runtime are referenced as
         escad:1 and escad:2. Schematics on http server is escad:http://www.domain.org/file, for local file escad:file://dir/file.

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
(format t "First try to type
'(ls)'.
This shows you all symbols on the schematic. You may expect none, but instead you should see one.
There is nothing wrong with this. It is a symbol which escad creates automatically. With this symbol you can configure your escad environment
for the current session. To see what the possible settings of the symbol are, type
'(as \"_escad_\")'.
This activates the symbol. You can do this with every symbol. But depending on the taxonomy of the symbol, there is different behaviour.
E.g. expansion-symbols will do their configured work if you activate them.
But most of the symbols only print some text, which is the DATA (content) of the symbol."))
    ((= ,step 1)
(format t "Now we want begin to make something usefull. We want create a small and simple family-tree. Just type following line:
'(make-tutorial-schematic)'
which in automatically executes following commands:
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
Luckily escad has a tool for you. Since escad is a commandline-tool to describe schematics, you have no direct command to do this.
But for additional purposes there are EXPANSIONS. Those tools allow you to make further things with your modeled graph.
Some expansions also allow you a easier editing and creation of your schematic. But we need a export-expansion, which
exports our family-tree to graphic.
To show all available expansions use this:
'(lea)'
It looks good, SVG is a graphic-format you can mostly view in your browser and it is also good vector-graphic quality.
To use this export-expansion you have to insert it in your current schematic. It will show up there as a new symbol - but do not
panic, it will not be shown on our graphical family-tree. So type:
'(ne \"Make-SVG-Graphic\" \"escad.export.svg\" \"activate\")'
"))
    ((= ,step 4)
(format t "Now you should first save our view (the family-tree) on the file-system of your computer with:
'(sav)'
To generate your svg-graphic just activate our expansion, whic is now nothing else than a normal escad-symbol,
you can prove by the known command:
'(ls)'
There is also a special command which only lists the expansion in the view:
'(le)'
However you know already that activation makes symbols which represent expansions, to work - so type:
'(as \"Make-SVG-Graphic\")'
This produces a svg-file in a settable directory - standard is the directory where your escad.lisp file is.
To whatch the svg file, simple turn on an new web-browser with svg-support and search the file.
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

(defun le (&optional filter)
  "<L>ist all symbols which represent <e>xpansions in current schematic."
  (ssel (ls) :taxonomy "escad.symbol.escad.expansion"))

(defun lea (&optional filter)
  "[filter-string] -> list of all, or matching the filter-string, expansion-strings
<L>ist <e>xpansions currently loaded and <a>vailable for schematic."
  (let ((expansions '()))
    (if filter
	(dolist (expansion-item (cadr *expansions*))
	  (if (search filter (getf expansion-item :expansion))
	      (push (getf expansion-item :expansion) expansions)))
      (dolist (expansion-item (cadr *expansions*))
	(push (getf expansion-item :expansion) expansions)))
    expansions))

(defun lov (file_name)
  "file-name -> T
<lo>ad <v>iew from file[18~ into memory. All current symbols and relations will be deleted!"
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
	    (make-instance 'sym :comment (cdr (assoc 'comment x)) :data (cdr (assoc 'data x))
			   :ref_from (cdr (assoc 'ref_from x)) :ref_to (cdr (assoc 'ref_to x))
			   :taxonomy (cdr (assoc 'taxonomy x)))))
    (dolist (x relations)
      (setf (gethash (cdr (assoc 'name x)) *relations*)
	    (make-instance 'sym :comment (cdr (assoc 'comment x)) :data (cdr (assoc 'data x))
			   :ref_from (cdr (assoc 'ref_from x)) :ref_to (cdr (assoc 'ref_to x))
			   :taxonomy (cdr (assoc 'taxonomy x))))))
  T)

(defun lr (&optional filter)
  "<L>ist all <r>elations in current schematic."
  (let ((rels '()))
    (maphash #'(lambda (k v) (push k rels)) *relations*)
    rels))

(defun ls (&optional filter)
  "<L>ist all <s>ymbols in current schematic."
  (let ((syms '()))
    (maphash #'(lambda (k v) (push k syms)) *symbols*)
    syms))

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

(defun ne (name expansion_name command &key comment)
  "symbol-name expansion-name command-string [comment] -> symbol-object
Create a <n>ew <e>xpansion with given name and possible additional values in schematic."
  (if (load-expansion expansion_name)
      (ns name :comment comment :data (list (read-from-string command)) :taxonomy "escad.symbol.escad.expansion")
    nil))

(defun nr (name ref_from ref_to &key comment data (taxonomy "escad.relation.directed") weight)
  "relation-name ref_from ref_to [comment data taxonomy weight] -> relation-object
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
				 (make-instance 'rel :comment comment :data data
						:ref_to (list ref_to) :ref_from (list ref_from) :taxonomy taxonomy))))))

(defun ns (name &key comment data (taxonomy "escad.symbol"))
  "symbol-name [comment data taxonomy] -> symbol-object
Create a <n>ew <s>ymbol with given name and possible additional values in schematic."
  (multiple-value-bind (sym sym-exists) (gethash name *symbols*)
		       (if sym-exists
			   nil
			 (setf (gethash name *symbols*)
			       (make-instance 'sym :comment comment :data data :taxonomy taxonomy)))))

(defun psd (name)
  "<P>rompt for guided <s>ymbol <d>ata input. TODO!"
  (let ((data '()) (tax nil))
    (if (setq tax (taxonomy (s name)))
	(progn
	  (cond
	   ((string= "EXPANSION-INTERFACE_LIST" (get-taxonomy-type tax)) (setq data (prompt-expansion-interface-list))))
	  (s name :data data))
      (error "no taxonomy"))))

(defun r (name &key comment data ref_from ref_to taxonomy)
  "relation-name [comment data ref_from ref_to taxonomy] -> nil
Sets <r>elation properties (slots) given by key - if any given - and returns relation, or 'nil' if not existent."
  (multiple-value-bind (rel rel-exists) (gethash name *relations*)
		       (if rel-exists
			 (with-slots ((data1 data) (comment1 comment) (ref_from1 ref_from) (ref_to1 ref_to)
				      (taxonomy1 taxonomy)) (gethash name *relations*)
				      (if data     (setf data1 data))
				      (if comment  (setf comment1 comment))
				      (if ref_from (push ref_from ref_from1))
				      (if ref_to   (push ref_to ref_to1))
				      (if (and taxonomy (not taxonomy1)) (setf taxonomy1 taxonomy))  ; not overwrite existing taxonomy
				      (gethash name *relations*))
			 nil)))

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

(defun s (name &key comment data taxonomy)
  "symbol-name ->
Sets <s>ymbol properties (slots) given by key - if any given - and returns symbol, or 'nil' if not existent."
  (multiple-value-bind (sym sym-exists) (gethash name *symbols*)
		       (if sym-exists
			 (with-slots ((data1 data) (comment1 comment) (taxonomy1 taxonomy)) (gethash name *symbols*)
				     (if data (setf data1 data))
				     (if comment (setf comment1 comment))
				     (if taxonomy (setf taxonomy1 taxonomy))
				     (gethash name *symbols*))
			 nil)))

(defun sav (&optional (file_name "view.escad"))
  "[file-name] ->
<sa>ve current <v>iew in a specified file in a user-dir which is defined by escad-admin."
  (let ((output (make-array 3 :fill-pointer 0)) (symbols '()) (relations '()))
    (vector-push (list (cons 'escad_version *escad_version*) (cons 'escad_file_format *escad_file_format*)) output)
    (dolist (name (ls))
      (with-slots (comment data ref_from ref_to taxonomy weight) (gethash name *symbols*)
		  (push (list (cons 'name name) (cons 'data data) (cons 'comment comment) (cons 'taxonomy taxonomy)
			      (cons 'ref_from ref_from) (cons 'ref_to ref_to) (cons 'weight weight)) symbols)))
    (vector-push symbols output)
    (dolist (name (lr))
      (with-slots (comment data ref_from ref_to taxonomy weight) (gethash name *relations*)
		  (push (list (cons 'name name) (cons 'data data) (cons 'comment comment) (cons 'taxonomy taxonomy)
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

(defun ssel (symbols &key (name nil name-p) (comment nil comment-p) (ref_from nil ref_from-p) (ref_to nil ref_to-p)
		     (taxonomy nil taxonomy-p))
  "symbol-name [name comment ref_from ref_to taxonomy] -> (symbol-names)
<s>ymbol <sel>ector: create list of symbols out from given symbol-list which match all given conditions."
  (let ((sym_set '()))
    (dolist (sname symbols)
      (with-slots ((comment_ comment) (ref_from_ ref_from) (ref_to_ ref_to) (taxonomy_ taxonomy)) (gethash sname *symbols*)
		  (if (and (if name-p     (string-equal name sname) t)
			   (if comment-p  (string-equal comment comment_) t)
			   (if ref_from-p (not (set-exclusive-or ref_from ref_from_)) t)
			   (if ref_to-p   (not (set-exclusive-or ref_to ref_to_)) t)
			   (if comment-p  (string-equal comment comment_) t)
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

;;;;;;;
; MAIN
(init-escad)