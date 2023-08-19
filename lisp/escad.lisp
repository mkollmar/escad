;; Copyright (C) 2011, 2012, 2013, 2014, 2019, 2020,
;; 2021, 2022 Markus Kollmar (email: markuskollmar@onlinehome.de)
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
;; escad base library.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :de.markus-herbert-kollmar.escad)

;; USER CONFIG START
(defparameter *escad-view-dir* "./public/view/" "This directory will be used to save and load view's if no directory is specified.")
(defparameter *escad-lib-dir* "./lib/" "This directory will be used mostly to look for expansions and taxonomies.")
(defparameter *escad-external-lib-dir* "./lib/" "This directory will be used to look for code which is not maintained in this project.")
(defparameter *escad-taxonomy-file* "./lib/escad_taxonomy.lisp" "Actual valid escad taxonomy-tree for insertable symbols and relations.")
(defparameter *escad_tmp_file* "escad4567.tmp" "Temporary file used mostly for export-functions.")
;; USER CONFIG END


(defparameter *current-stream* *STANDARD-OUTPUT* "Output stream for some output commands.")
(defparameter *symbols1* (make-hash-table :test 'equal) "Symbols for first view.")
(defparameter *symbols2* (make-hash-table :test 'equal) "Symbols for second view.")
(defparameter *symbols* *symbols1* "Pointer to current symbol-hash.")
(defparameter *relations1* (make-hash-table :test 'equal))
(defparameter *relations2* (make-hash-table :test 'equal))
(defparameter *relations* *relations1* "Pointer to current relation-hash.")
(defvar *current_symbol1* "_view" "Current marked symbol (e.g. as a result of flow-view).")
(defvar *current_symbol2* "_view" "Current marked symbol (e.g. as a result of flow-view).")
(defvar *current_symbol* *current_symbol1* "Pointer to current marked symbol.")
(defparameter *escad_version* 0)
(defparameter *escad_file_format* 0)
(defparameter *taxonomy* nil "Currently used taxonomy-tree for symbols, relations and attributes.")
(defparameter *escad-max-obj-count* 999 "Maximal count of automatic generated relations or symbols.")

;;;;;;;;;;;;;;;;;;;;;;;;; data structures/object-definitions
;;;; relation+symbol
(defclass obj ()
  ((attributes
    :initarg :attributes
    :documentation "A data-structure with detailed info of a object. Usually this attributes are slower/expensive to get."
    :initform '()
    :reader attributes)
   (comment
    :documentation "Description or notices to a object."
    :initarg :comment
    :initform "")
   (label
    :documentation "String with a text given to the symbol. Note that this text need not to be unique and could occur more than once for different symbols. It is meant for pretty printing for some user interfaces."
    :initarg :label
    :initform "")
   (representation
    :initarg :representation
    :documentation "String which represents the object. Often used for dynamic text in expansion."
    :initform "")
   (taxonomy
    :documentation "Classificates the object into a defined taxonomy-scheme to indicate basic meaning of the object."
    :initarg :taxonomy
    :reader taxonomy)
   (weight
    :initarg :weight
    :documentation "Positive or negative float-number that rates the object aginst other objects. Null means neutral."
    :initform '())))

(defgeneric o (obj)
  (:documentation "object -> property-list
Creates a list of all <o>bject (symbol or relation) data with references (in/out)."))

(defmethod print-object ((object obj) *current-stream*)
  (print-unreadable-object (object *current-stream* :type t)
    (with-slots (representation comment weight) object
      (format *current-stream* "representation: ~s comment: ~s weight: ~s" representation comment weight))))


;;;; symbol
(defclass sym (obj)
  ((ref_from
    :initarg :ref_from
    :documentation "List with relation-names, which indicate the relations which comes from another symbol (but need not to be directed!)."
    :initform '())
   (ref_to
    :initarg :ref_to
    :documentation "List with relation-names, which indicate the relations which go to another symbol (but need not to be directed!)."
    :initform '())))

(defmethod o ((obj sym))
  (with-slots ((att attributes) (com comment) (lab label) (rep representation) (tax taxonomy)
	       (refto ref_to) (reffrom ref_from) (wei weight)) obj
    (list :attributes att :comment com :label lab :representation rep :ref_to refto :ref_from reffrom :taxonomy tax :weight wei)))

(defmethod print-object ((object sym) *current-stream*)
  (print-unreadable-object (object *current-stream* :type t)
    (with-slots (attributes comment label representation taxonomy ref_to ref_from weight) object
      (format *current-stream* "~s: ~s" label representation))))


;; relation
(defclass rel (obj)
  ((ref_from
    :initarg :ref_from
    :documentation "List with one symbol-name that indicate from which symbol the relation starts.")
   (ref_to
    :initarg :ref_to
    :documentation "List with one symbol-name that indicate to which symbol the relation points.")))

(defmethod o ((obj rel))
  (with-slots ((att attributes) (com comment) (lab label) (rep representation) (tax taxonomy)
	       (refto ref_to) (reffrom ref_from) (wei weight)) obj
    (list :attributes att :comment com :label lab :ref_to refto :ref_from reffrom :representation rep :taxonomy tax :weight wei)))

(defmethod print-object ((object rel) *current-stream*)
  (print-unreadable-object (object *current-stream* :type t)
    (with-slots (attributes comment taxonomy ref_to ref_from view weight) object
      (format *current-stream*
	      "attributes: ~s comment: ~s taxonomy: ~s ref_to: ~s ref_from: ~s weight: ~s"
	      attributes comment taxonomy ref_to ref_from weight))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions for this library

(define-condition escad-internal-error (error)
  ((error-text
    :initarg :error-text
    :reader error-text)))

(defun get-taxonomy-db-header ()
  "-> taxonomy-header-list
Get the taxonomy header from the taxonomy database."
  (car *taxonomy*))

(defun get-taxonomy-db-attributes ()
  "-> taxonomy-attribute-list
Get the attributes-taxonomies-property-list from the taxonomy database."
  (getf (cadr *taxonomy*) :attribute))

(defun get-taxonomy-db-relations ()
  "-> taxonomy-relation-list
Get the relation-taxonomies-property-list from the taxonomy database."
    (getf (cadr *taxonomy*) :relation))

(defun get-taxonomy-db-symbols ()
  "-> taxonomy-symbol-list
Get the symbol-taxonomies-property-list from the taxonomy database."
  (getf (cadr *taxonomy*) :symbol))

(defun get-taxonomy-item (type taxonomy-name)
   ":type-key taxonomy-string -> taxonomy-item-list
Get exact named taxonomy-item whereby :type-key is :attribute, :relation or :symbol."
  (dolist (taxonomy-item (getf (cadr *taxonomy*) type))
     (if (string= taxonomy-name (getf taxonomy-item :taxonomy))
	  (return-from get-taxonomy-item taxonomy-item)))
  nil)

(defun get-taxo-prop (type taxonomy-name property)
  ":type-key taxonomy-string :property-key -> type-string
<get> <taxo>nomy <prop>erty of the taxonomy."
  (let ((item (get-taxonomy-item type taxonomy-name)))
     (if item
	 (getf item property)
	 nil)))

(defun init-escad ()
  (let ((cmd-line-args (cadr (get-cmdline-args))))
    (init-views)
    (load-taxonomy)
    (start-gui-server)
    ;(lisp_over_network)
    ;(handler-bind ((escad-internal-error #'skip-json_rpc-request))
    ;(json-rpc_over_network)))
    (pprint "Welcome and thanks for using escad!  :-)")
    (pprint "Please type now '(in-package :escad)' to switch to escad package for typing any escad command.")
    (pprint "Type then (help) to see short info about available escad commands.")))

(defun init-views ()
  "Initialize the two escad views with symbols and relations."
  ;(ns "_escad" :taxonomy "escad.escad" :comment "Settings for escad (https://github.com/mkollmar/escad) belonging to this view.")
  ;(ns "_encoding" :taxonomy "escad.encoding" :representation "UTF-8")
  ;(ns "_view" :comment "Settings for active view." :taxonomy "escad.export.pdf")
  ;(ns "_view_exportfile" :taxonomy "escad.filename_relative" :representation "view-0.pdf")
  ;(cs "_view")
  (tv)
  ;(ns "_escad" :attributes '("url" "https://github.com/mkollmar/escad" "encoding" "UTF-8") :comment "Settings for escad belonging to this view." :taxonomy "escad.symbol._escad")
  ;(ns "_view" :comment "Settings and a function for the current view.")
  ;(s "_view" :taxonomy "escad.symbol._escad.export.pdf")
  ;(asa "_view" (list "escad.attribute.filename_relative" "view-1.pdf"))
  ;(cs "_view")
  (tv))

(defun load-taxonomy (&optional (taxonomy-filename *escad-taxonomy-file*))
  "Load in taxonomy-tree information (all supported domains)."
  (with-open-file (in taxonomy-filename)
    (with-standard-io-syntax
      (setf *taxonomy* (read in)))))

(defun make-test-dot-import ()
  "Create schematic to test svg-mindmap export-expansion."
  (ns "import_dot!" :taxonomy "escad.symbol._escad.import.dot"))

(defun make-tutorial-schematic ()
  "Deletes current view and creates schematic for internal tutorial."
  (cls)
  (ns "mother") (ns "father") (ns "myself")
  (nr "has_child[1]" "mother" "myself") (nr "has_child[2]" "father" "myself") (nr "are_married" "father" "mother") (nr "hatKind1" "Vater" "Ich"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESCAD USER-COMMANDS:
(defun ad ()
  "-> diameter; longest-path(s)-list
<a>nalyze <d>iameter (length of longest path(s)) of current view and returns
multiple values: the diameter of longest path(s) and the path(s) itself."
  (let ((path-list '()) (longest-path '()) (path-length nil))
    (dolist (symbol (ls))
      (setq path-list (cons (list symbol) path-list)))
    (setq path-length (list-length (car (reverse (lsort (path path-list))))))
    (dolist (next-path (reverse (lsort (path path-list))))
      (if (= path-length (length next-path)) ; if there are multiple path with same length
	  (setq longest-path (concatenate 'list longest-path (list next-path)))
	  (return)))
    (values path-length longest-path)))

(defun adp (start-sym &optional (max-depth 10))
  "start_symbol_name [depth] -> list with paths
<a>nalyze all <d>irected <p>aths from start-symbol."
  (path (list (list start-sym)) max-depth))

(defun asr (sym_a sym_b)
  "symbol_name_string1 symbol_name_string2 -> root_symbol_name_string
<a>nalyze <s>ame <r>oot symbol (=root) from the given two symbols (=leaves)."
  (if (string= sym_a sym_b)
      (return-from asr sym_a)
      (dolist (sym_x (nth 0 (lsc sym_a)))
	(dolist (sym_y (nth 0 (lsc sym_b)))
	  (when (asr sym_x sym_y)
	    (return-from asr sym_x)))))
  nil)

(defun asw ()
  "-> assoc-list
<a>nalyze <s>ymbol <w>weights and return ordered weight (highest first) and associated symbol-name(s)."
  (let ((weights '()))
    (dolist (name (ls :exclude-taxonomy '("escad.symbol._escad" "escad.symbol._view" "escad.symbol._escad.export.mindmap.svg")))
      (with-slots ((w weight)) (gethash name escad::*symbols*)
	(if (assoc w weights) ; exists weight already in weights list?
	    (setf (cdr (assoc w weights)) (push name (cdr (assoc w weights))))
	    (setf weights (acons w (list name) weights))) ; new weight-entry
	))
    (sort weights #'> :key #'car)))

(defun aap ()
  "-> path-list
<a>nalyze <a>ll <p>ath's of current view."
  (let ((path-list '()))
    (dolist (symbol (ls))
      (setq path-list (cons (list symbol) path-list)))
    (path path-list)))

(defun apc (path symbol-list)
  "path symbol-name-list -> t | nil
<A>nalyze whether the given <p>ath (one!) <c>ontains all the given symbols."
  (dolist (symbol symbol-list)
    (if (not (member symbol path :test #'string=))
	(return-from apc nil)))
  t)

(defun ara (relation-name attr-tax-val-list)
  "relation-name attrib-taxonomies-values-list ->
<A>dd <r>elation <a>ttributes depending of key."
  (multiple-value-bind (rel rel-exists) (gethash relation-name *relations*)
    (if rel-exists
	(loop for (given-key given-val) on attr-tax-val-list by #'cddr
	   do
	     (with-slots ((a attributes)) rel
	       (setf a (remove given-key a :key #'car :test #'string=))  ; remove some possibly existing key before setting:
	       (setf a (acons given-key given-val a))
	       a))
	nil)))

(defun as (&optional (symbol "_view"))
  "[symbol-name] -> info-string
<a>ctivate <s>ymbol in current view.
What happens depends on the taxonomy of the symbol. Many symbols print out a string as their contents.
Symbols which represent expansions will execute the configured function of the expansion."
  (let* ((taxonomy-name (getf (o (s symbol)) :taxonomy))
	 (expansion-file (get-taxo-prop :symbol taxonomy-name :expansion))
	 (expansion-package (get-taxo-prop :symbol taxonomy-name :package))
	 (taxonomy-documentation (get-taxo-prop :symbol taxonomy-name :doc))
	 (expansion-function (get-taxo-prop :symbol taxonomy-name :function)))
    (cond
     (expansion-file  ; this seems to be expansion
      (unless (find-package expansion-package) (load (concatenate 'string *escad-lib-dir* expansion-file)))
      (use-package expansion-package)
      (pprint taxonomy-documentation)
      (funcall (read-from-string expansion-function) symbol))
     (t (pprint taxonomy-documentation)))))

(defun asa (symbol-name attr-tax-val-list)
  "symbol-name-string '(attrib-taxonomie-key value) -> nil / symbol-name
<A>dd/edit <s>ymbol <a>ttributes depending of key."
  (multiple-value-bind (sym sym-exists) (gethash symbol-name *symbols*)
    (if sym-exists
	(loop for (given-key given-val) on attr-tax-val-list by #'cddr
	   do
	     (with-slots ((a attributes)) sym
	       (setf a (remove given-key a :key #'car :test #'string=))  ; remove some possibly existing key
	       (setf a (acons given-key given-val a))
	       a))
	nil))
  symbol-name)

(defun asp (symbol1 symbol2)
  "symbol-name1 symbol-name2 -> path-list
<a>nalyze <s>hortest <p>ath between 2 given symbols whereby ignoring possible weights."
  (let ((path-list (path (list (list symbol1)))) (symbols (list symbol1 symbol2)) (result '()) (shortest-length nil) (shortest '()))
    (dolist (path path-list)
      (if (apc path symbols)
	  (setq result (cons path result))))
    (setq shortest-length (list-length (car (lsort result))))
    (dolist (path result)
      (if (= (list-length path) shortest-length)  ; collect all, if there are more shortest path's
	  (setq shortest (cons path shortest))))
    shortest))

(defun cls ()
  "<cl>ear <s>chematic. This removes ALL symbols and relations in current view!"
  (clrhash *symbols*) (clrhash *relations*) (init-views)
  t)

;(defun cps (symbols)
; "symbol-name-list -> symbol-name-list
;<co>py given <s>ymbol(s) with their relations to the other view. This overwrites some existing objects with same name!"
  ;; (let ((syms_src  (if (eq *symbols* *symbols1*) *symbols1* *symbols2*))
  ;; 	(syms_dest (if to-other-view-p (if (eq *symbols* *symbols1*) *symbols2* *symbols1*) syms_src))
  ;; 	(rels_src  (if (eq *relations* *relations1*) *relations1* *relations2*))
  ;; 	(rels_dest (if to-other-view-p (if (eq *relations* *relations1*) *relations2* *relations1*) rels_src)))
  ;;   (dolist (name symbols)
  ;;     (multiple-value-bind (sym sym-exists-p) (gethash name syms_dest)
  ;; 			   (if sym-exists-p
  ;; 			       (print "Overwrite symbol!"))
  ;; 			   (setf (gethash name syms_dest) sym)))
  ;;   (dolist (name relations)
  ;;     (multiple-value-bind (rel rel-exists-p) (gethash name rels_dest)
  ;; 			   (if rel-exists-p
  ;; 			       (print "Overwrite relation!"))
  ;; 			   (setf (gethash name rels_dest) rel))))
  ;; symbols)

(defun cpv ()
  "[src dest] -> t/nil
<c>o<p>y current active <v>iew in other view (all possible existing objects in destination will be deleted!)."
  (tv)
  (clrhash *symbols*)
  (clrhash *relations*)
  (if (eq *symbols* *symbols1*)
      (progn ; cp 1 -> 2
	(maphash #'(lambda(key value) (setf (gethash key *symbols2*) value)) *symbols1*)
	(maphash #'(lambda(key value) (setf (gethash key *relations2*) value)) *relations1*))
      (progn ; cp 2 -> 1
	(maphash #'(lambda(key value) (setf (gethash key *symbols1*) value)) *symbols2*)
	(maphash #'(lambda(key value) (setf (gethash key *relations1*) value)) *relations2*)))
  t)

(defun cus (&optional symbol)
  "[symbol-name-string] -> current-symbol
Get/set <cu>rrent <s>ymbol in current view."
  (when (stringp symbol) (setf *current_symbol* symbol))
  *current_symbol*)

(defun get-copyright-info ()
  ""
(pprint "What i am allowed to do?
------------------------
COPYRIGHT on ESCAD has Markus Kollmar <markuskollmar@onlinehome.de>.
ESCAD is licensed under: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.
If you want a other license (like for commercial purposes), please contact Markus Kollmar <markuskollmar@onlinehome.de>." *current-stream*))

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
	  (cdr (assoc attribute-key a :test #'string=)))
	nil)))

(defun gsdump ()
  "-> list of data-lists without keys just values in following order: name attributes comment label representation taxonomy ref_to ref_from weight.
<G>et all data of <s>ymbols in current schematic <dump>ed."
  (let ((result '()))
    (dolist (symbol-name (ls))
      (push (with-slots ((att attributes) (com comment) (lab label)
			 (rep representation) (tax taxonomy)
			 (refto ref_to) (reffrom ref_from) (wei weight)) (gethash symbol-name *symbols*)
			 (list symbol-name att com lab rep tax refto reffrom wei))
	    result))
    result))

(defun gsp (symbol-name)
  "symbol-name -> property-list
<g>et <s>ymbol <p>roperties as result in a property list."
  (with-slots ((com comment) (lab label) (rep representation) (tax taxonomy)
	       (wei weight)) (s symbol-name)
    (list :comment com :label lab :representation rep :taxonomy tax :weight wei)))

(defun gtd (&optional taxonomy)
  "[taxonomy-string] -> taxonomy-doc-string | nil
<G>et <t>axonomy <d>ocumentation. If there is no argument then print global documentation of taxonomy. If there is a argument then print documentation of the specified taxonomy. Return nil if doc or taxonomy not exists."
  (let ((result-string nil))
    (if taxonomy
	(dolist (taxonomy-item (cadr *taxonomy*))
	  (when (string= taxonomy (getf taxonomy-item :taxonomy))
	    (setq result-string (getf taxonomy-item :doc))))
	(setq result-string (getf (nth 1 (cdar *taxonomy*)) :doc)))
    result-string))

(defmacro help ()
  "-> command-list
Print overview of escad, meaning of terms and all available commands."
(pprint
"escad allows you to create, edit, use, analyze and view graphs. There can be expansions for all domains you want model as graph.
Type (help-command '<command-name>) for the help of command <command-name>.
Type (help-tutorial <step>) where <step> is a number starting from 0 (beginnging) to get a interactive tutorial for a feeling of escad's basics.

COPYRIGHT on ESCAD has Markus Kollmar <markuskollmar@onlinehome.de>.
ESCAD is licensed under: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

Definitions of terms used in escad:
 * ATTRIBUTES: universal usable property of a object additional to the standard attributes (optional).
 * CONTEXT: The set of symbols and relations which are directly related to and from a symbol.
 * EXPANSION: Concept to adapt escad for a special domain-specific purpose/funtionality (like a app).
 * GRAPH: combinations of connected symbols and relations in a view.
 * OBJECT-NAME: Is no property, it is it's idendity-string.
 * OBJECT: SYMBOL or RELATION in escad.
 * PATH-LIST: List with list(s) of symbol-relation-symbol-relation... names which are connected (at least a symbol must be given):
              '((Sym1 Rel2) (SymX)).
 * RELATION: object which represents/models a relationship between SYMBOLS.
 * SCHEMATIC: combinations of not necessarily connected symbols and relations (can also describe a graph) in a view.
 * SYMBOL: object which represents something (state, thing, process,...) the editor want.
 * TAXONOMY: classification scheme which are defined names you can use to specify relations and symbols in order to define their semantic.
 * VIEW: place (server, file, device, memory...) which contains SCHEMATIC data. There are two escad view's at runtime.

Commands grouped depending on function:
 * HELP        escad: help-command, help-tutorial
 * SHOW   attributes: gra, gsa
 * SET    attributes: ara, asa
 * REMOVE attributes: rra, rsa
 * SHOW      context: cs, lsc
 * EDIT/ACT. context: as, cs
 * INSERT    objects: nr, ns
 * REMOVE    objects: rr, rs
 * SHOW      objects: o, r, s, rp, sp
 * EDIT      objects: r, s
 * RENAME    objects: mr, ms
 * SHOW   properties: grp, gsp
 * SHOW     taxonomy: lta, gtd
 * LOAD         view: lov
 * SAVE         view: sav
 * SHOW         view: aap, lr, ls, rsel, ssel, vs
 * CHANGE       view: tv
 * CLEAR        view: cls
 * COPY         view: cpv
 * ANALYSE      view: aap, ad, adp, apc, asp, asr, asw" *current-stream*)
;`(let ((lst ()))
;  (do-external-symbols (s (find-package :escad)) (push s lst))
;  (sort lst (lambda (x y) (string< (symbol-name x) (symbol-name y)))))
)

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
This shows you all symbols on the schematic. You may expect none, but instead you see at least two.
There is nothing wrong with this. It are symbols which escad creates automatically.
With the _escad symbol you can configure your escad environment
for the current view. To see more about this symbol, type
'(as \"_escad\")'.
This 'activates' the symbol. You can do this with every symbol. But depending on the taxonomy of the symbol, there is different behaviour.
E.g. expansion-symbols will do their configured work if you activate them.
But most of the symbols print some text (comment), which is the meaningfull (content) of the symbol.
_view can store info about current view (e.g. author and copyright information).
_emergency is a escad-service providing offline based decision-tree with hints for acting in emergency-cases, but without guarante of correctness."))
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
This produces a svg-file in a settable directory location - standard is the public view-directory.
To watch the svg file, start an actual web-browser with svg-support and search the file.
"))
    ((= ,step 5)
(format t "Now you got the very basic idea of escad. Try to explore it by yourself. The inline-documentation will perhaps help you:
'(help)'
prints you a overview and the explanation of some escad-terms and also all available commands.
You get a help to every escad command by:
'(help-command 'COMMAND_NAME)'
Have fun with escad and stay tuned, it will get more improvements in the future.
If you have some suggestions, just write to the author of escad.
Thanks. :-)
"))
    (t (princ "Type (help-tutorial N) where N is a number beginning with 0, to describe which tutorial step you want see."))))


(defun lov (file_name)
  "absolut-file-name -> result_status_string
<lo>ad <v>iew from file into current view. All existing symbols and relations will be deleted! Note that the file can be a fast loading default escad save format or with key :as_escad_commands saved file. In the last case standard escad commands will be executed, so that this can take some time.
Escad recognizes the different files automatically, you not need to specify anything."
  (with-open-file (stream file_name :direction :input)
    (if (string= ";" (subseq (format nil "~a" (read-line stream)) 0 1)) ; if file starts with comment
	(progn (load file_name :verbose nil :print t) (return-from lov "Executed file with escad-commands (slow format)."))
	(with-open-file (in (concatenate 'string *escad-view-dir* file_name)) ; load in default file-format (may be faster):
	  (with-standard-io-syntax
	    (let ((input '()) header symbols relations)
	      (setf input (read in))
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
		      (make-instance 'sym :comment (cdr (assoc 'comment x)) :label (cdr (assoc 'label x))
				     :attributes (cdr (assoc 'attributes x))
				     :ref_from (cdr (assoc 'ref_from x)) :ref_to (cdr (assoc 'ref_to x))
				     :representation (cdr (assoc 'representation x))
				     :taxonomy (cdr (assoc 'taxonomy x)))))
	      (dolist (x relations)
		(setf (gethash (cdr (assoc 'name x)) *relations*)
		      (make-instance 'sym :comment (cdr (assoc 'comment x)) :label (cdr (assoc 'label x))
				     :attributes (cdr (assoc 'attributes x))
				     :ref_from (cdr (assoc 'ref_from x)) :ref_to (cdr (assoc 'ref_to x))
				     :representation (cdr (assoc 'representation x))
				     :taxonomy (cdr (assoc 'taxonomy x)))))))
	  "Loaded standard file data into view (fast format)."))))

(defun lr (&key (filter nil filter-p) (exclude-taxonomy nil exclude-p))
"[:FILTER filter] [:EXCLUDE-TAXONOMY exclude-taxonomy_string-list] -> symbol-list
<L>ist all <r>elations in current schematic which match the optional filter.
Additionally include/exclude relations which match the include/exclude-taxonomy."
  (let ((step1done-rels '()))
    (if filter-p  ; step1
	(maphash #'(lambda (k v)
		     (if (search filter k)
			 (push k step1done-rels))) *relations*)
	(maphash #'(lambda (k v) (push k step1done-rels)) *relations*)) ; collect all keys of rels)
    (if exclude-p  ; step2
	(set-difference step1done-rels
			(loop for taxonomy in exclude-taxonomy
			   append (rsel step1done-rels :taxonomy taxonomy)) :test #'string=)  ; return only relations not in exclude list
	step1done-rels)))


(defun ls (&key (filter nil filter-p) (exclude-taxonomy nil exclude-p))
  "[:FILTER filter] [:EXCLUDE-TAXONOMY exclude-taxonomy_string-list] -> symbol-list
<L>ist all <s>ymbols in current schematic which match the optional filter.
Additionally exclude symbols which match the exclude-taxonomy."
  (let ((step1done-syms '()))
    (if filter-p  ; step1
	(maphash #'(lambda (k v)
		     (if (search filter k)
			 (push k step1done-syms))) *symbols*)
	(maphash #'(lambda (k v) (push k step1done-syms)) *symbols*)) ; collect all keys of syms)
    (if exclude-p  ; step2
	(set-difference step1done-syms
			(loop for taxonomy in exclude-taxonomy
			   append (ssel step1done-syms :taxonomy taxonomy)) :test #'string=)  ; return only symbols not in exclude list
	step1done-syms)))

(defun lsc (&optional (name *current_symbol*))
  "[symbol-name] -> list-of-2lists-of-lists
<L>ist <s>ymbol <c>ontext of the current or a given symbol (default is current). The returned list has two lists. First are the input relation(s) and second with output relation(s). The containing data(s) is a two element list with first element containing the relation-name and the second is the related symbol-name of this relation."
  (let ((sym-go-in '()) (rel-go-in '()) (rel-go-out '()) (sym-go-out '()))
    (with-slots (ref_from ref_to) (gethash name *symbols*)
      (setq rel-go-in ref_from)
      (setq rel-go-out ref_to)
      (dolist (in-rel ref_from)
	(with-slots (ref_from ref_to) (gethash in-rel *relations*)
	  ; store symbols from which is pointed to this symbol:
	  (setq sym-go-in (concatenate 'list sym-go-in ref_from))))
      (dolist (out-rel ref_to)
	(with-slots (ref_from ref_to) (gethash out-rel *relations*)
	  ; store symbols to which is pointed to by this symbol:
	  (setq sym-go-out (concatenate 'list sym-go-out ref_to))))
      (list sym-go-in rel-go-in rel-go-out sym-go-out))))


(defun lst (&optional filter)
  "[filter-string] -> list of taxonomy-strings
<L>ist all or filtered <s>ymbol <t>axonomies. Note: to reflect some actual changes in the taxonomy-database you should restart escad."
  (let ((result '()) (symbol-taxonomies (get-taxonomy-db-symbols)))
    (dolist (taxonomy-item symbol-taxonomies)
      (if (or (and filter (search filter (getf taxonomy-item :taxonomy)))
	      (not filter))
	  (push (getf taxonomy-item :taxonomy) result)))
    result))


(defun mr (name new_name)
  "relation-name -> relation-name
<m>ove <r>elation - which means to change it's full-name."
  (setf (gethash new_name *relations*) (gethash name *relations*))
  (remhash name *relations*)
  new_name)

(defun ms (name new_name)
  "name new-name -> symbol-object
<m>ove <s>ymbol - which means to change it's name."
  (setf (gethash new_name *symbols*) (gethash name *symbols*))
  (remhash name *symbols*)
  (gethash new_name *symbols*))

(defun nr (ref_from ref_to &optional name &key attributes comment label representation (taxonomy "escad") weight)
  "ref_from, ref_to, [relation-name-string :attributes :comment :label :representation :taxonomy :weight] -> relation-name
Create <n>ew <r>elation with given name and possible additional values in schematic. If the relation-name already exists do nothing and return nil. Default type is undirected relation. To make a directed or bidirected relation, set the appropriate taxonomy (note that ref_from and ref_to are only technical terms meaning you first tie the relation from that symbol to another. it can mean that is directe, but it is not guaranted that the author means that unless he makes that explicit with a relation that declares that)."
  (let* ((name name))
    (if (not (stringp name))
      (dotimes (i *escad-max-obj-count*)
	(setq name (concatenate 'string "r" (write-to-string i)))
	(unless (gethash name *relations*) (return name))))
    (multiple-value-bind (rel rel-exists) (gethash name *relations*)
      (if rel-exists
	  nil
	  (progn
	    (with-slots ((ref_from1 ref_from)) (gethash ref_to *symbols*)
	      (pushnew name ref_from1))
	    (with-slots ((ref_to2 ref_to)) (gethash ref_from *symbols*)
	      (pushnew name ref_to2))
	    (setf (gethash name *relations*)
		  (make-instance 'rel :comment comment :label label :representation representation :ref_to (list ref_to)
				 :ref_from (list ref_from) :taxonomy taxonomy :weight weight))
	    (if attributes (ara name attributes))
	    name)))))


(defun ns (&optional name &key attributes comment label representation (taxonomy "escad") weight)
  "[symbol-name-string :attributes :comment :label :representation :taxonomy :weight] -> symbol-name | nil
Create a <n>ew <s>ymbol with given or automatically generated name and possible additional values in schematic, or
if the symbol-name already exists do nothing and return nil."
  (let ((name name))
    (if (not (stringp name))
      (dotimes (i *escad-max-obj-count*)
	(setq name (concatenate 'string "s" (write-to-string i)))
	(multiple-value-bind (sym sym-exists) (gethash name *symbols*)
	  (unless sym-exists
	    (setf (gethash name *symbols*) (make-instance 'sym :comment comment :label label :representation representation :taxonomy taxonomy :weight weight))
	    (if attributes (asa name attributes))
	    (return name)))) ; sym with generated name created
      (multiple-value-bind (sym sym-exists) (gethash name *symbols*)
	(if (not sym-exists)
	    (progn
	      (setf (gethash name *symbols*) (make-instance 'sym :comment comment :label label :representation representation :taxonomy taxonomy :weight weight))
	      (if attributes (asa name attributes))
	      name) ; sym with requested name created
	    nil))))) ; sym exists already


(defun path (path-list &optional (max-depth 10))
  "path-list [depth] -> list of flat path(s)
List all <path>s by moving in outgoing (ref_to) direction from given path-list/symbol(s), with last visited/retrieved (newest) symbol first.
Makes a list of flat path ((A B C A) (C B)) - no nested structures. Optional give a maximum iteration depth (amount of visited relations)."
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

(defun r (name &key comment label representation ref_from ref_to taxonomy weight)
  "relation-name [comment label ref_from ref_to representation taxonomy weight] -> rel-obj | nil
Sets <r>elation properties (slots) given by key - if any given - and returns relation, or 'nil' if not existent."
  (multiple-value-bind (rel rel-exists) (gethash name *relations*)
    (if rel-exists
	(with-slots ((com comment) (lab label) (reffrom ref_from) (refto ref_to) (rep representation)
		     (tax taxonomy) (wei weight)) rel
	  (if comment (setf com comment))
	  (if label (setf lab label))
	  (if ref_from (push ref_from reffrom))
	  (if ref_to (push ref_to refto))
	  (if representation (setf rep representation))
	  (if (and taxonomy (not tax)) (setf tax taxonomy))  ; not overwrite existing taxonomy
	  (if weight (push weight wei))
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
			 (with-slots ((a attributes)) rel
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
			 (with-slots ((a attributes)) sym
				     (setf a (remove attribute-taxonomy a :key #'car :test #'string=))
				     a))
		       nil))

(defun rsel (relations &key (name nil name-p) (comment nil comment-p) (taxonomy nil taxonomy-p))
  "relation-names-list [name comment taxonomy] -> relation-names-list
<r>elation <sel>ector: create list of relations out from given relation-list which match all given conditions."
  (let ((rel_set '()))
    (dolist (rname relations)
      (with-slots ((comment_ comment) (taxonomy_ taxonomy)) (gethash rname *relations*)
		  (if (and (if name-p     (string-equal name rname) t)
			   (if comment-p  (string-equal comment comment_) t)
			   (if taxonomy-p (string-equal taxonomy taxonomy_) t))
		      (push rname rel_set))))
    rel_set))

(defun s (name &key comment label representation taxonomy weight)
  "symbol-name [comment label representation taxonomy weight] -> sym-obj | nil
Sets <s>ymbol properties (slots) depending of key - if any given - and returns symbol, or 'nil' if not existent."
  (multiple-value-bind (sym sym-exists) (gethash name *symbols*)
    (if sym-exists
	(with-slots ((com comment) (lab label) (rep representation) (tax taxonomy) (wei weight)) sym
	  (if comment (setf com comment))
	  (if label (setf lab label))
	  (if representation (setf rep representation))
	  (if taxonomy (setf tax taxonomy))
	  (if weight (setf wei weight))
	  (gethash name *symbols*))
	nil)))

(defun sav (file_name &key as_escad_commands)
  "file-name [:as_escad_commands] -> file_contents
<sa>ve current <v>iew in a specified file in a user-dir (which is predefined by escad-admin). Default format is the fast loading format.
However you can specify ::as_escad_commands key, which generates a file with escad-commands (usefull to edit view in a editor) producing the current view."
  (if as_escad_commands
      (let ((escad-commands "")) ;; save in slow user friendly file format
	(dolist (name (ls))
	  (with-slots (attributes comment label ref_from ref_to representation taxonomy weight) (gethash name *symbols*)
	    (setq escad-commands (concatenate 'string escad-commands
					      (format nil "(ns ~S :attributes '~S :comment ~S :label ~S :representation ~S :taxonomy ~S :weight ~S)~%" name attributes comment label representation taxonomy weight)))))
	(setq escad-commands (concatenate 'string escad-commands
					  (format nil ";; relations:~%")))
	(dolist (name (lr))
	  (with-slots (attributes comment label ref_from ref_to representation taxonomy weight) (gethash name *relations*)
	    (setq escad-commands (concatenate 'string escad-commands
					      (format nil "(nr ~S :attributes '~S :comment ~S :label ~S :representation ~S :taxonomy ~S :weight ~S)~%" name attributes comment label representation taxonomy weight)))))	
	(with-open-file (cmd-stream (concatenate 'string *escad-view-dir* file_name)
				    :direction :output
				    :if-exists :supersede
				    :if-does-not-exist :create)
	  (format cmd-stream ";; File format version <0>.~%;; Saved by escad command sav with key :as_escad_commands. Load this file in escad with command lov.~%~%;; symbols:~%~a" escad-commands)))
      (let ((output (make-array 3 :fill-pointer 0)) (symbols '()) (relations '()))
	(vector-push (list (cons 'escad_version *escad_version*) (cons 'escad_file_format *escad_file_format*)) output) ; save in fast file format
	(dolist (name (ls))
	  (with-slots (attributes comment label ref_from ref_to representation taxonomy weight) (gethash name *symbols*)
	    (push (list (cons 'attributes attributes) (cons 'name name) (cons 'comment comment)
			(cons 'label label) (cons 'representation representation) (cons 'taxonomy taxonomy)
			(cons 'ref_from ref_from) (cons 'ref_to ref_to) (cons 'weight weight)) symbols)))
	(vector-push symbols output)
	(dolist (name (lr))
	  (with-slots (attributes comment label ref_from ref_to representation taxonomy weight) (gethash name *relations*)
	    (push (list (cons 'attributes attributes) (cons 'name name) (cons 'comment comment) (cons 'label label)
			(cons 'representation representation) (cons 'taxonomy taxonomy)
			(cons 'ref_from ref_from) (cons 'ref_to ref_to) (cons 'weight weight)) relations)))
	(vector-push relations output)
	(with-open-file (out (concatenate 'string *escad-view-dir* file_name)
			     :direction :output
			     :if-exists :supersede)
	  (with-standard-io-syntax
	    (print output out))))))


(defun sp (symbol-name property-name)
  "symbol-name-string property-name-symbol -> property-string
get <s>ymbol <p>roperty as result."
  (slot-value (s symbol-name) property-name))


(defun ssel (symbols &key (name nil name-p) (comment nil comment-p) (ref_from nil ref_from-p) (ref_to nil ref_to-p) (taxonomy nil taxonomy-p))
  "symbol-names-list [name comment ref_from ref_to taxonomy] -> (symbol-names)
<s>ymbol <sel>ector: create list of symbols out from given symbol-list which match all given conditions."
  (let ((sym_set '()))
    (dolist (sname symbols)
      (with-slots ((comment_ comment) (ref_from_ ref_from) (ref_to_ ref_to) (taxonomy_ taxonomy))
	  (gethash sname *symbols*)
	(when (and (if name-p     (string-equal name sname) t)
		   (if comment-p  (string-equal comment comment_) t)
		   (if ref_from-p (intersection ref_from ref_from_ :test 'equal) t)
		   (if ref_to-p   (intersection ref_to ref_to_ :test 'equal) t)
		   (if taxonomy-p (string-equal taxonomy taxonomy_) t))
	  (push sname sym_set))))
    sym_set))

(defun tv ()
  "-> number_of_view
Escad has two views, each representing a own schematic, so <t>oggle from one <v>iew to the other."
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

(defun vs ()
  " -> (ACTUAL_VIEW_NUMBER SYM_COUNT_VIEW1 REL_COUNT_VIEW1 SYM_COUNT_VIEW2 REL_COUNT_VIEW2)
Gives <v>iew <s>tatus."
  (let ((act_view nil) (sc1 nil) (rc1 nil) (sc2 nil) (rc2 nil))
    (setq act_view (if (eq *symbols* *symbols1*) 0 1))
    (setq sc1 (hash-table-count *symbols1*))
    (setq rc1 (hash-table-count *relations1*))
    (setq sc2 (hash-table-count *symbols2*))
    (setq rc2 (hash-table-count *relations2*))
    (list act_view sc1 rc1 sc2 rc2)))
