;; Copyright (C) 2021 Markus Kollmar
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
;;
;; This taxonomy is developed from community at https://schema.org/ (founded by
;; Google, Microsoft, Yahoo and Yandex).
;;
;; IMPORTANT:
;; Do NOT use this directly in your schematic unless you know what you do!
;; You are allowed to do so, but
;; this may break some "intelligent expansions" to work, because they may not
;; understand all the alternative taxonomies (schemas).
;; Another aspect is that parallel usage of different taxonomy would it make
;; hard to programm expansions for all of these.
;;
;; Escad tries a different approach to give you freedom for other taxonomies.
;; It is planned to implement expansions which try to "translate" between
;; other taxonomies. This creates a translation list which tries to find a
;; escad taxonomy for every non-escad taxonomy. This process will always
;; succeed, even if there are no directly matching taxonomys. In such a case
;; escad will use the root-symbol or root-relation as fallback (which is
;; conceptually correct, but may of course loose much semantic information).
;; If you accept this translation
;; list then you can use escad with the new taxonomy. However it is not
;; guaranted that all things work similar like with native escad taxonomy.
;;
(
(
(:name "schema-org")
(:author "schema.org")
(:doc "Alternative taxonomy as the escad-taxonomy. Note that if you create or use your own, you may not able to easily interchange data.
Doc-strings of the taxonomies begin with [] field, which tells - if necessary, separated with comma - following things:
E -> means a Expansion is using this symbol by symbol-activation with as-command to execute a function.
S -> means this symbol/relation should be insert only one time (Single) in the schematic.
U -> means the relation is treated as a Undirected relation.
B -> means the relation is treated as a Bidirected relation.")
(:license "?")
(:version 7.01)
)

(
 :attribute (
(:taxonomy "schema-org.attribute.default_taxonomy" :doc "" :type "STRING")
)

:relation (
(:taxonomy "schema-org.relation" :doc "Root, universal relation (lowest fallback).")
)

:symbol (
(:taxonomy "schema-org.thing" :doc "Root, universal symbol (lowest fallback).")
)
)

)
