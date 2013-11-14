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

(
(
(:name "escad")
(:version 1))
(
(:taxonomy "escad.symbol" :doc "base (universal) symbol taxonomy" :data-type "STRING")
(:taxonomy "escad.symbol.escad.expansion" :doc "expansion." :data-type "EXPANSION-INTERFACE_LIST")
(:taxonomy "escad.symbol.escad.group" :doc "groups other symbols. mathematically it is a aequivalenz-relation." :data-type "SYMBOL_LIST")
(:taxonomy "escad.symbol.human.man" :doc "man" :data-type "STRING")
(:taxonomy "escad.symbol.human.woman" :doc "woman" :data-type "STRING")
(:taxonomy "escad.symbol.petri_net.net" :doc "net" :data-type "SYMBOL_LIST")
(:taxonomy "escad.symbol.petri_net.place" :doc "place" :data-type "STRING")
(:taxonomy "escad.symbol.process" :doc "process" :data-type "STRING")
(:taxonomy "escad.symbol.thing" :doc "thing" :data-type "STRING")
(:taxonomy "escad.symbol.topic" :doc "topic" :data-type "STRING")
(:taxonomy "escad.relation.bidirected" :doc "base (universal) relation taxonomy" :data-type "UNIVERSAL")
(:taxonomy "escad.relation.directed" :doc "base (universal) relation taxonomy" :data-type "UNIVERSAL")
(:taxonomy "escad.relation.directed.human.has_child" :doc "a human (man or woman) has genetic child" :data-type "STRING")
(:taxonomy "escad.relation.directed.petri_net.transition" :doc "transition" :data-type "STRING")
(:taxonomy "escad.relation.undirected" :doc "base (universal) relation taxonomy" :data-type "UNIVERSAL")
))