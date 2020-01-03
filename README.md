# Welcome to ESCAD.

ESCAD stands for **E**xpandable **S**ymbolic **C**omputer **A**ided **D**escription.
It is a tool which allows you to model parts of the real world in symbolic form with pre-defined taxonomy.
Therefore it only uses two objects: symbols and relations, which are combined to a graph.

After modeling the graph, you can generate content, reports or other things from teh graph.
The whole system is extendable through domain specific (self-writeable or existing) expansions, so you are not limited to the predefined shipped expansions.

# Competitors

* Gephi can provide you with much more data analysis and a nice graphical user interface. Escad is more focused to be a practical working horse, and not to be a analysis tool. Instead of java, escad is built on common-lisp.

* graphviz can generate beautiful visualizations of graphs. If you search that, you probably not need escad. But the aim of escad is to generate things like 3D-data, music or other domain specific data out of graphs (via a domain specific taxonomy expansion).

# Getting started

## Command-line-client
  * Start the system in the escad directory with: ./escad_server start terminal
  * Execute "(help)" in escad to get the newest information about escad. There is also a built in tutorial, so you can
    interactively learn how to use escad.
  * Stop the system in the escad directory with: ./escad_server stop

## Web-interface-client
  * Start the server in the escad directory with: ./escad_server start net-rest
  * Open Web-Browser and type: http://127.0.0.1:4000
  * Stop the system in the escad directory with: ./escad_server stop

## Net-Interface-API (for programms to use)
  * Start the server in the escad directory with: ./escad_server start net-lisp
  * Open Web-Browser and type: http://127.0.0.1:4000
  * Stop the system in the escad directory with: ./escad_server stop

Check the expansions for what you can do with your graph modelled in escad.

# COPYRIGHT

COPYRIGHT on ESCAD has Markus Kollmar <markuskollmar@onlinehome.de>.
ESCAD is licensed for no commercial use under: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.
If you need a other license (like for commercial purposes), please contact Markus Kollmar <markuskollmar@onlinehome.de>.

;; Copyright (C) 2013, 2014, 2019 Markus Kollmar
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
