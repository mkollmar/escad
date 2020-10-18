# Welcome to ESCAD.

ESCAD stands for **E**xpandable **S**ymbolic **C**omputer **A**ided **D**escription.
It is a tool which allows you to model parts of the real world in symbolic form with pre-defined taxonomy.
Therefore it only uses two objects: symbols and relations, which are combined to a graph.

After modeling the graph, you can generate content, reports or other things from the graph.
The whole system is extendable through domain specific (self-written or existing) expansions, so you are not limited to the predefined shipped expansions.

Currently there is no released version. Just clone the current repository and try it. Note that not all parts are working yet.

# Getting started

## Command-line-client
  * Start the system in the root directory with: ./escad_server start
  * Follow the hints on the screen. There is also a built in tutorial, so you can interactively learn how to use escad.
  * Stop the system in the escad directory with: ./escad_server stop

## Web-interface-client
  To be developed, not working right now!

## Net-Interface-API (for programms to use)
  * Start the server in the escad directory with: ./escad_server start net-lisp
  * Acess the system via a terminal connected to the server and follow the hints on the screen.
  * Stop the system in the escad directory with: ./escad_server stop

Check the expansions for what you can do with your graph modelled in escad.

# COPYRIGHT

COPYRIGHT on ESCAD has Markus Kollmar (Germany).
ESCAD is licensed: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.