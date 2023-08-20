# Welcome to ESCAD.

ESCAD stands for **E**xpandable **S**ymbolic **C**omputer **A**ided **D**escription. It is a tool in common-lisp which allows you to model parts of the world in a graph (symbols and relations). Unlike some (still common) workflows which need the switching to different software tools and probably erroneous reentering of data, escad in principal (with domain expansions) allows model it once and do as many tasks as possible in escad.

After modeling the graph, you can generate content (like PDF, SVG, DOT,...) from the graph or extract informations. The whole system is extendable through domain specific (self-written or existing) expansions, so you are not limited to the predefined shipped expansion-functions.

# State

Escad is in basic development state. Currently there is no released version and nor is it guaranted to work (or just partly) after download! However the development follows the document driven development approach. Thus currently the activities are to prepare the manual and, controlled by it, also the code for the version 0.1 release.

Just clone the current repository and look at it. There are basically two two ways to use escad. The easiest way is to start the browser and use the graphical gui.
![escad-gui](./doc/figures/escad_web_ui.png)
Experienced user can use the commandline (or the scripting possibility) and directly write lisp code.
![escad-in_emacs](./doc/figures/escad_emacs.png)

# Getting started

Visit the [PDF-manual](./doc/escad_manual.pdf) for usage, user stories or a tutorial. Since escad is developed documentation-driven, this manual is the specification what should be in the first release. However it lives like the code, so you should regularly check it.

# COPYRIGHT

COPYRIGHT on ESCAD has Markus Kollmar (Germany).
ESCAD is licensed: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.
