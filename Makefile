# COPYRIGHT: Markus Kollmar
# Subject: ESCAD
#
# This file should make or guide to make all necessary things of escad.
#
# It assumes to use GNU-make. Some makefile hints:
#   * rule:  "target : prerequesites
#                    reciepe"
#   * "%.pdf" in target gives with prerequisite "%.tex" a "test.tex" if supplied a file "test.pdf".
#   * $@ = the file name of the target of the rule
#   * $< = the name of the first prerequisite
#   * @echo "Print a message in make"
#
# Currently just use following commands:
#    * To show all available commands with help:
#      > make help
#    * To delete all temporary tex files (e.g. *.aux, *.log):
#      > make clean
#    * To make pdf-files from tex-sources in figures-directory:
#      > make figures
#    * To make pdf-files from tex-sources in figures-directory with two parallel processes (jobs):
#      > make -j 2 figures
#    * To make a dry run (just print what to do):
#      > make -n
#
# GIT workflow hints:
#   Commit without stage:
#   * git commit -a -m 'Add new benchmarks'
#   Show all tags:
#   * git tag
#   Create tag with name v1.4:
#   * git tag -a v1.4 -m "my version 1.4"
#   Create branch and check out tag v1.4:
#   * git checkout -b my_branch1_4 v1.4
#   Delete tag:
#   * git tag -d v1.4
#   Create branch:
#   * git branch iss53
#   Checkout into current branch:
#   * git checkout iss53
#   Commit:
#   * git commit -a -m 'iss53 things'
#   Go back to master branch in order to merge:
#   * git checkout master
#   Merge branch iss53 into master:
#   * git merge iss53
#   Merge interacticely commits into one after given commit [commit-hash]:
#   * git rebase --interactive [commit-hash]
#   Show commits in one line (with graph visualisation):
#   git log --pretty=oneline --graph
#
##################################################################

### variables
SHELL = /bin/bash
latex = lualatex

doc_dir = ./doc/
lisp_dir = ./lisp/
figures_dir = ./doc/figures/
web_dir = ./web/

manual_tex = escad_manual.tex
manual_pdf = escad_manual.pdf
manual_tmp = escad_manual.aux escad_manual.bbl escad_manual.bcf escad_manual.blg \
	   escad_manual.glo escad_manual.idx escad_manual.lof escad_manual.lot \
	   escad_manual.out escad_manual.toc
texts_aux = $(manual_tex:.tex=.aux)
texts_log = $(manual_tex:.tex=.log)
bibliography = bibliography.bib
#texts_tex = $(wildcard *.tex)
#latex=rubber --pdf

figures_tex = $(wildcard $(figures_dir)*.tex) #$(notdir $(wildcard $(figures_dir)*.tex))
figures_aux = $(figures_tex:.tex=.aux)
figures_log = $(figures_tex:.tex=.log)
# note: we only need to generate pdf from tex files (other static pdf files need no makefile rule):
figures_pdf = $(figures_tex:.tex=.pdf)
figures_all = $(wildcard $(figures_dir)*.png $(figures_pdf)) #$(notdir $(wildcard $(figures_dir)*.png $(figures_pdf)))

#FIGURES_PNG=$(subst figures/svg/,defense/images/,$(FIGURES_SVG:.svg=.png))
###

.PHONY: executable
executable: escad ## make binary of lisp part
	sbcl --eval "(save-lisp-and-die \"escad\" :executable t :toplevel \"init-escad\")"	
#sbcl --eval "(asdf:operate :build-op :escad)"


.PHONY: manual
manual: $(doc_dir)$(manual_pdf)  ## Build manual (LaTeX + figures)

$(doc_dir)$(manual_pdf): $(doc_dir)$(manual_tex) $(figures_all) $(doc_dir)$(bibliography)
	cd $(doc_dir); $(latex) $(manual_tex)      # main run
	#cd $(doc_dir); biber $(manual_tex:.tex=)  # bibliography
	cd $(doc_dir); bibtex $(manual_tex:.tex=)  # bibliography
	cd $(doc_dir); $(latex) $(manual_tex)  # incremental run
	cd $(doc_dir); $(latex) $(manual_tex)  # incremental run
	cd $(doc_dir); makeglossaries $(manual_tex:.tex=) # list of abbreviations, nomenclature
	cd $(doc_dir); $(latex) $(manual_tex)  # incremental run
	cd $(doc_dir); $(latex) $(manual_tex)  # incremental run


.PHONY: figures
figures: $(figures_pdf)  ## Create pdf-figures from tex for the manuscript

#$(figures_all): %.pdf : %.tex
#$(filter %.pdf,$(figures_all)): %.pdf : %.tex
#$(addprefix $(figures_dir),$(figures_all))
$(figures_pdf): $(figures_tex)
	cd $(figures_dir); $(latex) $(notdir $<);


.PHONY: clean
clean:  ## Clean LaTeX and output figure files
	cd $(doc_dir); rm -f $(texts_aux) $(texts_log) #$(manual_tmp)
	cd $(figures_dir); rm -f $(figures_aux) $(figures_log)


.PHONY: run
run:  ## run escad
	cd lisp && sbcl --load main.lisp


.PHONY: test
test:  ## run module tests
	@echo "TODO!"


.PHONY: help
help:  # auto generate this help via http://marmelab.com/blog/2016/02/29/auto-documented-makefile.html
	@grep -P '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'


.DEFAULT_GOAL := help

