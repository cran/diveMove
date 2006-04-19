# Makefile for LaTeX files

# LATEX	= latex
PDFLATEX = pdflatex
BIBTEX = bibtex
# DVIPS = dvips
# PSTOPDF = ps2pdf

SRC = diveMove.tex

all:	diveMove.pdf

%.pdf:	%.tex
	R CMD Rd2dvi --pdf -o 'diveMove-rd.pdf' --title='diveMove' ../../man/
	$(PDFLATEX) $*
	$(BIBTEX) $*
	$(PDFLATEX) $*
	$(PDFLATEX) $*
	$(PDFLATEX) $*
	rm -rf *.ps diveMove-* $*.dvi $*.log $*.bbl $*.aux $*.blg \
	$*.log $*.toc $*.out *.eps auto/
