burst-model.pdf : burst-model.tex burst-model-factor-graph.pdf
	pdflatex -shell-escape $<
	bibtex burst-model
	pdflatex -shell-escape $<
	pdflatex -shell-escape $<

%.pdf : %.svg
	inkscape --export-pdf=$@ $<

