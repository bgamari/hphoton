BurstFind.pdf : BurstFind.tex burst-model.pdf
	pdflatex -shell-escape $<

%.pdf : %.svg
	inkscape --export-pdf=$@ $<

