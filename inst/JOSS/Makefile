all:
	Rscript -e "library(knitr); knit('paper.Rmd')"
	pandoc -o paper.pdf paper.md

.PHONY: clean
clean:
	rm -f paper.pdf paper.md
