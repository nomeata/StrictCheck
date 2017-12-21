PDFLATEX = pdflatex -synctex=-1

DATE = "$(shell date +'%Y-%m-%d--%H-%M')"

NAME = main

all: $(NAME).tex
	rm -f $(NAME).log
	$(MAKE) once
	-$(PDFLATEX) -halt-on-error $<
	-$(PDFLATEX) -halt-on-error $<
#	-$(PULP) $(NAME).log

once:  $(NAME).tex
	$(PDFLATEX) -halt-on-error $<
	-bibtex $(NAME)
	cat $(NAME).pdf > temp.pdf

bcp: $(NAME).tex
	-$(PDFLATEX) -halt-on-error $<
	-bibtex $(NAME)
	pdfcrop --margins 20 $(NAME).pdf temp.pdf

#	pdfcrop $(NAME).pdf temp.pdf


# You can get pulp here: https://github.com/dmwit/pulp
PULP = "$(shell which pulp || echo \"$(HOME)\"~/.cabal/bin/pulp)"

pulp: $(NAME).tex
	-@rm -f $(NAME).log || true
	-@$(PDFLATEX) -interaction nonstopmode $< &> /dev/null
	-@bibtex $(NAME) | grep -v "Database file" | grep -v "This is BibTeX" |\
           grep -v "bibtex main" | grep -v "The top-level auxiliary file" |\
           grep -v "The style file"
	-@$(PDFLATEX) -interaction nonstopmode $< &> /dev/null
	-@$(PDFLATEX) -interaction nonstopmode $< &> /dev/null
	-@$(PULP) $(NAME).log

clean:
	@ rm -rf temp *.aux *.log $(NAME).pdf temp.pdf 
	@ rm -rf *~ *.log *.out *.bbl *.blg *.brf *.synctex* *.toc
	@ rm -rf comment.cut

.PHONY : all once pulp clean dist
