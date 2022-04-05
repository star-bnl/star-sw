TEXSRC		= docXiFinder
MAKEFILE	= Makefile
GV              = /usr/local/bin/X11/gv
XDVI            = /usr/local/bin/X11/xdvi -copy -bg white -fg black 
STY             = 
DVIPS           = dvips -Z -D 600 -tletter
DVIPDF          = dvipdf
LATEX           = latex
# PSNUP		= /usr/local/bin/psnup -2 -pletter -d -b10
PSNUP		= /usr/local/bin/psnup -2 -pletter

all:			$(TEXSRC).ps

view:			$(TEXSRC).ps
			$(GV) $(TEXSRC).ps &

show:			$(TEXSRC).dvi
			$(XDVI) $(TEXSRC).dvi &

print:			$(TEXSRC).ps 
			lp -od $(TEXSRC).ps

psnup:			$(TEXSRC).psnup

pdf:                    $(TEXSRC).pdf


$(TEXSRC).psnup:	$(TEXSRC).ps
			$(PSNUP) $(TEXSRC).ps > $(TEXSRC).psnup

$(TEXSRC).pdf:		$(TEXSRC).dvi $(MAKEFILE)
			$(DVIPDF) $(TEXSRC).dvi $(TEXSRC).pdf

$(TEXSRC).ps:		$(TEXSRC).dvi $(MAKEFILE)
			$(DVIPS) $(TEXSRC).dvi -o $(TEXSRC).ps

$(TEXSRC).dvi:		$(TEXSRC).tex $(MAKEFILE) $(STY)
			$(LATEX) $(TEXSRC)

clean:			
			@rm -f $(TEXSRC).ps $(TEXSRC).psnup $(TEXSRC).dvi \
			$(TEXSRC).log $(TEXSRC).toc $(TEXSRC).aux \
			$(TEXSRC).ilg $(TEXSRC).lof .tex

bib:
			latex $(TEXSRC)
			latex $(TEXSRC)
