TEXSRC		= trs
MAKEFILE	= Makefile
GV              = /usr/local/bin/X11/gv
XDVI            = /usr/local/bin/X11/xdvi -copy -bg white -fg black 
STY             = 
DVIPS           = dvips -Z -D 600 -tletter
DVIPDF          = dvipdf
LATEX           = latex
MKINDEX         = makeindex -s sclindex.sty
# PSNUP		= /usr/local/bin/psnup -2 -pletter -d -b10
PSNUP		= /usr/local/bin/psnup -2 -pletter
PSSRC		= trsTitle.eps StarIcon.eps 

all:			$(TEXSRC).ps

view:			$(TEXSRC).ps
			$(GV) $(TEXSRC).ps &

show:			$(TEXSRC).dvi
			$(XDVI) $(TEXSRC).dvi &

print:			$(TEXSRC).ps 
			lp -od $(TEXSRC).ps

psnup:			$(TEXSRC).psnup

pdf:                    $(TEXSRC).pdf

index:			$(TEXSRC).idx
			$(MKINDEX) $(TEXSRC).idx

$(TEXSRC).psnup:	$(TEXSRC).ps
			$(PSNUP) $(TEXSRC).ps > $(TEXSRC).psnup

$(TEXSRC).pdf:		$(TEXSRC).dvi $(MAKEFILE) $(PSSRC)
			$(DVIPDF) $(TEXSRC).dvi $(TEXSRC).pdf

$(TEXSRC).ps:		$(TEXSRC).dvi $(MAKEFILE) $(PSSRC)
			$(DVIPS) $(TEXSRC).dvi -o $(TEXSRC).ps

$(TEXSRC).dvi:		$(TEXSRC).tex $(TEXSRC).ind $(MAKEFILE) $(PSSRC) $(STY)
			$(LATEX) $(TEXSRC)

$(TEXSRC).idx:		$(TEXSRC).tex
			$(LATEX) $(TEXSRC)

$(TEXSRC).ind:		$(TEXSRC).idx
			$(MKINDEX) $(TEXSRC).idx

clean:			
			@rm -f $(TEXSRC).ps $(TEXSRC).psnup $(TEXSRC).dvi \
			$(TEXSRC).log $(TEXSRC).toc $(TEXSRC).aux \
			$(TEXSRC).idx $(TEXSRC).ilg \
			$(TEXSRC).ind .tex

bib:
			latex $(TEXSRC)
			latex $(TEXSRC)
