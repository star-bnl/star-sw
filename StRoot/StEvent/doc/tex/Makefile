TEXINPUTS	=
TEXSRC		= StEvent
MAKEFILE	= Makefile
GV              = gv
DVIPS           = dvips -Z -D 600 -tletter
DVIPDF          = dvipdf
LATEX           = latex
MKINDEX         = makeindex -s StEventIndex.sty
PSNUP		= psnup -2 -pletter
PSSRC		= StarIcon.eps StEventTitle.eps \
		  cartoon1.eps cartoon2.eps cartoon4.eps \
		  run.eps event.eps monitors.eps trigger.eps \
		  nodes.eps tracks.eps umlClassIcon.eps umlClass.eps \
		  umlComposition.eps umlInheritance.eps \
		  umlAggregation.eps umlAssociation.eps umlDependency.eps

all:			$(TEXSRC).ps

view:			$(TEXSRC).ps
			$(GV) $(TEXSRC).ps &

psnup:			$(TEXSRC).psnup

pdf:			$(TEXSRC).pdf

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
			@rm -f $(TEXSRC).psnup $(TEXSRC).dvi \
			$(TEXSRC).log $(TEXSRC).toc $(TEXSRC).aux \
			$(TEXSRC).idx $(TEXSRC).ilg $(TEXSRC).pdf \
			$(TEXSRC).ind .tex $(TEXSRC).ps $(TEXSRC).out 
