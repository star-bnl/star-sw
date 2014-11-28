#==============================================================================
# Generic Makefile                                     Henning Weber 03.01.1996
#==============================================================================

LATEX		=	latex
DVIPS         	=       dvips
PS2PDF          =       ps2pdf


# --------------- Files involved ------------------
SRC 		= 	urqmd-user.tex
# -------------------------------------------------

SRCFILES 	= 	$(SRC) GNUmakefile
TARGET		=	urqmd-user.ps
INSTPATH	=	$(HOME)/local/doc

# --------------- Pattern rules -------------------

%.dvi: %.tex
	$(LATEX) $<
	$(LATEX) $<

%.ps: %.dvi
	$(DVIPS) -o $@ $<

%.pdf: %.ps
	$(PS2PDF) $< $@
# -------------------------------------------------

.PHONY:		all

all:		$(TARGET)
pdf:            urqmd-user.pdf

.PHONY:		clean

clean:
		-rm $(TARGET)		
		-rm *.aux
		-rm *.dvi
		-rm *.log

install:	all
		cp $(TARGET) $(INSTPATH)

deinstall:	
		-rm $(INSTPATH)/$(TARGET)

# --------------- Dependencies -------------------



