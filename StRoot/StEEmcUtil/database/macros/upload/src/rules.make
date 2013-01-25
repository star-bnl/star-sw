# -*-Makefile-*-
#
XPP      = cpp
XPPFLAGS = -P -C 

# #############################################################################
#
.SUFFIXES: .hh .xml .h .idl

#
%.h: %.hh 
	$(XPP) $(XPPFLAGS) $< $@

#
%.xml : %.h
	dbTableXml.pl -d $(SCHEME) -f $<   

