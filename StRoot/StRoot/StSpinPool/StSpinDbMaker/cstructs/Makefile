# 

SCHEMEBASE  = Calibrations
SCHEMENAME  = rhic
STAR_DB_PATH =./
DBHOST      = robinson.star.bnl.gov
DBPORT      = 3306

DBSERVER = $(DBHOST):$(DBPORT)
SCHEME      = $(SCHEMEBASE)_$(SCHEMENAME)

SOURCES := spinDbV124 spinDbStar  spinDbBXmask

# #############################################################################
#
XPP      = cpp
XPPFLAGS = -P -C

.SUFFIXES: .hh .xml .h .idl

#
%.h: %.hh
	$(XPP) $(XPPFLAGS) $< $@

#
%.xml : %.h
	dbTableXml.pl -d $(SCHEME) -f $<

all:	
	@for d in $(SOURCES) ; do \
	 echo "$$d" ;\
	 make $$d.h ;\
	 make $$d.xml ;\
	 done

getidl:
	@echo use  -o $(STAR_DB_PATH) -d $(SCHEME) -s $(DBSERVER)
	@for i in $(SOURCES) ; do \
	echo "extracting  $$i "; \
	dbMakeFiles.pl -n $$i -o $(STAR_DB_PATH) -d $(SCHEME) -s $(DBSERVER) ;\
	done

clean:
	rm -f *.h *.xml