# doc.mk - make variables and targets for ASP documentation
########################################################################
.PRECIOUS: %.html
.PHONY: doc_all doc_clean doc_test doc_show
#
ifndef DOC_MK
export DOC_MK += DONE
#
CFILES = $(wildcard $(BASE_NAME)/src/*.c) \
	$(wildcard $(BASE_NAME)/inc/*.h)
CCFILES = $(wildcard $(BASE_NAME)/src/*.cc) \
	$(wildcard $(BASE_NAME)/src/*.hh)
FFILES = $(wildcard $(BASE_NAME)/src/*.[Ff]) \
	$(wildcard $(BASE_NAME)/src/*.inc)
CCODE = $(CFILES) $(CCFILES) $(FFILES)
DREF := http://iago.lbl.gov/~tull/star/staf/$(BASE_NAME)/doc
#
endif #DOC_MK
#
ifndef DOC_MAK
DOC_MAK += DONE
#
# REMOVE DEFAULT ACTION
doc_all:
doc_doc: $(BASE_NAME)_code.html
#
doc_clean:
doc_clean_doc:
	-rm -r $(DOCDIR)/html
	-rm $(BASE_NAME)_code.html
	-rm CTAGS $(BASE_NAME)
doc_test:
doc_show:
	@echo " ****************************** "
	@echo " ** doc.mk                   ** "
	@echo " ****************************** "
	@echo " BASE_DIR = " $(BASE_DIR)
	@echo "BASE_NAME = " $(BASE_NAME)
	@echo "   DOCDIR = " $(DOCDIR)
	@echo "    CCODE = " $(CCODE)
	@echo "    CCODE ==" $(CCODE:$(BASE_NAME)%=$(BASE_DIR)%)
#
CTAGS:
	-rm $@
	touch $@
	-ln -s $(BASE_DIR) $(DOCDIR)/$(BASE_NAME)
	ctags -txw $(CCODE) | sort >> $(DOCDIR)/$@
#
html: CTAGS
	-ln -s $(BASE_DIR) $(DOCDIR)/$(BASE_NAME)
	C_to_HTML.pl $<
#	$(STAR_LIB)/bin/C_to_HTML.pl $<
#
%_code.html: CTAGS html
	-rm $@
	echo "<HTML><HEAD><TITLE>$(BASE_NAME) Code</TITLE>" > $@
	echo "</HEAD><BODY>" >> $@
	echo "<H2>$(BASE_NAME) Code</H2><UL>" >> $@
	cd $(BASE_DIR); \
	find . \( -name '*.[CcHhFf]' -o -name '*.cc' -name '*.hh' \) -print | \
	sed -e 's/\.\(.*\/\)\(.*\)/<LI><A HREF=$(subst /,\/,$(DREF))\/html\/\2.html>$(subst /,\/,$(BASE_NAME))\1\2<\/A>/' \
	>> $(DOCDIR)/$@
	echo "</UL>" >> $@
	echo "<HR><I>Automagically generated documentation for" >> $@
	echo $(BASE_NAME) "code.<BR>" >> $@
	echo "Do not edit.</I><P>" >> $@
#
endif #DOC_MAK
#
