# src_t.mk - make targets for source files
########################################################################
.PHONY: src_all src_clean src_test src_show
#
ifndef SRC_T_MK
SRC_T_MK+= DONE
#
src_all: $(notdir $(shell ls $(GENFILES)))
#
src_clean:
	-rm $(notdir $(GENFILES))
#
src_test:
#
src_show:
	@echo " ****************************** "
	@echo " ** src.mk                   ** "
	@echo " ****************************** "
	@echo "   IDLDIR = " $(IDLDIR)
	@echo "   SRCDIR = " $(SRCDIR)
	@echo "  SRCDIRS = " $(SRCDIRS)
	@echo " SRCFILES = " $(SRCFILES)
	@echo " SRCFILES = " $(notdir $(SRCFILES))
	@echo " CDFFILES = " $(CDFFILES)
	@echo " IDLFILES = " $(IDLFILES)
	@echo " GENFILES = " $(GENFILES)
	@echo " GENFILES = " $(notdir $(GENFILES))
	@echo "  SRCOBJS = " $(SRCOBJS)
	@echo "     OBJS = " $(OBJS)
#
$(CDFTYPES): $(CDFDIR)/%_def.cdf
	cp $(dir $<)/$@ $@
	echo $@ >> .sources_generated
$(IDLTYPES): $(IDLDIR)/%.idl
	cp $(dir $<)/$@ $@
	echo $@ >> .sources_generated
#
.sources_generated:
	touch $@
#
endif #SRC_T_MK
#
