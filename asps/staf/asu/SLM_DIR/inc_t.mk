# inc_t.mk - make targets for include and header files
########################################################################
.PHONY: inc_all inc_clean inc_test inc_show
#
ifndef INC_T_MK
INC_T_MK+= DONE
#
# HACK 25jun96
# inc_all: $(LOADFILES) $(notdir $(shell ls $(GENFILES)))
inc_all:
	cp ../idl/*.h .
	cp ../idl/*.inc .
#
inc_clean:
	-rm $(notdir $(shell ls $(GENFILES))) core
#
inc_test:
#
inc_show:
	@echo " ****************************** "
	@echo " ** inc.mk                   ** "
	@echo " ****************************** "
	@echo "   IDLDIR = " $(IDLDIR)
	@echo "   INCDIR = " $(INCDIR)
	@echo "  INCDIRS = " $(INCDIRS)
	@echo " INCFILES = " $(INCFILES)
	@echo " INCFILES = " $(notdir $(INCFILES))
	@echo " GENFILES = " $(GENFILES)
	@echo " GENFILES = " $(notdir $(GENFILES))
#
%.h: $(IDLDIR)/%.h
	cp $< $@
	echo $@ >> .includes_generated
%.inc: $(IDLDIR)/%.inc
	cp $< $@
	echo $@ >> .includes_generated
#
%_load.h:
	egrep -h '^STAFCV_T ' $(SRCDIR)/*_i.cc \
	| sed -e 's/STAFCV_T /#include "/' \
	| sed -e 's/_load_ami.*/.h"/' \
	> $@
%_load.cc:
	egrep -h '^STAFCV_T ' $(SRCDIR)/*_i.cc \
	| sed -e 's/STAFCV_T /   /' \
	-e 's/amiBroker \*//' \
	-e 's/$$/;/' \
	> $@
#
.includes_generated:
	touch $@
#
endif #INC_T_MK
#
