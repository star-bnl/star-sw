# srcm_t.mk - make targets for source files
########################################################################
.PHONY: srcm_all srcm_clean srcm_test srcm_show
.PRECIOUS: %Staf.cc
#
ifndef SRCM_T_MK
SRCM_T_MK+= DONE
#
srcm_all: $(shell ls $(ASP)Staf.cc)
#
srcm_clean:
	rm -f .backup.*Staf
#
srcm_test:
#
srcm_show:
	@echo " ****************************** "
	@echo " ** srcm.mk                  ** "
	@echo " ****************************** "
	@echo "   SRCMDIR = " $(SRCMDIR)
	@echo " SRCMFILES = " $(SRCMFILES)
#
$(BASE_NAME)Staf.cc: $(BASE_DIR)/Makefile.control
	-cp $@ .backup.$@
	-$(STAR_LIB)/$(STAR_SYS_LEVEL)/sys/bin/stafGen \
		-asp $(ASPS) -pam $(PAMS) > $@
#
whoops: $(shell ls .backup.$(BASE_NAME)Staf.cc)
	@echo $^
.backup.$(BASE_NAME)Staf.cc: FORCE
	mv $@ $(subst .backup.,$(EMPTY),$@)
FORCE:
#
endif #SRCM_T_MK
#
