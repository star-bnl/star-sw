# idl_t.mk - make targets for compiling IDL files
########################################################################
.PRECIOUS: %C.cc %S.cc %.hh %.h %.inc %_i.cc
.PHONY: idl_all idl_clean idl_test idl_show
.PHONY: idl_drvs
#
ifndef IDL_T_MK
IDL_T_MK += DONE
#
idl_all: $(IDLINCS)
#
idl_clean:
	rm -f $(GENFILES) $(IDLJUNK)
	rm -f *.d
#
idl_test:
	@echo "---"
	@echo $(ASP)
	@echo "---"
	@echo $(PAM)
	@echo "---"
#
idl_show:
	@echo " ****************************** "
	@echo " ** idl.mk                   ** "
	@echo " ****************************** "
	@echo "      IDL = " $(IDL)
	@echo "  IDLDIRS = " $(IDLDIRS)
	@echo " IDLFLAGS = " $(IDLFLAGS)
	@echo " IDLFILES = " $(IDLFILES)
	@echo " GENFILES = " $(GENFILES)
	@echo "  IDLJUNK = " $(IDLJUNK)
#
%.d: %.idl
	$(IDL) -O $(IDLFLAGS) $< > $@
#
#HACK - not in CVS 25jun96
#
$(GENTYPES): %.idl
	-$(IDL) $(IDLDIRS:%=-I%) $^
#
endif #IDL_T_MK
#
