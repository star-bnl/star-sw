# cdf_t.mk - make variables and targets for asp/cdf
########################################################################
.PRECIOUS: %.c
.PHONY: cdf_all cdf_clean cdf_test cdf_show
.PHONY: cdf_drvs
#
ifndef CDF_T_MK
CDF_T_MK += DONE
#
cdf_all: cdf_drvs
#
cdf_clean:
	rm -f $(CDFDRVS)
cdf_test:
cdf_show:
	@echo " ****************************** "
	@echo " ** cdf.mk                   ** "
	@echo " ****************************** "
	@echo "    CDFFILES = " $(CDFFILES)
	@echo "     CDFDRVS = " $(CDFDRVS)
	@echo "       KUIPC = " $(KUIPC)
	@echo " KUIPC_FLAGS = " $(KUIPC_FLAGS)
#
cdf_drvs: $(CDFDRVS)
#
%.c: %.cdf
	$(KUIPC) $(KUIPC_FLAGS) $< $@
#
endif #CDF_T_MK
#
