# wrk_t.mk - make targets for setting up working area
########################################################################
.PHONY: wrk_all wrk_clean wrk_test wrk_show
#
ifndef WRK_T_MK
export WRK_T_MK += DONE
#
#
wrk_all: links
wrk_clean: unlinks
	rm -f core last.kumac*
wrk_test:
wrk_show:
	@echo " ****************************** "
	@echo " ** wrk.mk                   ** "
	@echo " ****************************** "
	@echo $(patsubst %.o,%,$(wildcard $(BINDIR)/*.o))
#
links:
	-ln -s $(patsubst %.o,%,$(wildcard $(BINDIR)/*.o)) .
#
unlinks:
	-find . \! -name '.' -prune -type l \! -name Makefile \
		-exec rm {} \;
#
endif #WRK_T_MK
#
