# env_t.mk - make targets for creating environment
########################################################################
#
ifndef ENV_T_MK
ENV_T_MK += DONE
#
$(ENVDIRS):
	mkdir -p $@
#
$(ENVFILES):
	cp -i $(SLM_DIR)/$@ $@
#	ln -s $(SLM_DIR)/$@ $@
#
Makefile:
	cp -i $(MAKEFILEBASE) $@
#	ln -s $(MAKEFILEBASE) $@
#
%/Makefile:
	cp -i $(MAKEFILESUBD) $@
#	ln -s $(MAKEFILESUBD) $@
#
endif #ENV_T_MK
#
