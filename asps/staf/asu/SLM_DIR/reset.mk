#
slm_reset:
	tar cvf reset_makefiles.tar `find . -name Makefile -print`
	rm - f `find . -name Makefile -print`
	cp -i $(SLM_DIR)/Makefile .
	$(MAKE) setup
#
