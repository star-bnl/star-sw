# lib_t.mk - make targets for building shared & object libs
########################################################################
.PHONY: lib_all lib_clean lib_test lib_show
.PHONY: libs alib solib objs
#
ifndef LIB_T_MK
LIB_T_MK += DONE
#
include $(SLM_DIR)/$(STAR_ARCH).mk
include $(SLM_DIR)/depend.mk
#
lib_all: $(LIBDIR) $(SOLIBDIR)
ifneq ($(LIBS),$(EMPTY))
	-cd $(LIBDIR); \
	$(MAKE) -f ../Makefile $(LIBS); \
	mv lib*.so $(SOLIBDIR)
endif
#
lib_clean:
	-cd $(LIBDIR); \
	rm -f *.o *.a *.so so_locations *.d
	-cd $(SOLIBDIR); \
	rm -f *.o *.a *.so so_locations *.d
#
lib_test:
#
lib_show:
	@echo " ****************************** "
	@echo " ** lib.mk                   ** "
	@echo " ****************************** "
	@echo "   LIBDIR = " $(LIBDIR)
	@echo " SOLIBDIR = " $(SOLIBDIR)
	@echo "  LIBDIRS = " $(LIBDIRS)
	@echo "     OBJS = " $(OBJS)
	@echo "     ALIB = " $(ALIB)
	@echo "    SOLIB = " $(SOLIB)
	@echo "     LIBS = " $(LIBS)
	@echo "    DEBUG = " $(DEBUG)
	@echo "    SHARE = " $(SHARE)
	@echo " CPPFLAGS = " $(CPPFLAGS)
	@echo "   FFLAGS = " $(FFLAGS)
	@echo "   CFLAGS = " $(CFLAGS)
#
libs:
	cd $(LIBDIR); \
	$(MAKE) -f ../Makefile $(LIBS)
#
alib:
	cd $(LIBDIR); \
	$(MAKE) -f ../Makefile $(ALIB)
#
solib:
	cd $(SOLIBDIR); \
	$(MAKE) -f ../Makefile $(SOLIB)
#
objs:
	cd $(LIBDIR); $(MAKE) $(OBJS)
#
$(ALIB): $(OBJS)
	$(AR) $(ARFLAGS) $@ $?
	-@$(RANLIB) $@
#
$(SOLIB): $(OBJS)
	$(SO) $(SOFLAGS) $@ $^
#
$(LIBDIR) $(SOLIBDIR):
	mkdir -p $@
#
endif #LIB_T_MK
#
