# bin_t.mk - make targets for building shared & object bins
########################################################################
.PHONY: bin_all bin_clean bin_test bin_show
.PHONY: progs objs
#
#ifndef BIN_T_MK
#BIN_T_MK += DONE
#
include $(SLM_DIR)/$(STAR_ARCH).mk
include $(SLM_DIR)/depend.mk
#
bin_all:
ifneq ($(PROGS),$(EMPTY))
	-cd $(BINDIR); \
	$(MAKE) -f ../Makefile $(OBJS) $(PROGS)
endif
#
bin_clean:
	-cd $(BINDIR); \
	rm -f *.o *.d $(PROGS)
#
bin_test:
	-cd $(BINDIR); \
	$(MAKE) -f ../Makefile show
#
bin_show:
	@echo " ****************************** "
	@echo " ** bin.mk                   ** "
	@echo " ****************************** "
	@echo "    BINDIR = " $(BINDIR)
	@echo "     PROGS = " $(PROGS)
	@echo "  LIB_DIRS = " $(LIB_DIRS)
	@echo "  PAM_LIBS = " $(PAM_LIBS)
	@echo "  ASP_LIBS = " $(ASP_LIBS)
	@echo " USER_LIBS = " $(USER_LIBS)
	@echo " CERN_LIBS = " $(CERN_LIBS)
	@echo " LOAD_LIBS = " $(LOAD_LIBS)
#
progs:
	cd $(BINDIR); \
	$(MAKE) PROGS=a.out -f ../Makefile $(PROGS)
objs:
	cd $(BINDIR); \
	$(MAKE) -f ../Makefile $(OBJS)
#
vpath %.a $(subst $(SPACE),:,$(strip $(LIBDIRS)))
#
%Staf: %Staf.o $(USER_LIBS) $(CERN_LIBS)
	@echo "*** Building Staf executable:" $@ "***"
	rm -f $@
	$(LD) -o $@ \
	$(LDFLAGS) \
	$(LIB_DIRS) \
	$^ \
	$(LOAD_LIBS)
	chmod +x $@
#
tst_%: tst_%.o $(LOAD_LIBS) $(USER_LIBS) $(CERN_LIBS)
	@echo "*** Building test executable:" $@ "***"
	rm -f $@
	$(LD) -o $@ \
	$(LDFLAGS) \
	$(LIB_DIRS) \
	$^ \
	$(FOR_LIBS)
	chmod +x $@
#
$(PROGS): $(PAM_LIBS) $(ASP_LIBS) $(USER_LIBS) $(CERN_LIBS)
	@echo "*** Building program:" $@ "***"
	rm -f $@
	$(LD) -o $@ \
	$(addsuffix .o,$@) \
	$(LDFLAGS) \
	$(LIB_DIRS) \
	$(PAM_LIBS) $(ASP_LIBS) $(USER_LIBS) $(CERN_LIBS) \
	$(LOAD_LIBS)
	chmod +x $@
#
#endif #BIN_T_MK
#
