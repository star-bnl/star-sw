#
# MANDATORY PARAMETERS -------------------------------------------------
#
TAS := $(EMPTY)
MID_FILE := $(EMPTY)
PROGS := translate_xdf2rzd
#
# OPTIONAL PARAMETERS --------------------------------------------------
#
#TASMAIN := ../srcm/$(STAR_ARCH)/tasmain.o
#DEBUG := TRUE
#PKOFF := TRUE
#GEANT := geant321
#CERN_LIBS := pawlib $(GEANT) graflib grafX11 packlib mathlib kernlib
CERN_LIBS := packlib
#
# ----------------------------------------------------------------------
# MAKE CHANGES BELOW HERE WITH CARE ------------------------------------
default: bin
SLM_DIR := $(STAR_ROOT)/$(STAR_LEVEL)/sys/mgr/slm
include $(SLM_DIR)/slm.mk
# ----------------------------------------------------------------------
-include $(BASE_DIR)/Makefile.user
-include $(BIN_DIR)/Makefile.user
# ----------------------------------------------------------------------
.PHONY: default all bin tas progs test install clean
#
bin: tas progs
#
tas: $(BIN_ARCH) $(MIDF_FILE)
	cd $(BIN_ARCH);$(MMAKE) MAKE_DEPEND=TRUE -f ../Makefile $(TAS)
#
progs: $(PROGS)
#
$(TAS): $(TAS_OBJ) $(LIB_LIBS)
	rm -f $@
	$(LD) -o $@ \
	$(LDFLAGS) \
	$^ \
	$(LD_PATH) $(LD_LIBS)
	chmod +x $@
#
$(PROGS): $(BIN_ARCH)
	cd $(BIN_ARCH);$(MMAKE) MAKE_DEPEND=TRUE PROGS=a.out \
	-f ../Makefile $@
#
%: %.o $(LIB_LIBS)
	rm -f $@
	$(CC) -o $@ \
	$(LDFLAGS) \
	$^ \
	$(LD_PATH) -ldsl $(CERN_LIBS) $(OS_LIBS) $(FOR_LIBS)
	chmod +x $@
#
test: slm_test
	@echo " TAS = " $(TAS)
	@echo " "
#
install: slm_install
#
clean: slm_clean
	cd $(BIN_ARCH); rm -f *.o *.d $(TAS) 
#
