#
# MANDATORY PARAMETERS -------------------------------------------------
#
TAS := slm_tas
MID_FILE := slm.mid
PROGS := $(EMPTY)
#
# OPTIONAL PARAMETERS --------------------------------------------------
#
#TASMAIN := ../srcm/$(STAR_ARCH)/tasmain.o
#DEBUG := TRUE
#PKOFF := TRUE
GEANT := geant321
#CERN_LIBS := pawlib $(GEANT) graflib grafX11 packlib mathlib kernlib
#MOTIF := TRUE
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
	cd $(BIN_ARCH);$(SLMAKE) MAKE_DEPEND=TRUE -f ../Makefile $(TAS)
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
	cd $(BIN_ARCH);$(SLMAKE) MAKE_DEPEND=TRUE PROGS=a.out \
	-f ../Makefile $@
#
%: %.o $(LIB_LIBS)
	rm -f $@
	$(LD) -o $@ \
	$(LDFLAGS) \
	$^ \
	$(LD_PATH) $(MID_LIBS) $(TAS_LIBS) $(CERN_LIBS) $(OS_LIBS) \
	$(XM_LIBS)
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
