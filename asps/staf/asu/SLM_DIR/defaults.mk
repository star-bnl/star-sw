# defaults.mk - Default targets for make.
########################################################################
#
# Variables that must be definined:
#	BASE_DIR
#
####################################
# default directories
DIRS	:= bin cdf doc idl inc lib src srcm wrk \
	aix hpux irix sun4 sun4os5 osf1 \
	bin/aix bin/hpux bin/irix bin/sun4 bin/sun4os5 bin/osf1 \
	lib/aix lib/hpux lib/irix lib/sun4 lib/sun4os5 lib/osf1
$(DIRS):
	mkdir -p $@
#
####################################
# base makefile
BMAK	:= Makefile
$(BMAK):
	ln -s $(SLM_DIR)/Makefile $@
#
####################################
# sub makefiles
SMAKS	:= $(addsufix /Makefile,$(DIRS))
$(SMAKS):
	ln -s $(SLM_DIR)/Makefile.sub $@
#
####################################
# cdf files
KUIPC	:= $(shell which kuipc)
ifneq ($(KUIPC),$(EMPTY))
KUIPCFLAGS	+= $(EMPTY)
%.c: %.cdf
	$(KUIPC) $(KUIPCFLAGS) $< $@
endif
#
####################################
# Orbix idl files
IDL	:= $(shell which idl)
ifneq ($(IDL),$(EMPTY))
IDLFLAGS	:= -S -C -c C.cc -s S.cc
ifneq ($(IDLPATH),$(EMPTY))
IDLFLAGS	+= $(addprefix -I, $(IDLPATH))
endif
ifneq ($(IDLDEFS),$(EMPTY))
IDLFLAGS	+= $(addprefix -D, $(IDLDEFS))
endif
IDLOUTS 	:= %C.cc %S.cc %.hh
$(IDLOUTS): %.idl
	$(IDL) $(IDLFLAGS) $^
%.d: %.idl
	$(IDL) -O $(IDLFLAGS) $^ > $@
endif
#
####################################
# STAF idl files
STIC	:= $(shell which stic)
ifneq ($(STIC),$(EMPTY))
STICFLAGS	:= 
STICOUTS	:= %_i.cc %.h %.inc
$(STICOUTS): %.idl
	$(STIC) $(STICFLAGS) $^
endif
#
####################################
# Staf main source files
STAFGEN	:= $(shell which stafGen)
ifneq ($(STAFGEN),$(EMPTY))
$(BASE_NAME)Staf.cc: $(BASE_DIR)/Makefile.control
	-stafGen -asp $(ASPS) -pam $(PAMS) >! $@
endif
#
####################################
# Last-Resort Default Rule
%::
	@echo "*** CREATING NULL" $@ "***"
	touch $@
#
