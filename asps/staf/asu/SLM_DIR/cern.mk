# cern.mk - make variables for CERN libraries
########################################################################
#
ifndef CERN_MK
export CERN_MK += DONE
#
ifndef CERNLIBS
export CERNLIBS := pawlib packlib mathlib kernlib
endif
export CERN_LIBS := $(shell cernlib $(CERNLIBS))
#
CDIR := $(dir $(word 1,$(CERN_LIBS)))
CLIBS := $(notdir $(filter %lib.a,$(CERN_LIBS)))
CLIBS := $(CLIBS:lib%.a=%)
DIVERSE := $(filter-out %lib.a,$(CERN_LIBS))
##export CERN_LIBS := $(addprefix -L,$(CDIR)) $(addprefix -l,$(CLIBS)) \
##	$(DIVERSE)
#
endif #CERN_MK
#
