#include $(STAR)/asps/staf/MakeArch.mk
include $(STAR)/mgr/MakeSYS.mk
CMDS  = atlsim starsim gstar staf staf+ staf++ Staf
INCL  = -I$(STAR)/asps/staf/inc
STAF  = YES
ifeq ($(EMPTY),$(findstring $(STAR_HOST_SYS),i386_linux2))
Motif = YES
endif
AGI_DIR := $(CWD)
loadlibs :=
CPPFLAGS   += -DCERNLIB_SHL -DCERNLIB_TYPE -DCERNLIB_DZDOC -DCERNLIB_NONEWL -DCERNLIB_HIGZ
ifdef STAF
CPPFLAGS += -DSTAF $(INCL)
loadlibs += -L../lib -ltdm -lspx -lsoc -lasu -ltop -ltnt -lami -ldio -ldui -ldsl -ldsu 
endif
ifdef GCALOR
loadlibs +=     -L/afs/cern.ch/atlas/offline/@sys/pro/lib -lgcalor 
endif
ifdef  Motif
ifdef  Shift
loadlibs +=	`cernlib -v pro geant321 pawlib graflib/Motif packlib-shift mathlib kernlib-shift` /usr/local/lib/libshift.a
else
loadlibs +=	`cernlib -v pro geant321 pawlib graflib/Motif packlib mathlib kernlib`
endif
else 
ifdef  Shift
loadlibs +=	`cernlib -v pro geant321 pawlib graflib/X11  packlib-shift mathlib kernlib-shift` /usr/local/lib/libshift.a
else
loadlibs +=	`cernlib -v pro geant321 pawlib graflib/X11  packlib mathlib kernlib` 
endif
endif
loadlibs += $(LD_LIBS) $(CC_LIBS)
CPPFLAGS += -I. -I../ -I/usr/include -I$(STAR)/asps/staf/inc -I$(CERN_ROOT)/include 
FFLAGS   += -DCERNLIB_TYPE 
SUFX  := *.g *.c *.cc *.F *.f *.cdf
DIRS  := agsim comis hadr deccc dzdoc geant
FILES := $(foreach dir, $(DIRS), $(foreach s, $(SUFX), $(wildcard $(AGI_DIR)/gst/$(dir)/$(s))))
ifeq ($(LDS),$(CXX))
load  = $(LDS) $(LDS_FLAGS) $(AGI_DIR)/gst/agsim/ccsim.cc 
NAMES := $(filter-out ccsim, $(sort $(notdir $(basename $(FILES)))))
else
load  = $(LDS) $(LDFLAGS)
# $(AGI_DIR)/gst/agsim/fgsim.f  
#NAMES := $(filter-out fgsim, $(sort $(notdir $(basename $(FILES)))))
NAMES := $(sort $(notdir $(basename $(FILES))))
endif
OBJS  := $(addprefix $(STAR_BIN)/, $(addsuffix .o, $(NAMES)))
VPATH := $(addprefix $(AGI_DIR)/gst/, $(DIRS)) $(STAR_BIN)
#.SILENT:
.SUFFIXES:
.SUFFIXES: .sl .o .g .f .c .cc .cdf .rz
.PHONY               : all
all                  : geant3  detm  Staf 
#
$(STAR_BIN)/%.o:%.cdf
	cd $(STAR_BIN); kuipc     $(STEM); $(FC) -c $(STEM).f -o  $(ALL_TAGS); $(RM) $(STEM).f
$(STAR_BIN)/%.o:%.g  
	test -h $(STAR_BIN)/geant3.def || $(RM)  $(STAR_BIN)/geant3.def
	test -h $(STAR_BIN)/geant3.def || ln -s $(STAR)/asps/agi/gst/geant3.def  $(STAR_BIN)/geant3.def 
	cd $(STAR_BIN); geant3    $(FIRST_DEP) -o $(STAR_BIN)/$(STEM).F; \
        $(FC) $(CPPFLAGS) $(FFLAGS) -c $(STEM).F -o  $(ALL_TAGS); $(RM) $(STEM).F
$(STAR_BIN)/%.o:%.f
	$(FC)              $(FFLAGS)   -c $(FIRST_DEP) -o  $(ALL_TAGS)
$(STAR_BIN)/%.o:%.F
	$(FC)  $(CPPFLAGS) $(FFLAGS)   -c $(FIRST_DEP) -o  $(ALL_TAGS)
$(STAR_BIN)/%.o:%.c
	$(CC)  $(CPPFLAGS) $(CFLAGS)   -c $(FIRST_DEP) -o  $(ALL_TAGS)
$(STAR_BIN)/%.o:%.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $(INCL)  $(FIRST_DEP) -o $(ALL_TAGS)
#
#
#geant3:$(STAR_BIN)/geant3
detm: 
$(STAR_BIN)/geant3: gst/geant3/geant3.f; $(FC) $(FFLAGS) -o $(ALL_TAGS) $(FIRST_DEP) `cernlib -v pro kernlib`
$(STAR_BIN)/$(CMDS): $(OBJS) 
	cd $(STAR_BIN); $(load) $(OBJS) $(loadlibs) -o $(SCRATCH)/staf
ifneq (,$(findstring $(STAF_ARCH),rs_aix31 rs_aix32 rs_aix41))
	echo '#!'$(PWD)'/$(ALL_TAGS)'  > import.map
	nm $(ALL_TAGS)|egrep ' [BAD] '|cut -f1 -d' '|sed -e 's/^#/ #/'|sort|uniq>>import.map
	xlf $(ALL_TAGS) -o $(ALL_TAGS)  -bE:import.map -lX11 -lXm -lXt
endif
	cd $(STAR_BIN); mv  $(SCRATCH)/staf  $(ALL_TAGS)
 
detm:$(AGI_DIR)/gst/detm.rz
$(AGI_DIR)/gst/detm.rz:$(AGI_DIR)/gst/dzdoc.bank
	$(RM) $(ALL_TAGS); echo "trace on full; createdoc $(FIRST_DEP) $(ALL_TAGS); quit;" | dzedit
cleanup:
	$(RM) -f *.o *.f last* *~ *.lst *.exp
	$(RM) -f agsim.g agdummy.g kgsim.cdf comisc.* ccsim.cc
	$(RM) -f cmzsave.dat paw.metafile dzedit.las
atlas:	geant3 atlsim detm cleanup
star:	geant3 gstar  detm cleanup
show:
	@echo VPATH    := $(VPATH)
	@echo FILES    := $(FILES)
	@echo OBJS     := $(OBJS)
	@echo loadlibs := $(loadlibs)
	@echo load     := $(load)
	@echo "STAR_HOST_SYS=" $(STAR_HOST_SYS) "; OPSYS =" $(OPSYS)
	@echo "HOST      =" $(HOST)  "; STAR_SYS =" $(STAR_SYS)
	@echo "MAKE      =" $(MAKE) 
	@echo "VPATH     =" $(VPATH)
	@echo "SHELL     =" $(SHELL)
	@echo "MAKE      =" $(MAKE)
	@echo "MAKELEVEL =" $(MAKELEVEL)
	@echo "MAKEFILE  =" $(MAKEFILE)
	@echo "MAKFLAGS  =" $(MAKEFLAGS)
	@echo "SUFFIXES  =" $(SUFFIXES)
	@echo "STIC      =" $(STIC)	"; STICFLAGS	="	$(STICFLAGS)
	@echo "AR        =" $(AR)	"; ARFLAGS 	="	$(ARFLAGS)
	@echo "RANLIB    =" $(RANLIB)
	@echo "AS        =" $(AS)	"; ASFLAGS 	="	$(ASFLAGS)
	@echo "CC        =" $(CC)	"; CFLAGS 	="	$(CFLAGS)
	@echo "CXX       =" $(CXX)	"; CXXFLAGS 	="	$(CXXFLAGS)
	@echo "CPP       =" $(CPP)	"; CPPFLAGS 	="	$(CPPFLAGS)
	@echo "FC        =" $(FC)	"; FFLAGS 	="	$(FFLAGS)
	@echo "F_EXTENDED=" $(F_EXTENDED)
	@echo "LD        =" $(LD)	"; LDFLAGS	="	$(LDFLAGS)
	@echo "LD_LIBS   =" $(LD_LIBS)  "; CC_LIBS	="	$(CC_LIBS)
	@echo "LDS       =" $(LDS)      "; LDS_FLAGS    ="      $(LDS_FLAGS)
	@echo "RM        =" $(RM)
	@echo "SUBDIRS   =" $(SUBDIRS)
	@echo "LIBRARIES =" $(LIBRARIES)
	@echo "CERN_LIBS =" $(CERN_LIBS)
	@echo "DIRS      =" $(DIRS)
	@echo "ALL_DEPS  =" $(ALL_DEPS)
	@echo "FIRST_DEP =" $(FIRST_DEP)
	@echo "FIRSTF    =" $(FIRSTF)
	@echo "ALL_TAGS  =" $(ALL_TAGS)
	@echo "STEM      =" $(STEM)
	@echo "STEMF     =" $(STEMF)
	@echo "STIC      =" $(STIC)
	@echo "KUIPC     =" $(KUIPC)
	@echo "KUIPC_FLAGS=" $(KUIPC_FLAGS)
	@echo "EMPTY     =" $(EMPTY)
	@echo "ZERO      =" $(ZERO)
	@echo "ONE       =" $(ONE)
	@echo "TWO       =" $(TWO)
	@echo "THREE     =" $(THREE)
	@echo "FOUR      =" $(FOUR)








