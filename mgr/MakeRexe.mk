# $Id: MakeRexe.mk,v 1.21 1999/05/08 23:19:36 fisyak Exp $
# $Log: MakeRexe.mk,v $
# Revision 1.21  1999/05/08 23:19:36  fisyak
# Add rootPstar
#
# Revision 1.20  1999/04/30 14:48:02  fisyak
# replace Root.exe by rootPstar for persistent StEvent
#
# Revision 1.19  1999/04/30 13:20:56  fisyak
# Split root4star into two version root4star with StEvent and rootPstar without it
#
# Revision 1.18  1999/04/24 13:15:24  fisyak
# Add --sillent mode for set SILENT environmnet variable
#
# Revision 1.17  1999/03/27 23:58:42  fisyak
# Add hbook to root
#
# Revision 1.16  1999/03/02 20:27:38  fisyak
# Filter out #
#
# Revision 1.15  1999/02/25 02:21:51  wenaus
# StSclRoot -> StarClassLibrary
#
# Revision 1.14  1999/02/20 19:39:55  fisyak
# Take out ROOT New library
#
# Revision 1.13  1999/02/19 01:12:07  fisyak
# Add xt
#
# Revision 1.12  1999/02/14 23:10:09  fisyak
# split tables for HP, remove duplicates for root4star
#
# Revision 1.11  1999/02/12 02:50:33  fisyak
# Fix St_Tables, single module
#
#  Make root4star executable

ifndef STAR_MAKE_HOME
  STAR_MAKE_HOME := $(STAR)/mgr
endif
include $(STAR_MAKE_HOME)/MakeEnv.mk

#
#	INP_DIR & OUT_DIR could be declared in invoking
#
include $(STAR_MAKE_HOME)/MakeDirs.mk

#CPPFLAGS += -DSTAF -DCERNLIB_DZDOC -DCERNLIB_NONEWL -DCERNLIB_SHL -DCERNLIB_HADRON 
CPPFLAGS += -D__ROOT__ -DCERNLIB_DZDOC -DCERNLIB_NONEWL -DCERNLIB_SHL -DCERNLIB_HADRON 
GEA := $(FOR72) $(FFLAGS)   $(CPPFLAGS)  
FOR := $(FOR72) $(FFLAGS)   $(CPPFLAGS)  
GST := $(FOR72) $(FFLAGS)   $(EXEFLAGS)
GSC := $(CXX)   $(CXXFLAGS) $(EXEFLAGS)
CXX := $(CXX)   $(CXXFLAGS) $(CPPFLAGS)  
CC  := $(CC)    $(CFLAGS)   $(CPPFLAGS)   
FSL := $(FOR72) $(FFLAGS)   $(CPPFLAGS)   $(SOFLAGS)


#	OUT dirs
OBJ_DIR := $(OUT_DIR)/.$(STAR_HOST_SYS)/obj/rexe
DEP_DIR := $(OUT_DIR)/.$(STAR_HOST_SYS)/dep/rexe
EXE_DIR := $(OUT_DIR)/.$(STAR_HOST_SYS)/bin
check   := $(shell test -d $(OBJ_DIR) || mkdir -p $(OBJ_DIR)) 
check   := $(shell test -d $(DEP_DIR) || mkdir -p $(DEP_DIR)) 
check   := $(shell test -d $(EXE_DIR) || mkdir -p $(EXE_DIR)) 
####EXE_DIR := $(ROOTSYS)/bin

ifAFS_OUT := $(strip $(filter /afs/%, $(OUT_DIR)))
ifdef ifAFS_OUT
  AFS_OBJ_DIR := $(OUT_DIR)/obj
  AFS_EXE_DIR := $(OUT_DIR)/bin
endif

SRC_DIRS := $(INP_DIR)  $(addprefix $(STAR)/asps/agi/gst/, agsim geant zebra)
#                         $(addprefix $(STAR)/asps/agi/gst/, agsim  main geant comis)
VPATH := $(SRC_DIRS) $(OUT_DIR) 
#                                  $(OBJ_DIR)  $(EXE_DIR) 

NAMES_O := $(wildcard $(addsuffix /*.[fFgc]*,$(SRC_DIRS)))
NAMES_O := $(filter-out $(STAR)/asps/agi/gst/agsim/agdummy.g, $(NAMES_O))
NAMES_O := $(filter-out \#%\#,$(NAMES_O))
NAMES_O := $(filter-out %.bck,$(NAMES_O))
NAMES_O := $(basename $(notdir $(NAMES_O)))
NAMES_O := $(filter-out traceqc,$(NAMES_O))
NAMES_D := $(filter-out MAIN_rmain, $(NAMES_O))
FILES_O := $(sort $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(NAMES_O))))
FILES_D := $(sort $(addprefix $(DEP_DIR)/,$(addsuffix .d,$(NAMES_D)))) 

INCL  := $(ROOTSYS)/src $(STAR)/StRoot/St_base $(INP_DIR) $(GST_DIR) $(STAF_SYS_INCS) \
         $(STAR)/asps/agi $(STAR)/asps/agi/kuip  $(CERN_ROOT)/include $(CERN_ROOT)/src/pawlib/paw/ntuple 
INCL  := $(addprefix -I,$(INCL))
CPPFLAGS += $(INCL)
#STAF  = YES
CCload = YES

# static linking of SCL and StEvent needed with current Solaris compiler
ifneq (,$(findstring $(STAR_SYS),sun4x_55 sun4x_56))
  STCLASS_OBJS =  $(wildcard $(STAR)/.$(STAR_HOST_SYS)/obj/StarClassLibrary/*.o \
$(STAR)/.$(STAR_HOST_SYS)/obj/StarClassLibrary/Templates.DB/*.o) 
  STEVENT_OBJS = $(wildcard $(STAR)/.$(STAR_HOST_SYS)/obj/StEvent/*.o \
$(STAR)/.$(STAR_HOST_SYS)/obj/StEvent/Templates.DB/*.o)
endif



#DOEXE  = $(GSC) $(GST_DIR)/main/acmain.c -o $(EXE_DIR)/$(TARGET) $(FILES_O)
DOEXE  = $(GSC)


ifdef STAF
DOEXE  += -DSTAF 
endif
DOEXE  += $(INCL)

ALL_EXE_LIBS :=
ifdef GCALOR
ALL_EXE_LIBS +=     -L/afs/cern.ch/atlas/offline/@sys/pro/lib -lgcalor 
endif

ifdef STAF
# ALL_EXE_LIBS +=    -L$(STAF_SYS_LIB) -lmsg -ltdm -lspx -lsoc -lasu -ltop -ltnt -lami -ldio -ldui -ldsl -ldsu -ltls 
endif

ALL_EXE_LIBS +=	`cernlib geant321 pawlib graflib/X11 packlib mathlib kernlib`
 
#ROOTLIBS      = -L$(ROOTSYS)/lib -lRint -lNew -lBase -lCint -lClib -lCont -lFunc \
#
ROOTLIBS      = -L$(ROOTSYS)/lib -lRint  -lBase -lCint -lClib -lCont -lFunc \
                -lGraf -lGraf3d -lHist -lHtml -lMatrix -lMeta -lMinuit -lNet \
                -lPostscript -lProof -lTree -lUnix -lZip
ROOTGLIBS     = -lGpad -lGui -lGX11 -lX3d





ALL_EXE_LIBS += $(ROOTLIBS) $(ROOTGLIBS)
ALL_EXE_LIBS += -lXpm $(FLIBS) $(CLIBS)	

.SUFFIXES:
.SUFFIXES:  .o .g .f .c .cc .cxx   .F
#all: rootPstar root4star 
ifndef STEVENT_OBJS
all: root4star 
else
all: rootPstar root4star 
endif
rootPstar: $(FILES_O) 
	$(DOEXE) $(ALL_DEPS) $(ALL_EXE_LIBS) -o $(EXE_DIR)/$(notdir $(TARGET))  
root4star: $(FILES_O) $(STCLASS_OBJS) $(STEVENT_OBJS)
	$(DOEXE) $(ALL_DEPS) $(ALL_EXE_LIBS) -o $(EXE_DIR)/$(notdir $(TARGET))  
#
#
#
$(OBJ_DIR)/%.o : %.g
	cd $(OBJ_DIR);\
	test -h geant3.def || $(RM)  geant3.def; \
	test -h geant3.def || ln -s $(STAR)/asps/agi/gst/geant3.def  geant3.def; \
	$(EXE_DIR)/geant3    $(1ST_DEPS) -o $(STEM).F; \
	$(FOR) -c $(INCL) $(STEM).F -o  $(OBJ_DIR)/$(STEM).o;
#        $(RM)  $(STEM).F;
$(OBJ_DIR)/%.o : %.f
	$(FOR) -c $(INCL) $(1ST_DEPS) -o  $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o : %.F
	$(FOR) -c $(INCL) $(1ST_DEPS) -o  $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o : %.c
	$(CC)  -c $(INCL) $(1ST_DEPS) -o  $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o : %.cc
	$(CXX) -c $(INCL) $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o : %.cxx
	$(CXX) -c $(INCL) $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o :%.cdf
	cd $(OBJ_DIR);\
	kuipc     $(1ST_DEPS) $(STEM).f;\
	$(FOR) -c $(STEM).f -o  $(OBJ_DIR)/$(STEM).o;
#	$(RM)     $(STEM).f
#
#
#ifDEP_DIR := $(wildcard $(DEP_DIR))
ifndef NODEPEND
 include $(FILES_D)
endif
include $(STAR_MAKE_HOME)/MakeDep.mk

cleanall :
	$(RMDIR) $(OUT_DIR)
clean:
	$(RMDIR) $(OBJ_DIR) $(EXE_DIR)

setup:  $(OBJ_DIR) $(EXE_DIR) $(AFS_OBJ_DIR) $(AFS_EXE_DIR)

$(OBJ_DIR) $(EXE_DIR): 
	@$(MKDIR) $(TARGET)

$(AFS_OBJ_DIR) :
	@$(LN) $(OUT_DIR)/.@sys/obj $(AFS_OBJ_DIR) 
$(AFS_EXE_DIR) :
	@$(LN) $(OUT_DIR)/.@sys/bin $(AFS_EXE_DIR) 



show:
	@echo VPATH=$(VPATH)
	@echo INP_DIR=$(INP_DIR)
	@echo OUT_DIR=$(OUT_DIR)
	@echo CC=$(CC)
	@echo STAR_HOST_SYS=$(STAR_HOST_SYS)
	@echo FILES_O=$(FILES_O)
	@echo FILES_D=$(FILES_D)
	@echo NAMES_O=$(NAMES_O)
	@echo SRC_DIRS=$(SRC_DIRS)
	@echo INCL=$(INCL)
	@echo STAF_SYS_INCS=$(STAF_SYS_INCS)
	@echo STAR_SYS=$(STAR_SYS)
	@echo STAF_SYS=$(STAF_SYS)
