# $Id: MakeRexe.mk,v 1.11 1999/02/12 02:50:33 fisyak Exp $
# $Log: MakeRexe.mk,v $
# Revision 1.11  1999/02/12 02:50:33  fisyak
# Fix St_Tables, single module
#
#  Make root4star executable

ifndef STAR_MAKE_HOME
  STAR_MAKE_HOME := $(STAR)/mgr
endif
include $(STAR_MAKE_HOME)/MakeEnv.mk
include $(STAR_MAKE_HOME)/MakeArch.mk

#
#	INP_DIR & OUT_DIR could be declared in invoking
#
ifndef INP_DIR
  override INP_DIR := $(CWD)
endif
ifeq (,$(strip $(filter /%,$(INP_DIR))))
  override INP_DIR := $(CWD)/$(INP_DIR)
endif

#
ifndef OUT_DIR
  override OUT_DIR := $(CWD)/EXE
endif

ifeq (,$(strip $(filter /%,$(OUT_DIR))))
  override OUT_DIR := $(CWD)/$(OUT_DIR)
endif


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

SRC_DIRS := $(INP_DIR)  $(addprefix $(STAR)/asps/agi/gst/, geant zebra)
#                         $(addprefix $(STAR)/asps/agi/gst/, agsim  main geant comis)
VPATH := $(SRC_DIRS) $(OUT_DIR) $(OBJ_DIR)  $(EXE_DIR) 

NAMES_O := $(wildcard $(addsuffix /*.[fFgc]*,$(SRC_DIRS)))
#NAMES_O := $(wildcard $(NAMES_O))
#NAMES_O := $(filter %.g %.f %.F %.c %.cc %.cxx %.cdf,$(NAMES_O))
NAMES_O := $(filter-out %.bck,$(NAMES_O))
NAMES_O := $(basename $(notdir $(NAMES_O)))
#NAMES_O := $(filter-out gyintf,$(NAMES_O))
# from asps/rexe
#NAMES_0 := $(filter-out agmain  fdummy ggsim, $(NAMES_O))
#NAMES_O := $(filter-out kgsim, $(NAMES_O))
#NAMES_O := $(filter-out traceqc pawfca pawbrk pawcs pgexi gkfort, $(NAMES_O))
#NAMES_O := $(filter-out csrmsl csjcal csaddr csfile  csext  csexec, $(NAMES_O))
#NAMES_O := $(filter-out tdm_clear_all tdm_map_table ami_module_register agpawq hplopt, $(NAMES_O))
#NAMES_O := $(filter-out UTILS_h2root afmain acmain ,$(NAMES_O))
NAMES_D := $(filter-out MAIN_rmain kgsim, $(NAMES_O))
FILES_O := $(sort $(addprefix $(OBJ_DIR)/,$(addsuffix .o,$(NAMES_O))))
FILES_D := $(sort $(addprefix $(DEP_DIR)/,$(addsuffix .d,$(NAMES_D)))) 

# ggsim.o agdummy.o dummy.o 
INCL  := $(ROOTSYS)/src $(STAR)/StRoot/base $(INP_DIR) $(GST_DIR) $(STAF_SYS_INCS) $(STAR)/asps/agi $(STAR)/asps/agi/kuip  $(CERN_ROOT)/include $(CERN_ROOT)/src/pawlib/paw/ntuple 
INCL  := $(addprefix -I,$(INCL))

#STAF  = YES
CCload = YES

# static linking of SCL and StEvent needed with current Solaris compiler
ifneq (,$(findstring $(STAR_SYS),sun4x_55 sun4x_56))
  STEVENT_OBJS =  $(wildcard $(STAR)/.$(STAR_HOST_SYS)/obj/StSclRoot/*.o \
$(STAR)/.$(STAR_HOST_SYS)/obj/StSclRoot/Templates.DB/*.o) $(wildcard \
$(STAR)/.$(STAR_HOST_SYS)/obj/StEvent/*.o \
$(STAR)/.$(STAR_HOST_SYS)/obj/StEvent/Templates.DB/*.o)
else
  STEVENT_OBJS = 
endif



#DOEXE  = $(GSC) $(GST_DIR)/main/acmain.c -o $(EXE_DIR)/$(TARGET) $(FILES_O)
DOEXE  = $(GSC)  $(FILES_O)


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
 
ROOTLIBS      = -L$(ROOTSYS)/lib -lRint -lNew -lBase -lCint -lClib -lCont -lFunc \
                -lGraf -lGraf3d -lHist -lHtml -lMatrix -lMeta -lMinuit -lNet \
                -lPostscript -lProof -lTree -lUnix -lZip
ROOTGLIBS     = -lGpad -lGui -lGX11 -lX3d





ALL_EXE_LIBS += $(ROOTLIBS) $(ROOTGLIBS)
ALL_EXE_LIBS += -lXpm $(FLIBS) $(CLIBS)	

#.SILENT:
.SUFFIXES:
.SUFFIXES:  .o .g .f .c .cc .cxx   .F



root4star: $(FILES_O) $(STEVENT_OBJS)
	$(DOEXE) $(STEVENT_OBJS) $(ALL_EXE_LIBS) -o $(EXE_DIR)/$(notdir $(TARGET))  
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
#ifdef ifDEP_DIR
 include $(FILES_D)
#endif
#
$(DEP_DIR)/%.d:  %.g
	$(RM)  $(ALL_TAGS)
	$(GCC)  -MM -MG -nostdinc -w -traditional $(CPPFLAGS) $(INCLUDES) -x c $(ALL_DEPS) | sed -e 's/\.g\.o/.o/' > $(ALL_TAGS)

$(DEP_DIR)/%.d: %.c 
	$(RM) $(ALL_TAGS)
	$(GCC)  -MM -MG -nostdinc -w  $(CPPFLAGS) $(INCLUDES)  $(ALL_DEPS) > $(ALL_TAGS)

$(DEP_DIR)/%.d: %.cc 
	$(RM) $(ALL_TAGS)
	$(GCC)  -MM -MG -nostdinc -w  $(CPPFLAGS) $(INCLUDES)  $(ALL_DEPS) > $(ALL_TAGS)


$(DEP_DIR)/%.d:  %.F
	$(RM)  $(ALL_TAGS)
	$(GCC)  -MM -MG -nostdinc -w -traditional $(CPPFLAGS) $(INCLUDES) -x c $(ALL_DEPS) | sed -e 's/\.F\.o/.o/' > $(ALL_TAGS)



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
