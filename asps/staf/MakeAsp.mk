#
#SHELL := csh -v
ifndef STAF_HOME
  STAF_HOME := $(shell pwd)
endif

TOPDIR := $(shell pwd)

SUFF_IDM := idl
SUFF_IDL := idl

###	Suppress all imlicit rules
.SUFFIXES:

include $(STAF_HOME)/MakeEnv.mk
include $(STAF_HOME)/MakeArch.mk

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
#	Upper INP directory
#
UPP_INP_DIR := $(subst / ,,$(dir $(INP_DIR)) )

ifndef OUT_DIR
  override OUT_DIR := $(dir $(UPP_INP_DIR))
endif


ifeq (,$(strip $(filter /%,$(OUT_DIR))))
  override OUT_DIR := $(CWD)/$(OUT_DIR)
endif

PKGNAME := $(notdir $(INP_DIR))

PKG_SDD :=
ifeq ($(PKGNAME),sdd)
  PKG_SDD := YES
endif

#
#	Define .src dir. If does not exist EMPTY
#
SRC_DIR := $(INP_DIR)/src
SRC_GEN_DIR := $(OUT_DIR)/srg/$(PKGNAME)
ifneq (YES,$(shell if { test -d $(SRC_DIR); } then { echo YES; } fi ))
  SRC_DIR :=
  SRC_GEN_DIR :=
endif


#
#	Define .srcm dir. If does not exist EMPTY
#
SRM_DIR     := $(INP_DIR)/srcm
#SRM_GEN_DIR := $(OUT_DIR)/SRG/$(PKGNAME)
ifneq (YES,$(shell if { test -d $(SRM_DIR); } then { echo YES; } fi))
  SRM_DIR :=
endif

CDF_DIR := $(SRC_DIR)

INC_DIR := $(INP_DIR)/inc
IDL_DIR := $(INC_DIR)
IDM_DIR := $(SRC_DIR)

LIB_DIR := $(OUT_DIR)/lib
OBJ_DIR := $(OUT_DIR)/obj/$(PKGNAME)
TMP_DIR := $(OUT_DIR)/tmp
INC_GEN_DIR := $(OUT_DIR)/inc



#	Includes
INCLUDES := $(addprefix -I,$(wildcard $(UPP_INP_DIR)/*/inc)) -I$(CERN_ROOT)/src/cfortran


BIN_DIR := $(OUT_DIR)/bin
#
#	Do we need BIN_DIR ???
#
ifndef PKG_SDD   
  ifeq ($(SRM_DIR),)
     BIN_DIR :=
  endif
endif


#
#	If NO source , NOTHING to do
#	Skip up to the end
#
DOIT := $(strip $(SRC_DIR) $(SRM_DIR))
ifdef DOIT


DEP_DIR := $(OUT_DIR)/dep/$(PKGNAME)

OUPUT_DIRS := $(LIB_DIR) $(OBJ_DIR) $(DEP_DIR) $(BIN_DIR) $(TMP_DIR) $(SRC_GEN_DIR) $(INC_GEN_DIR) 
INPUT_DIRS := $(SRC_DIR) $(SRM_DIR) $(IDL_DIR) $(IDM_DIR) 

# 	Make dirs before make real work. Othervice VPATH does not see
#    	non existing directories
MAKEDIRS := $(shell mkdir -p $(OUPUT_DIRS))




VPATH =  $(INPUT_DIRS) $(OUPUT_DIRS)


FILES_IDM := $(wildcard $(IDM_DIR)/*.$(SUFF_IDM))
FILES_IDL := $(wildcard $(IDL_DIR)/*.$(SUFF_IDL))
FILES_SRC := $(wildcard $(addprefix $(SRC_DIR)/, *.c *.cc *.f *.F *.l *.y ))
FILES_SRC := $(filter-out %-lex.c %-yacc.c, $(FILES_SRC))

FILES_CDF := $(wildcard $(CDF_DIR)/*.cdf)
FILES_SRM := $(wildcard $(addprefix $(SRM_DIR)/,*c  *.cc))
FILES_SRM := $(filter-out %-lex.c %-yacc.c, $(FILES_SRM))
#
#	Test for main() it MUST NOT be in src but .....
#	I hope it is temporary check
#ifdef FILES_SRC
#  QWE1 := $(shell grep -l -e '.*main *(.*)' $(FILES_SRC))
#endif
ifdef FILES_SRM
  QWE2 := $(shell grep -l -e '.*main *(.*)' $(FILES_SRM))
endif
#FILES_SRC := $(filter-out $(QWE1),$(FILES_SRC) )
FILES_SRM := $(filter     $(QWE2),$(FILES_SRM) )


#
#	Special nasty case for "str" package
ifeq ($(PKGNAME),str)
 UNDERSCORES := $(wildcard $(SRC_DIR)/*_*)
 NOUNDERSCOR := $(filter-out $(UNDERSCORES),$(FILES_SRC))
 ONLYOSFID   := $(wildcard $(SRC_DIR)/*_$(OSFID).c $(SRC_DIR)/*_$(OSFID).f )
 FILES_SRC := $(NOUNDERSCOR) $(ONLYOSFID)
endif


ASPS := $(notdir $(subst / , ,$(dir $(wildcard $(UPP_INP_DIR)/*/src)) ))

FILES_A := $(wildcard $(LIB_DIR)/lib*.a)
FILES_S := $(addprefix $(LIB_DIR)/lib,$(addsuffix .$(SOEXT),$(ASPS)))

NAMES_EXE := $(notdir $(basename $(FILES_SRM)))
ifdef PKG_SDD
  NAMES_EXE += stic
endif 
FILES_EXE := $(addprefix $(BIN_DIR)/,$(NAMES_EXE))

FILES_D := $(FILES_SRC) $(FILES_SRM) 
FILES_D := $(addsuffix .d, $(addprefix $(DEP_DIR)/,$(basename $(notdir $(FILES_D)))))

FILES_C_CDF := $(addsuffix .c, $(addprefix $(SRC_GEN_DIR)/,$(basename $(notdir $(FILES_CDF)))))

FILES_O := $(FILES_SRC)  $(FILES_CDF)
FILES_O := $(addprefix $(OBJ_DIR)/,$(addsuffix .o, $(notdir $(basename $(FILES_O)))))

FILES_DIDLM :=$(addsuffix .didlm, $(addprefix $(DEP_DIR)/,$(basename $(notdir $(FILES_IDLM)))))

MY_LIB := $(LIB_DIR)/lib$(PKGNAME).a 
MY_SO  := $(LIB_DIR)/lib$(PKGNAME).$(SOEXT)

#
##	Include .d and .didlm files
ifndef PKG_SDD
-include $(FILES_D) 
endif

.PHONY : all  CDFtoC  Libraries Executables DeleteDirs




all:  CDFtoC Libraries Executables DeleteDirs

Executables : $(FILES_EXE)


###############################################################################
#
#	Very special case: STIC creation
#	********************************
#
$(BIN_DIR)/stic:        $(OBJ_DIR)/idl-yacc.o $(OBJ_DIR)/templateStuff.o
ifeq ($(OSFID),lnx)
	$(LD) $(LDFLAGS) -o $(ALL_TAGS) $(ALL_DEPS) /usr/lib/libfl.a
else
	$(LD) $(LDFLAGS) -o $(ALL_TAGS) $(ALL_DEPS) -ly -ll 
endif
#
$(OBJ_DIR)/idl-yacc.o :  $(SRC_DIR)/idl.l $(SRC_DIR)/idl.y
	cd $(TMP_DIR);\
        $(RM) idl-yacc.c;\
	yacc -v $(SRC_DIR)/idl.y;\
	cat y.tab.c \
        | sed 's/#ifndef YYSTYPE/#ifndef BUG_IN_AIX/' \
        | sed 's/#define YYSTYPE/#define BUG_IN_AIX/' \
        > idl-yacc.c;\
	$(RM) y.tab.c ;\
        lex $(SRC_DIR)/idl.l;\
        $(RM) idl-lex.c;\
        cat lex.yy.c | sed 's/FILE \*yyin = {stdin},/FILE/' > idl-lex.c;\
        $(RM) lex.yy.c;\
        $(CC) $(CFLAGS) -I. -c idl-yacc.c -o $(ALL_TAGS)
#
###############################################################################
$(filter-out $(BIN_DIR)/stic,$(FILES_EXE)) : $(FILES_A)

$(BIN_DIR)/% : $(SRM_DIR)/%.cc
	$(CXX)  $(CPPFLAGS) $(CXXFLAGS)  $(INCLUDES)  $(SRM_DIR)/$(STEM).cc $(FILES_A) $(CLIBS) -o $(ALL_TAGS)

$(BIN_DIR)/% : (SRM_DIR)/%.c
	$(CXX)  $(CPPFLAGS) $(CXXFLAGS)  $(INCLUDES) $(SRM_DIR)/$(STEM).c  $(FILES_A) $(CLIBS) -o $(ALL_TAGS)

CDFtoC: $(FILES_C_CDF)


#Libraries : $(MY_LIB) $(MY_SO)
Libraries : $(MY_LIB) 

$(MY_LIB) : $(FILES_O)
	$(AR) $(ARFLAGS) $(MY_LIB) $(FILES_O)
#	OBJS := $(foreach  OBJ, $(FILES_O), $(shell test -z `nm -p $OBJ |  grep ' T main' ` || echo $(OBJ)))
#	$(AR) $(ARFLAGS) $(MY_LIB) $(OBJS)
#	@OBJS=; for OBJ in $(FILES_O)/ ; do x=`nm -p $$OBJ | grep ' T main' `; if test "$$x"="" ; then OBJS="$$OBJS $$OBJ"; fi done;\
#	$(AR) $(ARFLAGS) $(MY_LIB) $(OBJS)

$(LIB_DIR)/lib$(PKGNAME).$(SOEXT) : $(FILES_O)
	$(SO) $(SOFLAGS) -o $(LIB_DIR)/lib$(PKGNAME).$(SOEXT) $(FILES_O)

$(SRC_GEN_DIR)/%.c : %.cdf
	kuipc -c $(ALL_DEPS) $(SRC_GEN_DIR)/$(STEM).c

%.c : %.y
	$(YACC) $(ALL_DEPS) ; mv y.tab.c $(STEM).c

$(OBJ_DIR)/%.o : %.c 
	$(CC)  -c $(CPPFLAGS) $(CFLAGS)    $(INCLUDES) $(1ST_DEPS) -o $(ALL_TAGS)

$(OBJ_DIR)/%.o : %.cc 
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS)  $(INCLUDES) $(1ST_DEPS) -o $(ALL_TAGS)

$(OBJ_DIR)/%.o : %.F 
	$(FC)  -c $(CPPFLAGS) $(FFLAGS) $(INCLUDES) $(1ST_DEPS) -o $(ALL_TAGS)

$(OBJ_DIR)/%.o : %.f 
	$(FC)  -c $(CPPFLAGS) $(FFLAGS) $(INCLUDES) $(1ST_DEPS) -o $(ALL_TAGS)

$(SRC_GEN_DIR)/%-lex.c : $(SRC_DIR)/%.l 
	lex $(ALL_DEPS)
	$(RM) $(ALL_TAGS)
	cat lex.yy.c | sed 's/FILE \*yyin = {stdin},/FILE/' > $(ALL_TAGS)
	$(RM) -f lex.yy.c
 
$(SRC_GEN_DIR)/%-lex.c : $(SRM_DIR)/%.l 
	lex $(ALL_DEPS)
	$(RM) $(ALL_TAGS)
	cat lex.yy.c | sed 's/FILE \*yyin = {stdin},/FILE/' > $(ALL_TAGS)
	$(RM) lex.yy.c
 

$(DEP_DIR)/%.d: %.c 
	$(RM) $(ALL_TAGS)
	$(GCC)  -MM -MG $(CPPFLAGS) $(INCLUDES)  $(ALL_DEPS) | \
        sed -e 's/$(notdir $(STEM)).o/$(subst /,\/,$(OBJ_DIR)/$(STEM).o) $(subst /,\/,$(ALL_TAGS))/g'\
        > $(ALL_TAGS)

$(DEP_DIR)/%.d: %.cc 
	$(RM) $(ALL_TAGS)
	$(GCC)  -MM -MG $(CPPFLAGS) $(INCLUDES)  $(ALL_DEPS) | \
        sed -e 's/$(notdir $(STEM)).o/$(subst /,\/,$(OBJ_DIR)/$(STEM).o) $(subst /,\/,$(ALL_TAGS))/g'\
        > $(ALL_TAGS)


$(DEP_DIR)/%.d:  %.F
	$(RM)  $(ALL_TAGS)
	$(GCC)  -MM -MG $(CPPFLAGS) $(INCLUDES) $(ALL_DEPS)  | \
        sed -e 's/$(notdir $(STEM)).o/$(subst /,\/,$(OBJ_DIR)/$(STEM).o) $(subst /,\/,$(ALL_TAGS))/g'\
        > $(ALL_TAGS)

####%.d:  %.cdf

$(DEP_DIR)/%.didlm : $(IDM_DIR)%.$(SUFF_IDM) 
	cd $(GEN_DIR); $(STIC) $(STICFLAGS) $(SRC_DIR)/$(STEM).idlm; \
        $(GCC)  $(MKDEPFLAGS) $(STICFLAGS) $(SRC_DIR)/$(STEM).idlm | \
        sed -e 's/.idlm.o/.didlm/g' > $(GEN_DIR)/$(STEM).didlm
	$(STIC) -M  $(STICFLAGS) $(SRC_DIR)/$(STEM).idlm | grep ":" >> $(GEN_DIR)/$(STEM).didlm
#       temporarly, until stic is fixed:
	@sed -e 's/broker->newInvoker(\(.*\),/broker->deleteInvoker(\1); broker->newInvoker(\1,/' \
                $(GEN_DIR)/$(STEM)_i.cc > temp
	@mv  -f temp $(GEN_DIR)/$(STEM)_i.cc


DeleteDirs :
	rm -r $(TMP_DIR)

clean :
	-rm -r $(OUPUT_DIRS)


endif # end of DoIt

show: 
	@echo MAKE        := $(MAKE)
	@echo MAKEFLAGS   := $(MAKEFLAGS)
	@echo MAKEFILES   := $(MAKEFILES)
	@echo STAF_HOME   := $(STAF_HOME)
	@echo SUFF_IDM    := $(SUFF_IDM) 
	@echo SUFF_IDL    := $(SUFF_IDL) 
	@echo INP_DIR     := $(INP_DIR) 
	@echo OUT_DIR     := $(OUT_DIR) 
	@echo PKGNAME     := $(PKGNAME) 
	@echo PKG_SDD     := $(PKG_SDD)
	@echo SRC_DIR     := $(SRC_DIR)
	@echo SRC_GEN_DIR := $(SRC_GEN_DIR) 
	@echo SRM_DIR     := $(SRM_DIR) 
	@echo SRM_GEN_DIR := $(SRM_GEN_DIR) 
	@echo INC_DIR     := $(INC_DIR)
	@echo IDL_DIR     := $(IDL_DIR)
	@echo IDM_DIR     := $(IDM_DIR) 
	@echo LIB_DIR     := $(LIB_DIR)
	@echo OBJ_DIR     := $(OBJ_DIR)
	@echo DEP_DIR     := $(DEP_DIR) 
	@echo TMP_DIR     := $(TMP_DIR)
	@echo INC_GEN_DIR := $(INC_GEN_DIR) 
	@echo BIN_DIR     := $(BIN_DIR) 
	@echo DOIT        := $(DOIT)
	@echo OUPUT_DIRS  := $(OUPUT_DIRS)
	@echo INPUT_DIRS  := $(INPUT_DIRS)
	@echo FILES_IDM   := $(FILES_IDM)
	@echo FILES_IDL   := $(FILES_IDL)
	@echo FILES_SRC   := $(FILES_SRC)
	@echo FILES_SRM   := $(FILES_SRM)
	@echo NAMES_EXE   := $(NAMES_EXE)
	@echo FILES_EXE   := $(FILES_EXE)
	@echo CDF_DIR     := $(CDF_DIR)
	@echo FILES_D     := $(FILES_D)
	@echo FILES_O     := $(FILES_O)
	@echo FILES_CDF   := $(FILES_CDF)
	@echo INCLUDES    := $(INCLUDES)
	@echo TOPDIR      := $(TOPDIR)
	@echo VPATH       := $(VPATH)
	@echo ASPS        := $(ASPS)
	@echo FILES_A   := $(FILES_A)
	@echo OSFID     := $(OSFID)
	@echo UNDERSCORES      := $(UNDERSCORES)
	@echo NOUNDERSCOR      := $(NOUNDERSCOR)
	@echo ONLYOSFID        := $(ONLYOSFID)
	@echo QWE1        := $(QWE1)
	@echo QWE2        := $(QWE2)
	@echo CXXFLAGS    := $(CXXFLAGS)
	@echo CFLAGS      := $(CFLAGS)
	@echo FFLAGS      := $(FFLAGS)
