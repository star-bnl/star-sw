#
#	Make file to make one ASP or PAM.  Victor.Perev Apr 1998
#
CWD := $(shell pwd)
MAKESTAFLOGON :=$(CWD)/makestaflogon.mk
ifMAKESTAFLOGON := $(strip $(wildcard $(MAKESTAFLOGON)))
ifdef ifMAKESTAFLOGON
  include $(MAKESTAFLOGON)
endif
#

ifndef STAF_MAKE_HOME
  STAF_MAKE_HOME := $(STAR)/mgr
endif


SUFF_IDM := idl
SUFF_IDL := idl

###	Suppress all imlicit rules
.SUFFIXES:

include $(STAF_MAKE_HOME)/MakeEnv.mk
include $(STAF_MAKE_HOME)/MakeArch.mk

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
DOMAIN 	    := $(notdir $(UPP_INP_DIR))

SYSTEM_DOMAIN := $(strip $(filter staf sys asps, $(DOMAIN)))

DOM := ana
ifdef SYSTEM_DOMAIN
 DOM :=sys
endif

ifndef OUT_DIR
  override OUT_DIR := GEN$(DOM)
endif


ifeq (,$(strip $(filter /%,$(OUT_DIR))))
  override OUT_DIR := $(CWD)/$(OUT_DIR)
endif

PKGNAME := $(strip $(subst / ,,$(notdir $(INP_DIR)) ))

PKG_SDD :=
ifeq ($(PKGNAME),sdd)
  PKG_SDD := YES
endif

#
#	Define .src dir. If does not exist EMPTY
#
SRC_DIR := $(INP_DIR)/src
SRC_GEN_DIR := $(OUT_DIR)/srg/$(PKGNAME)
SRC_DIR :=$(strip $(wildcard $(SRC_DIR)))
ifndef  SRC_DIR
  SRC_GEN_DIR :=
endif


#
#	Define .srcm dir. If does not exist EMPTY
#
SRM_DIR     := $(INP_DIR)/srcm
SRM_GEN_DIR := $(OUT_DIR)/srgm/$(PKGNAME)
SRM_DIR := $(strip $(wildcard $(SRM_DIR)))

CDF_DIR := $(SRC_DIR)

INC_DIR := $(INP_DIR)/inc
IDL_DIR := $(INC_DIR)
IDM_DIR := $(SRC_DIR)

LIB_DIR := $(OUT_DIR)/.$(STAF_ARCH)/lib
OBJ_DIR := $(OUT_DIR)/.$(STAF_ARCH)/obj/$(PKGNAME)
TMP_DIR := $(OUT_DIR)/tmp
INC_GEN_DIR := $(OUT_DIR)/inc



#	Includes
INCLUDES := $(SRC_DIR) $(INC_DIR)
INCLUDES += $(INC_GEN_DIR)
INCLUDES += $(STAF_SYS_INCS)
INCLUDES += $(CERN_ROOT_INCS)
INCLUDES += $(STAF_UTILS_INCS)

INCLUDES := $(addprefix -I,$(INCLUDES))

BIN_DIR := $(OUT_DIR)/.$(STAF_ARCH)/bin
#
#	Do we need BIN_DIR ???
#
ifdef SYSTEM_DOMAIN
  ifndef PKG_SDD   
    ifeq ($(SRM_DIR),)
       BIN_DIR :=
    endif
  endif
endif

#
#	If NO source , NOTHING to do
#	Skip up to the end
#
DOIT := $(strip $(SRC_DIR) $(SRM_DIR))
ifdef DOIT


DEP_DIR := $(OUT_DIR)/.$(STAF_ARCH)/dep/$(PKGNAME)

OUPUT_DIRS := $(LIB_DIR) $(OBJ_DIR) $(DEP_DIR) $(BIN_DIR) $(TMP_DIR) $(SRC_GEN_DIR) $(INC_GEN_DIR) 
INPUT_DIRS := $(SRC_DIR) $(SRM_DIR) $(IDL_DIR) $(IDM_DIR) 

# 	Make dirs before make real work. Othervice VPATH does not see
#    	non existing directories
MAKEDIRS := $(shell mkdir -p $(OUPUT_DIRS))




VPATH =  $(INPUT_DIRS) $(OUPUT_DIRS)

ifndef SYSTEM_DOMAIN
  FILES_IDM := $(wildcard $(IDM_DIR)/*.$(SUFF_IDM))
  FILES_IDL := $(wildcard $(IDL_DIR)/*.$(SUFF_IDL))
  FILES_IDH := $(FILES_IDL) $(FILES_IDM)
  FILES_IDH := $(addprefix $(INC_GEN_DIR)/,$(addsuffix .h,$(notdir $(basename $(FILES_IDH)))))
#  FILES_SRG := $(SRC_GEN_DIR)/$(PKGNAME)_init.cc
endif
FILES_SRC := $(wildcard $(addprefix $(SRC_DIR)/, *.c *.cc *.f *.F *.l *.y ))
FILES_SRC := $(filter-out %-lex.c %-yacc.c, $(FILES_SRC))

FILES_CDF := $(wildcard $(CDF_DIR)/*.cdf)
FILES_SRM := $(wildcard $(addprefix $(SRM_DIR)/,*.c  *.cc))
FILES_SRM := $(filter-out %-lex.c %-yacc.c, $(FILES_SRM))


ifdef FILES_SRM
  QWE2 := $(shell grep -l  '.*main *(.*)' $(FILES_SRM))
endif
FILES_SRM := $(filter     $(QWE2),$(FILES_SRM) )


#
#	Special nasty case for "str" package
ifeq ($(PKGNAME),str)
 UNDERSCORES := $(wildcard $(SRC_DIR)/*_*)
 NOUNDERSCOR := $(filter-out $(UNDERSCORES),$(FILES_SRC))
 ONLYSTRID   := $(wildcard $(SRC_DIR)/*_$(STRID).c $(SRC_DIR)/*_$(STRID).f )
 FILES_SRC := $(NOUNDERSCOR) $(ONLYSTRID)
endif


ASPS := $(notdir $(subst / , ,$(dir $(wildcard $(UPP_INP_DIR)/*/src)) ))



FILES_A := $(wildcard $(LIB_DIR)/lib*.a)
FILES_S := $(addprefix $(LIB_DIR)/lib,$(addsuffix .$(So),$(ASPS)))

#
#	List of all libs
ifdef SYSTEM_DOMAIN
  STAF_ANA_LIBS :=
endif
ALL_LIBS := $(FILES_A) $(STAF_ANA_LIBS) $(STAF_SYS_LIBS) # (keep last blank)  
#
#	Make -Ldir list to simplify output messages of compilation
L_DIR_THIS :=$(addprefix -L,$(sort $(subst / , ,$(dir $(FILES_A)) )))
L_DIR_SYS  :=$(addprefix -L,$(sort $(subst / , ,$(dir $(STAF_SYS_LIBS)) )))
ifeq ($(strip $(L_DIR_THIS)),$(strip $(L_DIR_SYS)))
  L_DIR_SYS :=
endif

L_DIR_ANA  :=$(addprefix -L,$(sort $(subst / , ,$(dir $(STAF_ANA_LIBS)) )))
LIB_DIRS := $(filter-out -L, $(L_DIR_THIS) $(L_DIR_ANA) $(L_DIR_ANA))

ALL_LIBS := $(subst lib,-l,$(basename $(notdir $(ALL_LIBS))))  
ALL_LIBS := -ltdm -lspx -lsoc -lasu -ltop -ltnt -lami -ldio -ldui -ldsl -ldsu
ifndef SYSTEM_DOMAIN
ALL_LIBS := $(ALL_LIBS) $(CERN_ROOT_LIBS) $(FLIBS) $(CLIBS)
else
ALL_LIBS := $(ALL_LIBS) $(CLIBS)
endif

ALL_LIBS := $(LIB_DIRS) $(ALL_LIBS)
#
#	Executables
NAMES_EXE := $(FILES_SRM) 
NAMES_EXE := $(notdir $(basename $(NAMES_EXE)))
ifndef SYSTEM_DOMAIN
  NAMES_EXE := $(PKGNAME)Staf
endif

ifdef PKG_SDD
  NAMES_EXE += stic
endif 
FILES_EXE := $(addprefix $(BIN_DIR)/,$(NAMES_EXE))

FILES_D := $(filter-out %.f,$(FILES_SRC) $(FILES_SRM) $(FILES_IDL))
FILES_D := $(addsuffix .d, $(addprefix $(DEP_DIR)/,$(basename $(notdir $(FILES_D)))))

FILES_C_CDF := $(addsuffix .c, $(addprefix $(SRC_GEN_DIR)/,$(basename $(notdir $(FILES_CDF)))))

FILES_O := $(FILES_SRC) $(FILES_SRG) $(FILES_CDF)
FILES_O := $(addsuffix .o, $(notdir $(basename $(FILES_O))))

FILES_DIDLM :=$(addsuffix .didlm, $(addprefix $(DEP_DIR)/,$(basename $(notdir $(FILES_IDLM)))))

MY_LIB := $(LIB_DIR)/lib$(PKGNAME).a 

# 	MY_SO
MY_SO  := $(LIB_DIR)/lib$(PKGNAME).$(So)
QWE  := $(wildcard $(MY_SO).*)
NEW_MY_SO := $(MY_SO).1000
ifdef QWE
  NQWE := $(words $(QWE))
  QWE  := $(word $(NQWE),$(QWE))
  QWE  := $(subst $(MY_SO).,,$(QWE))
  QWE  := $(shell expr $(QWE) + 1)
  NEW_MY_SO := $(MY_SO).$(QWE)
endif

#	for SDD only
ifdef PKG_SDD
  NAMES_EXE += stic
  MY_LIB := 
  MY_SO  :=
endif 

#
##	Include .d and .didlm files
ifndef PKG_SDD
 include $(FILES_D) 
endif

.PHONY : all  CDFtoC  IdlToH lib exe DeleteDirs




all:   lib exe DeleteDirs

exe : $(FILES_EXE)


###############################################################################
#
#	Very special case: STIC creation
#	********************************
#
$(BIN_DIR)/stic:        idl-yacc.o templateStuff.o
	$(LD) $(LDFLAGS) -o $(ALL_TAGS) $(addprefix $(OBJ_DIR)/,idl-yacc.o templateStuff.o) \
	      $(YACCLIB) $(LEXLIB) 
#
idl-yacc.o :  $(SRC_DIR)/idl.l $(SRC_DIR)/idl.y
	cd $(TMP_DIR);\
        $(RM) idl-yacc.c;\
	$(YACC) -v $(SRC_DIR)/idl.y;\
	cat y.tab.c \
        | sed 's/#ifndef YYSTYPE/#ifndef BUG_IN_AIX/' \
        | sed 's/#define YYSTYPE/#define BUG_IN_AIX/' \
        > idl-yacc.c;\
	$(RM) y.tab.c ;\
        $(LEX) $(SRC_DIR)/idl.l;\
        $(RM) idl-lex.c;
ifndef HPUX
	cd $(TMP_DIR); cat lex.yy.c | sed 's/FILE \*yyin = {stdin},/FILE/' \
	> idl-lex.c;
else
	cd $(TMP_DIR); cat lex.yy.c | sed 's/FILE \*yyin = {stdin},/FILE/' \
	| sed '/static void __yy__unused() { main(); }/d' \
	> idl-lex.c;
endif
	cd $(TMP_DIR); $(RM) lex.yy.c;\
        $(CC) $(CFLAGS) -I. -I$(SRC_DIR) -c idl-yacc.c -o $(OBJ_DIR)/idl-yacc.o
#
###############################################################################
$(filter-out $(BIN_DIR)/stic,$(FILES_EXE)) : $(FILES_A)

$(BIN_DIR)/% : $(SRM_DIR)/%.cc
	cd $(TMP_DIR);\
	$(CXX)  $(CPPFLAGS) $(CXXFLAGS) $(LDFLAGS) $(INCLUDES)  $(SRM_DIR)/$(STEM).cc $(ALL_LIBS) -o $(ALL_TAGS)

$(BIN_DIR)/% : (SRM_DIR)/%.c
	cd $(TMP_DIR);\
	$(CXX)  $(CPPFLAGS) $(CXXFLAGS) $(LDFLAGS) $(INCLUDES) $(SRM_DIR)/$(STEM).c   $(ALL_LIBS) -o $(ALL_TAGS)

#	PAM.exe
$(BIN_DIR)/$(PKGNAME)Staf : $(MY_LIB)
	cd $(SRC_GEN_DIR); $(RM) $(PKGNAME).cc $(PKGNAME)_init.cc; \
	$(STAFGEN) -p $(PKGNAME) > $(PKGNAME)Staf.cc; \
	$(PAMIGEN) $(PKGNAME) $(SRC_DIR)/*.idl > $(PKGNAME)_init.cc; \
	$(CXX)  $(CPPFLAGS) $(CXXFLAGS) $(INCLUDES)  $(PKGNAME)Staf.cc $(PKGNAME)_init.cc $(ALL_LIBS) -o $(ALL_TAGS)

CDFtoC: $(FILES_C_CDF)

IdlToH: $(FILES_IDH)

#lib : CDFtoC IdlToH $(MY_LIB)
lib :  $(MY_LIB) $(MY_SO)

$(MY_LIB) : $(FILES_O)
	$(AR) $(ARFLAGS) $(MY_LIB) $(addprefix $(OBJ_DIR)/,$(FILES_O))
	touch $(MY_LIB)

$(MY_SO) : $(FILES_O)
	$(CD) $(LIB_DIR);\
	$(SO) $(SOFLAGS) -o $(NEW_MY_SO) $(addprefix $(OBJ_DIR)/,$(FILES_O))
	$(RM) $(MY_SO)
	$(LN) $(NEW_MY_SO) $(MY_SO)
	
$(SRC_GEN_DIR)/%.c : %.cdf
	kuipc -c $(ALL_DEPS) $(SRC_GEN_DIR)/$(STEM).c

$(SRC_GEN_DIR)/%.c : %.y
	cd $(TMP_DIR);\
	$(YACC) $(ALL_DEPS) ; mv y.tab.c $(SRC_GEN_DIR)/$(STEM).c

%.c : %.y
	cd $(TMP_DIR);\
	$(YACC) $(ALL_DEPS) ; mv y.tab.c $(SRC_GEN_DIR)/$(STEM).c

%.o : %.c 
	$(CC)  -c $(CPPFLAGS) $(CFLAGS)    $(INCLUDES) $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o

%.o : $(SRC_GEN_DIR)/%.c 
	$(CC)  -c $(CPPFLAGS) $(CFLAGS)    $(INCLUDES) $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o

%.o : %.cc 
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS)  $(INCLUDES) $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o

%.o : %.F 
	$(FC)  -c $(CPPFLAGS) $(FFLAGS) $(INCLUDES)  $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o

%.o : %.f 
	$(FC)  -c $(CPPFLAGS) $(FFLAGS) $(INCLUDES)  $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o

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
	$(GCC)  -MM -MG -nostdinc -w  $(CPPFLAGS) $(INCLUDES)  $(ALL_DEPS) > $(ALL_TAGS)

$(DEP_DIR)/%.d: %.cc 
	$(RM) $(ALL_TAGS)
	$(GCC)  -MM -MG -nostdinc -w  $(CPPFLAGS) $(INCLUDES)  $(ALL_DEPS) > $(ALL_TAGS)


$(DEP_DIR)/%.d:  %.F
	$(RM)  $(ALL_TAGS)
	$(GCC)  -MM -MG -nostdinc -w -traditional $(CPPFLAGS) $(INCLUDES) -x c $(ALL_DEPS) | sed -e 's/\.F\.o/.o/' > $(ALL_TAGS)

$(DEP_DIR)/%.d:  $(IDL_DIR)/%.idl
	$(RM)  $(ALL_TAGS)
	$(STIC)  -M   $(ALL_DEPS)  | sed -e '1d' > $(ALL_TAGS)

$(INC_GEN_DIR)/%.h:  %.idl
	cd $(INC_GEN_DIR) ; cp $(1ST_DEPS) .;\
	$(STIC)  -f $(INCLUDES)   $(1ST_DEPS)  




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
	@echo MAKE        	:= $(MAKE)
	@echo MAKEFLAGS   	:= $(MAKEFLAGS)
	@echo MAKEFILES   	:= $(MAKEFILES)
	@echo STAF_MAKE_HOME   	:= $(STAF_MAKE_HOME)
	@echo SUFF_IDM    	:= $(SUFF_IDM) 
	@echo SUFF_IDL    	:= $(SUFF_IDL) 
	@echo So    		:= $(So) 
	@echo INP_DIR     	:= $(INP_DIR) 
	@echo OUT_DIR     	:= $(OUT_DIR) 
	@echo PKGNAME     	:= $(PKGNAME) 
	@echo PKG_SDD     	:= $(PKG_SDD)
	@echo SRC_DIR     	:= $(SRC_DIR)
	@echo SRC_GEN_DIR 	:= $(SRC_GEN_DIR) 
	@echo SRM_DIR     	:= $(SRM_DIR) 
	@echo SRM_GEN_DIR 	:= $(SRM_GEN_DIR) 
	@echo INC_DIR     	:= $(INC_DIR)
	@echo IDL_DIR     	:= $(IDL_DIR)
	@echo IDM_DIR     	:= $(IDM_DIR) 
	@echo LIB_DIR     	:= $(LIB_DIR)
	@echo OBJ_DIR     	:= $(OBJ_DIR)
	@echo DEP_DIR     	:= $(DEP_DIR) 
	@echo TMP_DIR     	:= $(TMP_DIR)
	@echo INC_GEN_DIR 	:= $(INC_GEN_DIR) 
	@echo BIN_DIR     	:= $(BIN_DIR) 
	@echo DOIT        	:= $(DOIT)
	@echo OUPUT_DIRS  	:= $(OUPUT_DIRS)
	@echo INPUT_DIRS  	:= $(INPUT_DIRS)
	@echo FILES_IDM   	:= $(FILES_IDM)
	@echo FILES_IDL   	:= $(FILES_IDL)
	@echo FILES_SRC   	:= $(FILES_SRC)
	@echo FILES_SRM   	:= $(FILES_SRM)
	@echo NAMES_EXE   	:= $(NAMES_EXE)
	@echo FILES_EXE   	:= $(FILES_EXE)
	@echo CDF_DIR     	:= $(CDF_DIR)
	@echo FILES_D     	:= $(FILES_D)
	@echo FILES_O     	:= $(FILES_O)
	@echo FILES_CDF   	:= $(FILES_CDF)
	@echo INCLUDES    	:= $(INCLUDES)
	@echo VPATH       	:= $(VPATH)
	@echo ASPS        	:= $(ASPS)
	@echo FILES_A   	:= $(FILES_A)
	@echo OSFID     	:= $(OSFID)
	@echo UNDERSCORES      	:= $(UNDERSCORES)
	@echo NOUNDERSCOR      	:= $(NOUNDERSCOR)
	@echo ONLYOSFID        	:= $(ONLYOSFID)
	@echo MY_LIB        	:= $(MY_LIB)
	@echo MY_SO        	:= $(MY_SO)
	@echo FC        	:= $(FC)
	@echo CC        	:= $(CC)
	@echo CXX        	:= $(CXX)
	echo QWE1        	:= $(QWE1)
	@echo QWE2        	:= $(QWE2)
	@echo INCLUDES        	:= $(INCLUDES)
	@echo STIC        	:= $(STIC)
	@echo L_DIR_THIS       	:= $(L_DIR_THIS)
	@echo L_DIR_SYS       	:= $(L_DIR_SYS)



