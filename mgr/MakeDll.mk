#
ifdef SO_LIB
   OUT_UP  := $(subst / ,,$(dir $(SO_LIB))  )
   OUT_UPP := $(subst / ,,$(dir $(OUT_UP))  )
   OUT_DIR := $(subst / ,,$(dir $(OUT_UPP)) )
endif
ifndef OUT_DIR
  OUT_DIR := $(shell pwd)
endif

TOPDIR := $(shell pwd)


###	Suppress all imlicit rules
.SUFFIXES:

#include $(STAF_ROOT_HOME)/MakeEnv.mk
include $(STAR)/mgr/MakeEnv.mk
include $(STAR)/mgr/MakeArch.mk

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


PKGNAME := $(notdir $(INP_DIR))
ifeq (base,$(PKGNAME))
#LIBRARY := -L$(STAR)/asps/../.$(STAR_SYS)/lib -ldsl -lasu 
#                                                            $(shell cernlib)
endif
INC_DIRS:= $(subst /TEMP,, $(addsuffix TEMP, $(sort $(dir $(wildcard $(OUT_DIR)/.share/*/*.h  $(STAR)/.share/*/*.h)))))
#
#	Define .src dir. If does not exist EMPTY
#
SRC_DIR := $(INP_DIR)
GEN_DIR := $(OUT_DIR)/.share/$(PKGNAME)
ifneq (YES,$(shell if { test -d $(SRC_DIR); } then { echo YES; } fi ))
  SRC_DIR :=
  GEN_DIR :=
endif
ifndef SO_LIB
  SO_LIB := $(LIB_DIR)/lib$(PKGNAME).$(SOEXT)
endif

SYS_DIR := $(OUT_DIR)/.$(STAR_SYS)
LIB_DIR := $(SYS_DIR)/lib
OBJ_DIR := $(SYS_DIR)/obj/$(PKGNAME)
DEP_DIR := $(SYS_DIR)/dep/$(PKGNAME)
TMP_DIR := $(SYS_DIR)/tmp
SRC_DIR := $(INP_DIR)
#.
    check_out   := $(shell test -d $(OUT_DIR) || mkdir -p $(OUT_DIR)) 
    check_sys   := $(shell test -d $(SYS_DIR) || mkdir -p $(SYS_DIR)) 
    check_lib   := $(shell test -d $(LIB_DIR) || mkdir -p $(LIB_DIR))
    check_obj   := $(shell test -d $(OBJ_DIR) || mkdir -p $(OBJ_DIR))
    check_dep   := $(shell test -d $(DEP_DIR) || mkdir -p $(DEP_DIR))
    check_gen   := $(shell test -d $(GEN_DIR) || mkdir -p $(GEN_DIR))

#	Includes
#####INCLUDES := $(addprefix -I,$(wildcard $(UPP_INP_DIR)/*/inc))

INCLUDES := -I$(SRC_DIR) -I$(OUT_DIR)/StRoot/base -I$(STAR)/StRoot/base -I$(STAR)/StRoot/xdf2root \
            -I$(STAR)/StRoot/StChain \
            -I$(ROOTSYS)/include -I$(STAR)/inc -I$(OUT_DIR)/.share/tables -I$(STAR)/.share/tables \
                                               -I$(OUT_DIR)/.share  -I$(STAR)/.share
#-I$(STAR)/asps/staf/inc 
INCL     :=  -I$(GEN_DIR) $(addprefix -I, $(INC_DIRS))
CPPFLAGS += -D__ROOT__

#
#	If NO source , NOTHING to do
#	Skip up to the end
#
FILES_SRC := $(wildcard $(addprefix $(SRC_DIR)/, *.c *.cxx))
ifeq ($(PKGNAME),xdf2root)
FILES_SRC  += $(wildcard $(STAR)/asps/staf/dsl/src/*.c)
INPUT_DIRS := $(STAR)/asps/staf/dsl/src
endif
DOIT := $(strip $(FILES_SRC))
ifneq (,$(DOIT))


DEP_DIR := $(SYS_DIR)/dep/$(PKGNAME)

OUPUT_DIRS := $(LIB_DIR) $(OBJ_DIR) $(DEP_DIR) $(BIN_DIR) $(TMP_DIR) $(GEN_DIR) $(SRC_DIR) 
INPUT_DIRS += $(SRC_DIR) 

# 	Make dirs before make real work. Othervice VPATH does not see
#    	non existing directories
MAKEDIRS := $(shell mkdir -p $(OUPUT_DIRS))

VPATH =  $(INPUT_DIRS) $(OUPUT_DIRS)

FILES_SYM  := $(strip $(wildcard $(addprefix $(SRC_DIR)/, St_Module.cxx )))
FILES_SYT  := $(strip $(wildcard $(addprefix $(SRC_DIR)/, St_Table.cxx )))
FILES_TAB  := $(strip $(wildcard $(addprefix $(SRC_DIR)/, St_*_Table.cxx )))
FILES_MOD  := $(strip $(wildcard $(addprefix $(SRC_DIR)/, St_*_Module.cxx )))
FILES_DAT  := $(strip $(wildcard $(addprefix $(SRC_DIR)/, St_DataSet.cxx )))
FILES_XDF  := $(strip $(wildcard $(addprefix $(SRC_DIR)/, St_XDFFile.cxx )))
FILES_ALL  := $(strip $(wildcard $(SRC_DIR)/St*.cxx ))
FILES_CINT := $(strip $(wildcard $(addprefix $(SRC_DIR)/, St_*Cint.cxx)))
FILES_ST   := $(strip $(FILES_CINT) $(FILES_SYM) $(FILES_SYT) $(FILES_TAB) $(FILES_MOD) $(FILES_DAT))
FILES_ALL  := $(filter-out $(FILES_ST),  $(FILES_ALL))
FILES_ORD  := $(FILES_ALL)
ifdef FILES_SYM
  NAMES_SYM      := $(subst St_,,$(basename $(notdir $(FILES_SYM))))
  FILES_SYM_H    := $(addprefix $(SRC_DIR)/St_,$(addsuffix .h,$(NAMES_SYM)))
  FILES_CINT_SYM := $(addprefix $(GEN_DIR)/St_,$(addsuffix Cint.cxx,$(NAMES_SYM)))
endif

ifdef FILES_SYT
  NAMES_SYT      := $(subst St_,,$(basename $(notdir $(FILES_SYT))))
  FILES_SYT_H    := $(addprefix $(SRC_DIR)/St_,$(addsuffix .h,$(NAMES_SYT)))
  FILES_CINT_SYT := $(addprefix $(GEN_DIR)/St_,$(addsuffix Cint.cxx,$(NAMES_SYT)))
endif

ifdef FILES_TAB
  NAMES_TAB      := $(subst _Table,,$(subst St_,,$(basename $(notdir $(FILES_TAB)))))
  FILES_TAB_H    := $(addprefix $(SRC_DIR)/St_,$(addsuffix _Table.h,$(NAMES_TAB)))
  FILES_CINT_TAB := $(addprefix $(GEN_DIR)/St_,$(addsuffix _TableCint.cxx,$(NAMES_TAB)))
endif

ifdef FILES_MOD
  NAMES_MOD      := $(subst _Module,,$(subst St_,,$(basename $(notdir $(FILES_MOD)))))
  FILES_MOD_H    := $(addprefix $(SRC_DIR)/St_,$(addsuffix _Module.h,$(NAMES_MOD)))
  FILES_CINT_MOD := $(addprefix $(GEN_DIR)/St_,$(addsuffix _ModuleCint.cxx,$(NAMES_MOD)))
endif

ifdef FILES_ORD
  NAMES_ORD      := $(basename $(notdir $(FILES_ORD)))
  FILES_ORD_H    := $(subst .cxx,.h, $(FILES_ORD))
  FILES_CINT_ORD := $(addprefix $(GEN_DIR)/,$(addsuffix Cint.cxx, $(NAMES_ORD)))
endif

LINKDEF := $(GEN_DIR)/LinkDef.h



#	Define the common part of ROOTCINTs
define  COMMON_LINKDEF
	@test -f $(LINKDEF) &&  $(RM) $(LINKDEF);\
	echo "#ifdef __CINT__"                  	>  $(LINKDEF);\
	echo "#pragma link off all globals;"    	>> $(LINKDEF);\
	echo "#pragma link off all classes;"    	>> $(LINKDEF);\
	echo "#pragma link off all functions;"  	>> $(LINKDEF);\
	echo "#pragma link C++ class St_$(STEM)-;"	>> $(LINKDEF);
endef
define  ORD_LINKDEF
	@test -f $(LINKDEF) &&  $(RM) $(LINKDEF);\
	echo "#ifdef __CINT__"                  	>  $(LINKDEF);\
	echo "#pragma link off all globals;"    	>> $(LINKDEF);\
	echo "#pragma link off all classes;"    	>> $(LINKDEF);\
	echo "#pragma link off all functions;"  	>> $(LINKDEF);\
	echo "#pragma link C++ class $(STEM);"  	>> $(LINKDEF);\
	echo "#endif"					>> $(LINKDEF);
endef

FILES_O := $(FILES_SRC) $(FILES_CINT_SYM) $(FILES_CINT_SYT) $(FILES_CINT_TAB) $(FILES_CINT_MOD) $(FILES_ORD) $(FILES_CINT_ORD)
FILES_O := $(addprefix $(OBJ_DIR)/,$(addsuffix .o, $(notdir $(basename $(FILES_O)))))
FILES_O := $(sort $(FILES_O))
FILES_D := $(addsuffix .d, $(addprefix $(DEP_DIR)/,$(basename $(notdir $(FILES_O)))))
ifeq (,$(FILES_O))
all: 
	@echo Nothing to do for $(PKG)
else

MY_SO  := $(SO_LIB)
MY_AR  := $(addsuffix .a, $(basename $(MY_SO)))
#

.PHONY : all  RootCint Libraries  DeleteDirs




all:   RootCint Libraries  DeleteDirs

RootCint : $(FILES_CINT_SYT) $(FILES_CINT_SYM) $(FILES_CINT_TAB) $(FILES_CINT_MOD)


$(FILES_CINT_SYT) : $(GEN_DIR)/St_%Cint.cxx : $(SRC_DIR)/St_%.h $(wildcard  $(STAR)/StRoot/base/*.h) 
	$(COMMON_LINKDEF)
	@echo "#pragma link C++ class table_head_st-!;"	>> $(LINKDEF);
	@echo "#endif"					>> $(LINKDEF);
	@cat $(LINKDEF);
	cd $(GEN_DIR); cp $(1ST_DEPS) .; \
	rootcint -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT $(INCLUDES) $(notdir $(1ST_DEPS)) $(LINKDEF); 


#$(FILES_CINT_SYM) : $(GEN_DIR)/St_%Cint.cxx : $(wildcard $(SRC_DIR)/St_*.h)
$(FILES_CINT_SYM) : $(GEN_DIR)/St_%Cint.cxx : $(SRC_DIR)/St_%.h $(wildcard  $(STAR)/StRoot/base/*.h) 
	$(COMMON_LINKDEF)
	@echo "#pragma link C++ class St_DataSet;"       >> $(LINKDEF);
	@echo "#pragma link C++ enum EModuleTypes;"      >> $(LINKDEF);
	@echo "#endif"					 >> $(LINKDEF);
	@cat $(LINKDEF);
	cd $(GEN_DIR); cp $(1ST_DEPS) .; \
	rootcint -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT $(INCLUDES) -I$(INP_DIR) $(notdir $(1ST_DEPS)) \
        St_DataSet.h $(notdir $(LINKDEF));

$(FILES_CINT_ORD) : $(GEN_DIR)/%Cint.cxx : $(SRC_DIR)/%.h  $(wildcard  $(STAR)/StRoot/base/*.h) 
	$(ORD_LINKDEF)
	@cat $(LINKDEF)
	cd $(GEN_DIR); cp $(1ST_DEPS) .; \
        rootcint -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT $(INCLUDES) $(notdir $(1ST_DEPS)) \
         $(notdir $(LINKDEF));


#$(FILES_CINT_TAB) : 
$(GEN_DIR)/St_%_TableCint.cxx : $(SRC_DIR)/St_%_Table.h $(wildcard  $(STAR)/StRoot/base/*.h)
	$(COMMON_LINKDEF)
	@echo "#pragma link C++ class $(STEM)_st-!;"	>> $(LINKDEF);
	@echo "#endif"					>> $(LINKDEF);
	@cat $(LINKDEF);
	cd $(GEN_DIR); cd $(GEN_DIR); cp $(1ST_DEPS) .;\
	rootcint -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT $(INCLUDES) $(notdir $(1ST_DEPS)) $(notdir $(LINKDEF));

#$(FILES_CINT_MOD) : 
$(GEN_DIR)/St_%_ModuleCint.cxx : $(GEN_DIR)/St_%_Module.h $(STAR)/StRoot/base/St_Module.h
	$(COMMON_LINKDEF)
	@echo "#pragma link C++ global $(STEM);"	>> $(LINKDEF);
	@echo "#endif"					>> $(LINKDEF);
	@cat $(LINKDEF);
	cd $(GEN_DIR);\
        rootcint -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT $(INCLUDES) $(notdir $(1ST_DEPS)) $(notdir $(LINKDEF));
#  $(INCLUDES)

Libraries : $(MY_SO) 


$(MY_SO) : $(FILES_O)
#	$(SO) $(SOFLAGS) -o $(SO_LIB) $(FILES_O) $(LIBRARY)
ifneq ($(EMPTY),$(findstring $(STAR_HOST_SYS),sgi_52 sgi_53 sgi_62 sgi_63 sgi_64))
	cd $(OBJ_DIR); $(AR) $(ARFLAGS) $(MY_AR) *.o; \
        $(SO) $(SOFLAGS) -Wl,-nltgot,134 -o $(SO_LIB) -all $(MY_AR) $(LIBRARY)
#        $(RM) OBJ_LIST so_*; \
#        ls $(OBJ_DIR) > OBJ_LIST;\
#        $(LD) $(LDFLAGS) -o $(ALL_TAGS) -objectlist OBJ_LIST $(LIBRARY)
else
ifneq ($(EMPTY),$(findstring $(STAR_HOST_SYS),hp_ux102))
	cd $(OBJ_DIR); $(SO) $(SOFLAGS) -o $(SO_LIB) $(notdir $(FILES_O))  $(LIBRARY)
else
	$(SO) $(SOFLAGS) -o $(SO_LIB) $(FILES_O)  $(LIBRARY)
endif
endif

#_________________dependencies_____________________________
ifneq (, $(strip $(FILES_D))) 
include $(FILES_D)
endif                               #

$(OBJ_DIR)/%.o : %.c
	$(CC)  -c $(CPPFLAGS) $(CFLAGS)    $(INCLUDES) $(INCL) $(1ST_DEPS) -o $(ALL_TAGS)

$(OBJ_DIR)/%.o : %.cxx 
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS)  $(INCLUDES) $(INCL) $(1ST_DEPS) -o $(ALL_TAGS)

$(DEP_DIR)/%.d: %.c 
	$(RM) $(ALL_TAGS)
	$(GCC) $(MKDEPFLAGS) $(CPPFLAGS) $(INCLUDES) $(INCL)  $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.o/$(subst .,\.,$(subst /,\/,$(OBJ_DIR)/$(STEM).o)) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)

$(DEP_DIR)/%.d: %.cxx
	$(RM) $(ALL_TAGS)
	$(GCC) $(MKDEPFLAGS) $(CPPFLAGS) $(INCLUDES) $(INCL)  $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.o/$(subst .,\.,$(subst /,\/,$(OBJ_DIR)/$(STEM).o)) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)



endif
DeleteDirs :
	$(RMDIR)  $(TMP_DIR)

clean :
	-$(RMDIR)  $(OUPUT_DIRS)


endif # end of DoIt

test: 
	@echo MAKE        := $(MAKE)
	@echo MAKEFLAGS   := $(MAKEFLAGS)
	@echo MAKEFILES   := $(MAKEFILES)
	@echo OUT_DIR     := $(OUT_DIR)
	@echo OUT_UP      := $(OUT_UP)
	@echo OUT_UPP     := $(OUT_UPP)
	@echo INP_DIR     := $(INP_DIR) 
	@echo UPP_INP_DIR := $(UPP_INP_DIR)
	@echo PKGNAME     := $(PKGNAME) 
	@echo SRC_DIR     := $(SRC_DIR)
	@echo GEN_DIR := $(GEN_DIR) 
	@echo LIB_DIR     := $(LIB_DIR)
	@echo OBJ_DIR     := $(OBJ_DIR)
	@echo DEP_DIR     := $(DEP_DIR) 
	@echo TMP_DIR     := $(TMP_DIR)
	@echo SRC_DIR := $(SRC_DIR) 
	@echo BIN_DIR     := $(BIN_DIR) 
	@echo DOIT        := $(DOIT)
	@echo OUPUT_DIRS  := $(OUPUT_DIRS)
	@echo INPUT_DIRS  := $(INPUT_DIRS)
	@echo FILES_SRC   := $(FILES_SRC)
	@echo FILES_D     := $(FILES_D)
	@echo FILES_O     := $(FILES_O)
	@echo INCLUDES    := $(INCLUDES)
	@echo TOPDIR      := $(TOPDIR)
	@echo VPATH       := $(VPATH)
	@echo OSFID     := $(OSFID)

	@echo FILES_ORD := $(FILES_ORD)
	@echo FILES_SYM := $(FILES_SYM)
	@echo FILES_SYT := $(FILES_SYT)
	@echo FILES_TAB := $(FILES_TAB)
	@echo FILES_MOD := $(FILES_MOD) 

	@echo NAMES_ORD := $(NAMES_ORD) 
	@echo NAMES_SYT := $(NAMES_SYT) 
	@echo NAMES_SYM := $(NAMES_SYM) 
	@echo NAMES_TAB := $(NAMES_TAB)
	@echo NAMES_MOD := $(NAMES_MOD)


	@echo FILES_CINT_ORD := $(FILES_CINT_ORD) 
	@echo FILES_CINT_SYT := $(FILES_CINT_SYT) 
	@echo FILES_CINT_SYM := $(FILES_CINT_SYM) 
	@echo FILES_CINT_TAB := $(FILES_CINT_TAB) 
	@echo FILES_CINT_MOD := $(FILES_CINT_MOD)

	@echo FILES_ORD_H := $(FILES_ORD_H) 
	@echo FILES_SYT_H := $(FILES_SYT_H) 
	@echo FILES_SYM_H := $(FILES_SYM_H) 
	@echo FILES_TAB_H := $(FILES_TAB_H) 
	@echo FILES_MOD_H := $(FILES_MOD_H)

