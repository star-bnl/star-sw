# $Id: MakeDll.mk,v 1.51 1999/01/28 16:49:58 fisyak Exp $
# $Log: MakeDll.mk,v $
# Revision 1.51  1999/01/28 16:49:58  fisyak
# remove copying of h-files for rootcint
#
# Revision 1.50  1999/01/27 23:59:50  fisyak
# More templates
#
# Revision 1.49  1999/01/27 23:46:27  fisyak
# Add Templates
#
# Revision 1.48  1999/01/25 23:49:13  fisyak
# Add MAKEFLAG
#
# Revision 1.47  1999/01/21 02:15:30  fisyak
# New StChain w/o automatical streamer generation
#
# Revision 1.45  1999/01/14 13:56:40  fisyak
# Add Victors MakeFun.mk, Add StMagF
#
# Revision 1.44  1999/01/04 16:24:38  fisyak
# Add TGeant3
#
# Revision 1.43  1999/01/01 02:38:55  fisyak
# restore GEN_DIR in OUTPUT_DIRS
#
# Revision 1.42  1999/01/01 01:46:09  fisyak
# Clean up include path
#
# Revision 1.41  1998/12/17 18:01:16  fisyak
# remove duplicated includes
#
# Revision 1.40  1998/12/12 00:58:33  fisyak
# remove STAF
#
# Revision 1.39  1998/12/04 01:17:30  fisyak
# fix for fortran source in StRoot
#
# Revision 1.38  1998/12/03 23:39:55  fisyak
# Add geant to StRoot
#
# Revision 1.37  1998/12/02 20:01:50  fisyak
# More NT
#
# Revision 1.36  1998/12/01 01:53:01  fisyak
# Merge with NT
#
# Revision 1.35  1998/11/16 02:09:41  fisyak
# replace ROOTSYS/include to ROOTSYS/src
#
# Revision 1.34  1998/11/16 01:26:45  fisyak
# New merging with NT
#
# Revision 1.33  1998/11/15 21:12:57  fisyak
# fix shared libraries versioning for St_Root
#
#
ifdef SILENT
.SILENT:
endif       


ifdef SO_LIB
   OUT_UP  := $(subst / ,,$(dir $(SO_LIB))  )
   OUT_UPP := $(subst / ,,$(dir $(OUT_UP))  )
   OUT_DIR := $(subst / ,,$(dir $(OUT_UPP)) )
endif
ifndef OUT_DIR
  OUT_DIR := $(shell pwd)
endif


###	Suppress all imlicit rules
.SUFFIXES:

#
ifndef STAR_MAKE_HOME
  STAR_MAKE_HOME := $(STAR)/mgr
endif

ASU_MALLOC_OFF :=YES

include $(STAR_MAKE_HOME)/MakeEnv.mk
include $(STAR_MAKE_HOME)/MakeArch.mk

#
#	INP_DIR & OUT_DIR could be declared in invoking
#
ifndef INP_DIR
  override INP_DIR := $(CWD)
endif

inp_dir = $(word 2,$(subst :, ,$(INP_DIR)))
ifneq (,$(inp_dir))
  override INP_DIR := $(inp_dir)
endif

ifeq (,$(strip $(filter /%,$(INP_DIR))))
  override INP_DIR := $(CWD)/$(INP_DIR)
endif

#	ROOT DIR (Not a Rene ROOT)
PAMS    := $(findstring /pams,$(INP_DIR))
ifndef PAMS
  PAMS    := $(findstring /StRoot,$(INP_DIR))
endif
ifndef PAMS
  PAMS    := $(findstring /.share,$(INP_DIR))
endif
ROOT_DIR:= $(word 1,$(subst $(PAMS), ,$(INP_DIR)))


PKGNAME := $(notdir $(INP_DIR))

#
#	Define .src dir. If does not exist EMPTY
#
SRC_DIR := $(INP_DIR)
GEN_DIR := $(OUT_DIR)/.share/$(PKGNAME)

ifndef SO_LIB
ifdef NT
  SO_LIB := $(BIN_DIR)/lib$(PKGNAME).$(SOEXT)
else
  SO_LIB := $(LIB_DIR)/lib$(PKGNAME).$(SOEXT)
endif
endif

SYS_DIR := $(OUT_DIR)/.$(STAR_HOST_SYS)
LIB_DIR := $(SYS_DIR)/lib

ifdef NT
BIN_DIR := $(SYS_DIR)/bin
endif

OBJ_DIR := $(SYS_DIR)/obj/$(PKGNAME)
STAR_OBJ_DIR := $(STAR)/obj/$(PKGNAME)
DEP_DIR := $(SYS_DIR)/dep/$(PKGNAME)
ifndef NT
TMP_DIR := $(SYS_DIR)/tmp
else
TMP_DIR := $(TEMP)/tmp
endif # /NT/

SRC_DIR := $(INP_DIR)
#.
ifndef NT
    check_out   := $(shell test -d $(OUT_DIR) || mkdir -p $(OUT_DIR)) 
    check_sys   := $(shell test -d $(SYS_DIR) || mkdir -p $(SYS_DIR)) 
    check_lib   := $(shell test -d $(LIB_DIR) || mkdir -p $(LIB_DIR))
    check_obj   := $(shell test -d $(OBJ_DIR) || mkdir -p $(OBJ_DIR))
    check_dep   := $(shell test -d $(DEP_DIR) || mkdir -p $(DEP_DIR))
    check_gen   := $(shell test -d $(GEN_DIR) || mkdir -p $(GEN_DIR))

else #/* NT */
    check_out   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(OUT_DIR))))
    check_sys   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(SYS_DIR))))
    check_lib   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(LIB_DIR))))
    check_obj   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(OBJ_DIR))))
#    check_dep   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(DEP_DIR))))
#    check_neg   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(GEN_DIR))))
endif #/* NT */
#	Includes

# 	Define internal and external includes dirs
INC_NAMES := $(addprefix StRoot/,base StChain xdf2root) StRoot .share .share/tables pams inc StarClassLibrary/include
INC_DIRS  := $(wildcard $(SRC_DIR) $(SRC_DIR)/include)
INC_DIRS  += $(strip $(wildcard $(addprefix $(ROOT_DIR)/,$(INC_NAMES)))) 
ifneq ($(ROOT_DIR),$(STAR))
INC_DIRS  += $(strip $(wildcard $(addprefix $(STAR)/,$(INC_NAMES))))
endif
INC_DIRS  +=  $(STAF_UTILS_INCS) $(CERN_ROOT)/include $(ROOTSYS)/src

INCINT := $(INC_DIRS)
ifdef NT
INC_DIRS := $(INC_DIRS) $(SUNRPC)
endif
INC_DIRS += $(CERN_ROOT)/include
INCLUDES := $(addprefix -I,$(INC_DIRS))
INCINT   := $(addprefix -I,$(INCINT))

ifdef NT
 INCLUDES := $(addsuffix I-,$(INCLUDES))
 INCINT   := $(addsuffix I-,$(INCINT))
 INCLUDE :=  $(INCLUDE)$(subst  -I,;,$(INCLUDES) $(INCINT))
 INCLUDE := $(subst  I- ;,;,$(INCLUDE))
 INCLUDE := $(subst  I-,,$(INCLUDE))
 INCLUDES :=
 INCINT   :=
endif


CPPFLAGS += -D__ROOT__ 
ifneq (,$(findstring $(STAR_SYS),sun4x_55 sun4x_56))
CXXFLAGS +=-ptr$(OBJ_DIR)
endif
#
#	If NO source , NOTHING to do
#	Skip up to the end
#
FILES_SRC := $(wildcard $(addprefix $(SRC_DIR)/, *.c *.cxx *.cc))
FILES_SRC += $(wildcard $(addprefix $(SRC_DIR)/src/, *.c *.cxx *.cc))
ifneq ($(GEN_DIR),$(SRC_DIR))
FILES_SRC += $(wildcard $(addprefix $(SRC_DIR)/, *.f *.F *.g))
endif
FILES_SRC := $(filter-out %/.share/%/*.F, $(FILES_SRC))
FILES_SRC := $(filter-out %/.share/%/*.f, $(FILES_SRC))
FILES_SRC := $(filter-out %/.share/%/*.c, $(FILES_SRC))
FILES_SRC := $(filter-out %/.share/%/*.g, $(FILES_SRC))
FILES_SRC := $(filter-out %_init.cc %_i.cc, $(FILES_SRC)) 
ifeq ($(PKGNAME),xdf2root)
ifndef NT
  FILES_SRC  += $(wildcard $(STAR)/asps/staf/dsl/src/*.c)
  INPUT_DIRS := $(STAR)/asps/staf/dsl/src
else
  LIBS := $(LIBS) $(AFS_RHIC)/star/packages/dsl/intel_wnt/lib/dsl.lib 
endif #/* NT */
endif

DOIT := $(strip $(FILES_SRC))
ifneq (,$(DOIT))


DEP_DIR := $(SYS_DIR)/dep/$(PKGNAME)

OUTPUT_DIRS := $(LIB_DIR) $(OBJ_DIR) $(DEP_DIR) $(BIN_DIR) $(TMP_DIR) $(GEN_DIR) 
#                                                                      $(SRC_DIR) 
INPUT_DIRS += $(SRC_DIR) $(SRC_DIR)/src
INPUT_DIRS += $(ROOT_DIR)

# 	Make dirs before make real work. Othervice VPATH does not see
#    	non existing directories
ifndef NT
MAKEDIRS := $(shell mkdir -p $(OUTPUT_DIRS))
else #/* NT */
MAKEDIRS := $(shell $(MKDIR) $(subst /,\,$(OUTPUT_DIRS))) 
endif# /* NT */

VPATH =  $(INPUT_DIRS) $(OUTPUT_DIRS)

FILES_SYM  := $(wildcard $(SRC_DIR)/St_Module.cxx)
FILES_SYT  := $(wildcard $(SRC_DIR)/St_Table.cxx)
FILES_TAB  := $(wildcard $(SRC_DIR)/St_*_Table.cxx)
FILES_MOD  := $(wildcard $(SRC_DIR)/St_*_Module.cxx)
FILES_DAT  := $(wildcard $(SRC_DIR)/St_DataSet.cxx)
FILES_XDF  := $(wildcard $(SRC_DIR)/St_XDFFile.cxx)
FILES_G3   := $(wildcard $(SRC_DIR)/TGeant3.cxx)
FILES_CHAIN:= $(wildcard $(SRC_DIR)/StChain.cxx)
FILES_ALL  := $(filter-out %Cint.cxx,$(wildcard $(SRC_DIR)/*.cxx))
FILES_ST   := $(FILES_SYM) $(FILES_SYT) $(FILES_TAB) $(FILES_MOD) $(FILES_G3) $(FILES_CHAIN)
#                                                                 $(FILES_DAT)
FILES_ALL  := $(sort $(filter-out $(FILES_ST),$(FILES_ALL)))
FILES_ORD  := $(FILES_ALL)
ifdef FILES_G3
  NAMES_G3      := $(basename $(notdir $(FILES_G3)))
  FILES_G3_H    := $(addprefix $(SRC_DIR)/,$(addsuffix .h,$(NAMES_G3)))
  FILES_CINT_G3 := $(addprefix $(GEN_DIR)/,$(addsuffix Cint.cxx,$(NAMES_G3)))
endif
ifdef FILES_CHAIN
  NAMES_CHAIN      := $(basename $(notdir $(FILES_CHAIN)))
  FILES_CHAIN_H    := $(addprefix $(SRC_DIR)/,$(addsuffix .h,$(NAMES_CHAIN)))
  FILES_CINT_CHAIN := $(addprefix $(GEN_DIR)/,$(addsuffix Cint.cxx,$(NAMES_CHAIN)))
endif

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
  NAMES_TAB      := $(strip $(subst _Table,,$(subst St_,,$(basename $(notdir $(FILES_TAB))))))
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

#	Define the common part of rootcint s
ifndef NT
define  COMMON_LINKDEF
	@test -f $(LINKDEF) &&  $(RM) $(LINKDEF);\
	echo "#ifdef __CINT__"                  	>  $(LINKDEF);\
	echo "#pragma link off all globals;"    	>> $(LINKDEF);\
	echo "#pragma link off all classes;"    	>> $(LINKDEF);\
	echo "#pragma link off all functions;"  	>> $(LINKDEF);\
	echo "#pragma link C++ class St_$(STEM)-;"	>> $(LINKDEF);
endef

else
define  COMMON_LINKDEF
	@echo #ifdef __CINT__                   	>  $(LINKDEF)&\
	@echo #pragma link off all globals; 	    	>> $(LINKDEF)&\
	@echo #pragma link off all classes;	    	>> $(LINKDEF)&\
	@echo #pragma link off all functions;	  	>> $(LINKDEF)&\
	@echo #pragma link C++ class St_$(STEM)-;	>> $(LINKDEF)
endef
endif

ifndef NT
ifeq ($(PKGNAME),StRootEvent)
define  ORD_LINKDEF
	@test -f $(LINKDEF) &&  $(RM) $(LINKDEF);\
	echo "#ifdef __CINT__"                  	>  $(LINKDEF);\
	echo "#pragma link off all globals;"    	>> $(LINKDEF);\
	echo "#pragma link off all classes;"    	>> $(LINKDEF);\
	echo "#pragma link off all functions;"  	>> $(LINKDEF);\
	echo "#pragma link C++ class $(STEM);"  	>> $(LINKDEF);\
	echo "#endif"					>> $(LINKDEF);
endef
else
define  ORD_LINKDEF
	@test -f $(LINKDEF) &&  $(RM) $(LINKDEF);\
	echo "#ifdef __CINT__"                  	>  $(LINKDEF);\
	echo "#pragma link off all globals;"    	>> $(LINKDEF);\
	echo "#pragma link off all classes;"    	>> $(LINKDEF);\
	echo "#pragma link off all functions;"  	>> $(LINKDEF);\
	echo "#pragma link C++ class $(STEM);"  	>> $(LINKDEF);\
	echo "#endif"					>> $(LINKDEF);
endef
endif

else
define  ORD_LINKDEF
	@echo #ifdef __CINT__                  	>  $(LINKDEF)&\
	@echo #pragma link off all globals;    	>> $(LINKDEF)&\
	@echo #pragma link off all classes;    	>> $(LINKDEF)&\
	@echo #pragma link off all functions;  	>> $(LINKDEF)&\
	@echo #pragma link C++ class $(STEM);  	>> $(LINKDEF)&\
	@echo #endif"				>> $(LINKDEF)

endef
endif

FILES_CINT := $(FILES_CINT_SYM) $(FILES_CINT_G3) $(FILES_CINT_CHAIN)  $(FILES_CINT_SYT) $(FILES_CINT_TAB) $(FILES_CINT_MOD)  $(FILES_CINT_ORD)
FILES_CINTH:= $(subst .cxx,.h,$(FILES_CINT))
FILES_O := $(addprefix $(OBJ_DIR)/,$(addsuffix .$(O), $(notdir $(basename $(FILES_SRC) $(FILES_ORD) $(FILES_CINT)))))
FILES_O := $(sort $(FILES_O))
STAR_FILES_O := $(wildcard $(STAR_OBJ_DIR)/*.$(O))
FILTER  := $(addprefix %/,$(notdir $(FILES_O)))
STAR_FILES_O := $(filer-out $(FILTER),$(STAR_FILES_O))
FILES_D := $(addsuffix .d, $(addprefix $(DEP_DIR)/,$(basename $(notdir $(FILES_O)))))
FILES_DCINT := $(addsuffix .d, $(addprefix $(DEP_DIR)/,$(basename $(notdir $(FILES_CINT)))))

ifeq (,$(FILES_O))
all: 
	@echo Nothing to do for $(PKG)
else

MY_SO  := $(SO_LIB)
ifndef NT
  QWE    := $(strip $(wildcard $(MY_SO).*))
  SL_NEW := $(MY_SO).1000
ifneq (,$(QWE))
  NQWE := $(words $(QWE))
  QWE  := $(word $(NQWE),$(QWE))
  QWE  := $(subst $(MY_SO).,,$(QWE))
  QWE  := $(shell expr $(QWE) + 1)
  SL_NEW := $(MY_SO).$(QWE)
endif
else #/* NT */
  SL_NEW := $(MY_SO)
endif
MY_AR  := $(addsuffix .a, $(basename $(MY_SO)))
#

.PHONY : all  RootCint Libraries  DeleteDirs

all:   RootCint Libraries  DeleteDirs


RootCint : $(FILES_CINT_SYT) $(FILES_CINT_SYM) $(FILES_CINT_G3) $(FILES_CINT_CHAIN) $(FILES_CINT_TAB) $(FILES_CINT_MOD)

$(FILES_CINT_SYT) : $(GEN_DIR)/St_%Cint.cxx : $(SRC_DIR)/St_%.h 
	$(COMMON_LINKDEF)
	@echo "#pragma link C++ class table_head_st-!;"	>> $(LINKDEF);
	@echo "#endif"					>> $(LINKDEF);
	@$(CAT) $(LINKDEF);
ifndef NT
#$(CP) $(1ST_DEPS) .; \
	cd $(GEN_DIR); \
	$(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT -D__ROOT__ $(INCINT) $(notdir $(1ST_DEPS)) $(LINKDEF)
else
	pushd $(subst /,\\,$(subst \,/,$(GEN_DIR) & $(CP) $(1ST_DEPS) . & )) \
	$(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT -D__ROOT__ $(INCINT) $(notdir $(1ST_DEPS)) $(LINKDEF)
endif

$(FILES_CINT_G3) : $(GEN_DIR)/%Cint.cxx : $(SRC_DIR)/%.h 
	$(COMMON_LINKDEF)
	@echo "#pragma  link C++ global geant;"	 >> $(LINKDEF);
	@echo "#pragma  link C++ global cquest;" >> $(LINKDEF);
	@echo "#pragma  link C++ global clink;"	 >> $(LINKDEF);
	@echo "#pragma  link C++ global ccuts;"	 >> $(LINKDEF);
	@echo "#pragma  link C++ global cflag;"	 >> $(LINKDEF);
	@echo "#pragma  link C++ global ckine;"	 >> $(LINKDEF);
	@echo "#pragma  link C++ global cking;"	 >> $(LINKDEF);
	@echo "#pragma  link C++ global cmate;"	 >> $(LINKDEF);
	@echo "#pragma  link C++ global ctmed;"	 >> $(LINKDEF);
	@echo "#pragma  link C++ global ctrak;"	 >> $(LINKDEF);
	@echo "#pragma  link C++ global ctpol;"	 >> $(LINKDEF);
	@echo "#pragma  link C++ global cvolu;"	 >> $(LINKDEF);
	@echo "#pragma  link C++ global cnum;"	 >> $(LINKDEF);
	@echo "#pragma  link C++ global csets;"	 >> $(LINKDEF);
	@echo "#pragma  link C++ class  TGeant3;"	 >> $(LINKDEF);
	@echo "#endif"					 >> $(LINKDEF);
	@$(CAT) $(LINKDEF);
	cd $(GEN_DIR); \
	$(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT -D__ROOT__ $(INCINT)  $(notdir $(1ST_DEPS)) \
        $(notdir $(LINKDEF))

$(FILES_CINT_CHAIN) : $(GEN_DIR)/%Cint.cxx : $(SRC_DIR)/%.h 
	@test -f $(LINKDEF) &&  $(RM) $(LINKDEF);\
	echo "#ifdef __CINT__"                  	>  $(LINKDEF);\
	echo "#pragma link off all globals;"    	>> $(LINKDEF);\
	echo "#pragma link off all classes;"    	>> $(LINKDEF);\
	echo "#pragma link off all functions;"  	>> $(LINKDEF);\
	echo "#pragma link C++ class $(STEM)-;"  	>> $(LINKDEF);\
	echo "#endif"					>> $(LINKDEF);
	@$(CAT) $(LINKDEF);
ifndef NT
	cd $(GEN_DIR); \
	$(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT -D__ROOT__ $(INCINT)  $(notdir $(1ST_DEPS)) \
        St_DataSet.h $(notdir $(LINKDEF))
else
	pushd $(subst /,\\,$(subst \,/,$(GEN_DIR) & $(CP) $(1ST_DEPS) . & )) \
	$(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT -D__ROOT__ $(INCINT)  $(notdir $(1ST_DEPS)) \
        St_DataSet.h $(notdir $(LINKDEF))
endif
$(FILES_CINT_SYM) : $(GEN_DIR)/St_%Cint.cxx : $(SRC_DIR)/St_%.h 
	$(COMMON_LINKDEF)
#	@echo "#pragma link C++ class St_DataSet;"       >> $(LINKDEF);
	@echo "#pragma link C++ enum EModuleTypes;"      >> $(LINKDEF);
	@echo "#endif"					 >> $(LINKDEF);
	@$(CAT) $(LINKDEF);
ifndef NT
	cd $(GEN_DIR); \
	$(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT -D__ROOT__ $(INCINT)  $(notdir $(1ST_DEPS)) \
        St_DataSet.h $(notdir $(LINKDEF))
else
	pushd $(subst /,\\,$(subst \,/,$(GEN_DIR) & $(CP) $(1ST_DEPS) . & )) \
	$(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT -D__ROOT__ $(INCINT)  $(notdir $(1ST_DEPS)) \
        St_DataSet.h $(notdir $(LINKDEF))
endif
$(FILES_CINT_ORD) : $(GEN_DIR)/%Cint.cxx : $(SRC_DIR)/%.h    
	$(ORD_LINKDEF)
	@$(CAT) $(LINKDEF)
ifndef NT
	cd $(GEN_DIR); \
        $(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT -D__ROOT__ $(INCINT) $(notdir $(1ST_DEPS)) \
        $(notdir $(LINKDEF))
else
	pushd $(subst /,\\,$(subst \,/,$(GEN_DIR) & $(CP) $(1ST_DEPS) . & )) \
        $(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT -D__ROOT__ $(INCINT) $(notdir $(1ST_DEPS)) \
	 $(notdir $(LINKDEF))
endif
#$(FILES_CINT_TAB) : 
$(GEN_DIR)/St_%_TableCint.cxx : $(SRC_DIR)/St_%_Table.h 
	$(COMMON_LINKDEF)
	@echo "#pragma link C++ class $(STEM)_st-!;"	>> $(LINKDEF);
	@echo "#endif"					>> $(LINKDEF);
	@$(CAT) $(LINKDEF);
ifndef NT
	cd $(GEN_DIR); \
	$(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT -D__ROOT__ $(INCINT) $(notdir $(1ST_DEPS)) $(notdir $(LINKDEF))
else
	pushd $(subst /,\\,$(subst \,/,$(GEN_DIR) & $(CP) $(1ST_DEPS) . & )) \
	$(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT -D__ROOT__ $(INCINT) $(notdir $(1ST_DEPS)) $(notdir $(LINKDEF))
endif

#$(FILES_CINT_MOD) : 
$(GEN_DIR)/St_%_ModuleCint.cxx : $(GEN_DIR)/St_%_Module.h 
	$(COMMON_LINKDEF)
	@echo "#pragma link C++ global $(STEM);"	>> $(LINKDEF);
	@echo "#endif"					>> $(LINKDEF);
	@$(CAT) $(LINKDEF);
ifndef NT
	cd $(GEN_DIR); \
        $(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT -D__ROOT__ $(INCINT) $(notdir $(1ST_DEPS)) $(notdir $(LINKDEF))
else
	pushd $(subst /,\\,$(subst \,/,$(GEN_DIR) & )) \
        $(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c -DROOT_CINT -D__ROOT__ $(INCINT) $(notdir $(1ST_DEPS)) $(notdir $(LINKDEF))
endif
#  $(INCLUDES)

Libraries : $(MY_SO) 


ifndef NT

$(MY_SO) : $(FILES_O) $(wildcard $(OBJ_DIR)/Templates.DB/*.o)
	cd $(OBJ_DIR); \
        $(SO) $(SOFLAGS) $(SoOUT)$(SL_NEW) $(ALL_DEPS) $(LIBRARY); \
        $(RM) $(MY_SO); $(LN) $(SL_NEW) $(MY_SO)
else # NT
ifdef MY_SO
MY_SOLIB := $(subst /bin/,/lib/,$(MY_SO))
endif 
MY_DLLNAME  := $(notdir $(basename $(MY_SO)))
MY_EXPLIB   := $(addsuffix .exp, $(basename $(MY_SOLIB)))
MY_IMPLIB   := $(addsuffix .lib, $(basename $(MY_SOLIB)))
MY_DEF      := $(addsuffix .def, $(basename $(MY_SOLIB)))
MY_PDB      := $(addsuffix .pdb, $(basename $(MY_SOLIB)))

LIBS        += $(ROOTSYS)/lib/*.lib
LIBS        += $(filter-out $(MY_IMPLIB),$(wildcard $(LIB_DIR)/*.lib))
LIBS        += $(LIB_PKG)

LIBRARY     :=  mathlib.lib packlib.lib $(CLIBS) $(FLIBS)

$(MY_DEF): $(FILES_O)
	@echo MY_DLLNAME = $(MY_DLLNAME)
	@echo MY_SO = $(MY_SO)
	BINDEXPLIB.exe $(MY_DLLNAME)  $(OBJ_DIR)/St*.$(O) > $(subst /,\\,$(subst \,/,$(MY_DEF) ))

$(MY_EXPLIB): $(MY_DEF)
	$(AR) $(ARFLAGS) $(subst /,\\,$(subst \,/,$(LOUT)$(MY_IMPLIB) $(OBJ_DIR)/St*.$(O) -def:$(MY_DEF)))

$(MY_SO) : $(MY_EXPLIB)
	@echo LIB_DIR   : = $(LIB_DIR)
	@echo MY_SO     : = $(MY_SO)
	@echo MY_EXPLIB : = $(MY_EXPLIB)
	@echo LIBS      : = $(LIBS)
	@echo LIBRARY   : = $(LIBRARY)
	@echo MY_PDB    : = $(MY_PDB)
	@echo MY_SOLIB    : = $(MY_SOLIB)
	$(SO) $(SOFLAGS) $(subst /,\\,$(subst \,/,$(SoOUT)$(MY_SO) $(sort $(OBJ_DIR)/*.$(O)  $(STAR_OBJ_DIR)/*.$(O) $(SYS_DIR)/obj/base/St_staf_dummies.$(O)) $(LIBS) $(LIBRARY) -PDB:$(MY_PDB))) 
#	$(SO) $(SOFLAGS) $(subst /,\\,$(subst \,/,$(SoOUT)$(MY_SO) $(FILES_O)  $(STAR_FILES_O) $(LIBS) $(LIBRARY) -PDB:$(MY_PDB)))
#	$(SO) $(SOFLAGS) $(subst /,\\,$(subst \,/,$(SoOUT)$(SL_NEW) $(FILES_O) $(LIBS) $(LIBRARY) -PDB:$(MY_PDB)))
#        $(RM) $(MY_SO)
endif
	@echo "           Shared library " $(MY_SO) " has been created"   
$(OBJ_DIR)/%.$(O) : %.c
ifdef NT
	$(CC)  -c $(subst \,/,$(CPPFLAGS) $(CFLAGS) $(INCLUDES) -Fd$(OBJ_DIR)\$(PKGNAME) $(CINP)$(1ST_DEPS) $(COUT)$(ALL_TAGS))
else
	$(CC)  -c $(CPPFLAGS) $(CFLAGS) $(INCLUDES) $(CINP)$(1ST_DEPS) $(COUT)$(ALL_TAGS)
endif

$(OBJ_DIR)/%.$(O) : %.cc
ifdef NT
	$(CXX) -c $(subst \,/,$(CPPFLAGS) $(CXXFLAGS)  $(INCLUDES) $(CXXINP)$(1ST_DEPS) $(COUT)$(ALL_TAGS) -Fd$(OBJ_DIR)\$(PKGNAME))
else
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS) $(INCLUDES) $(CXXINP)$(1ST_DEPS) $(COUT)$(ALL_TAGS)
endif
$(OBJ_DIR)/%.$(O) : %.cxx 
ifdef NT
	$(CXX) -c $(subst \,/,$(CPPFLAGS) $(CXXFLAGS)  $(INCLUDES) $(CXXINP)$(1ST_DEPS) $(COUT)$(ALL_TAGS)  -Fd$(OBJ_DIR)\$(PKGNAME))
else
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS) $(INCLUDES) $(CXXINP)$(1ST_DEPS) $(COUT)$(ALL_TAGS)
endif
$(FILES_OG): $(OBJ_DIR)/%.$(O):%.g $(GEN_DIR)/geant3.def
	$(CP)$(1ST_DEPS) $(GEN_DIR); cd $(GEN_DIR); $(GEANT3) $(1ST_DEPS) -o  $(GEN_DIR)/$(STEM).F
	$(FOR72) $(subst /,\\,$(subst \,/,$(CPPFLAGS) $(FFLAGS) -c $(GEN_DIR)/$(STEM).F  $(FOUT)$(ALL_TAGS)))
$(FILES_OBJ) $(FILES_ORJ) $(FILES_OTJ): $(OBJ_DIR)/%.$(O): %.cxx
	$(CXX) $(CXXFLAGS) $(CXXOPT) $(subst \,/, $(CPPFLAGS)  -c $(CXXINP)$(1ST_DEPS) $(COUT)$(OBJ_DIR)/$(STEM).$(O) -Fd$(OBJ_DIR)/$(DOMAIN)
ifndef NT
$(GEN_DIR)/geant3.def: $(STAR)/asps/agi/gst/geant3.def
	test -h $(GEN_DIR)/geant3.def || $(RM)  $(GEN_DIR)/geant3.def
	test -h $(GEN_DIR)/geant3.def || ln -s $(STAR)/asps/agi/gst/geant3.def  $(GEN_DIR)/geant3.def 
$(OBJ_DIR)/%.o:%.g $(GEN_DIR)/geant3.def
	cp $(1ST_DEPS) $(GEN_DIR); cd $(GEN_DIR); $(GEANT3) $(1ST_DEPS) -o  $(GEN_DIR)/$(STEM).F
	$(FOR72)  $(CPPFLAGS) $(FFLAGS) -c $(GEN_DIR)/$(STEM).F  -o  $(ALL_TAGS)
$(OBJ_DIR)/%.o: %.F
	$(FC)  $(CPPFLAGS)  $(INCLUDES) $(FFLAGS) $(FEXTEND)   -c $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o
	$(AR) $(ARFLAGS) $(LIB_PKG) $(OBJ_DIR)/$(STEM).o; $(RM) $(OBJ_DIR)/$(STEM).o
endif #/* NT */
$(OBJ_DIR)/%.o: %.cdf
	$(KUIPC) $(KUIPC_FLAGS) $(1ST_DEPS) $(GEN_DIR)/$(STEM).c
$(FILES_DCINT): $(DEP_DIR)/%Cint.d: %.h 
ifdef NT
	@echo $(FILES_DCINT) $<
endif #/* NT */
	$(RM) $(ALL_TAGS)
	$(MAKEDEPEND)  $(CPPFLAGS) $(INCLUDES) -x c $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.h\.$(O)/$(subst .,\.,$(subst /,\/,$(GEN_DIR)/$(STEM)Cint.cxx)) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)

$(DEP_DIR)/%.d: %.c 
	$(RM) $(ALL_TAGS)
	$(MAKEDEPEND)  $(CPPFLAGS) $(INCLUDES) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.$(O)/$(subst .,\.,$(subst /,\/,$(OBJ_DIR)/$(STEM).$(O))) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)

$(DEP_DIR)/%.d: %.cxx
	$(RM) $(ALL_TAGS)
	$(MAKEDEPEND) $(CPPFLAGS) $(INCLUDES) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.o/$(subst .,\.,$(subst /,\/,$(OBJ_DIR)/$(STEM).o)) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)

$(DEP_DIR)/%.d: %.cc
	$(RM) $(ALL_TAGS)
	$(MAKEDEPEND) $(CPPFLAGS) $(INCLUDES) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.o/$(subst .,\.,$(subst /,\/,$(OBJ_DIR)/$(STEM).o)) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
$(DEP_DIR)/%.d:%.g
	$(MAKEDEPEND) -traditional -x c $(CPPFLAGS) $(INCLUDES) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.g\.$(O)/$(subst .,\.,$(subst /,\/,$(LIB_PKG)($(STEM).$(O)))) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
$(DEP_DIR)/%.d:%.cdf
	cd $(SRC_DIR); \
        echo "$(notdir $(STEM)).c $(ALL_TAGS): $(ALL_DEPS)" > $(ALL_TAGS) ;
        echo "$(STEM).$(O): $(STEM).c" >> $(ALL_TAGS)
$(DEP_DIR)/%.d:%.F
	$(MAKEDEPEND) -traditional -x c $(CPPFLAGS) $(INCLUDES) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.F\.$(O)/$(subst .,\.,$(subst /,\/,$(LIB_PKG)($(STEM).$(O)))) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
$(DEP_DIR)/%.d:%.f
	$(MAKEDEPEND) -traditional -x c $(CPPFLAGS) $(INCLUDES) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.F\.$(O)/$(subst .,\.,$(subst /,\/,$(LIB_PKG)($(STEM).$(O)))) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
endif
DeleteDirs :
ifndef NT
	$(RMDIR)  $(TMP_DIR)
else #/* NT */
	if exist $(subst /,\\,$(subst \,/,$(TMP_DIR)/.)) $(RMDIR) $(subst /,\\,$(subst \,/,$(TMP_DIR)))
endif #/* NT */

clean :
ifndef NT
	-$(RMDIR)  $(OUTPUT_DIRS)
else #/* NT */
	-for %i in ($(subst /,\\,$(subst \,/,$(OUTPUT_DIRS)))) if exist %i\\. $(RMDIR)  %i
endif #/* NT */

#_________________dependencies_____________________________
ifndef NT
ifndef NODEPEND
$(FILES_CINTH) : $(GEN_DIR)/%Cint.h : $(SRC_DIR)/%.h
$(FILES_CINT)  : $(GEN_DIR)/%Cint.cxx : $(GEN_DIR)/%Cint.h
ifneq (, $(strip $(FILES_D))) 
include $(FILES_D)
endif                               #
endif
endif

else
.PHONY : all
all: 
	@echo "Nothing to be done for Dll"
endif # end of DoIt
test: 
	@echo MAKE        := $(MAKE)
	@echo MAKEFLAGS   := $(MAKEFLAGS)
	@echo MAKEFILES   := $(MAKEFILES)
	@echo OUT_DIR     := $(OUT_DIR)
	@echo OUT_UP      := $(OUT_UP)
	@echo OUT_UPP     := $(OUT_UPP)
	@echo INP_DIR     := $(INP_DIR) 
	@echo PKGNAME     := $(PKGNAME) 
	@echo SRC_DIR     := $(SRC_DIR)
	@echo GEN_DIR     := $(GEN_DIR) 
	@echo LIB_DIR     := $(LIB_DIR)
	@echo OBJ_DIR     := $(OBJ_DIR)
	@echo DEP_DIR     := $(DEP_DIR) 
	@echo TMP_DIR     := $(TMP_DIR)
	@echo SRC_DIR     := $(SRC_DIR) 
	@echo BIN_DIR     := $(BIN_DIR) 
	@echo DOIT        := $(DOIT)
	@echo OUTPUT_DIRS := $(OUTPUT_DIRS)
	@echo INPUT_DIRS  := $(INPUT_DIRS)
	@echo FILES_SRC   := $(FILES_SRC)
	@echo FILES_D     := $(FILES_D)
	@echo FILES_O     := $(FILES_O)
	@echo INCLUDES    := $(INCLUDES)
	@echo VPATH       := $(VPATH)
	@echo OSFID       := $(OSFID)

	@echo FILES_ORD := $(FILES_ORD)
	@echo FILES_SYM := $(FILES_SYM)
	@echo FILES_G3  := $(FILES_G3)
	@echo FILES_CHAIN:= $(FILES_CHAIN)
	@echo FILES_SYT := $(FILES_SYT)
	@echo FILES_TAB := $(FILES_TAB)
	@echo FILES_MOD := $(FILES_MOD) 
	@echo FILES_CINT:= $(FILES_CINT)

	@echo NAMES_ORD := $(NAMES_ORD) 
	@echo NAMES_SYT := $(NAMES_SYT) 
	@echo NAMES_SYM := $(NAMES_SYM) 
	@echo NAMES_G3 := $(NAMES_G3) 
	@echo NAMES_TAB := $(NAMES_TAB)
	@echo NAMES_MOD := $(NAMES_MOD)


	@echo FILES_CINT_ORD := $(FILES_CINT_ORD) 
	@echo FILES_CINT_SYT := $(FILES_CINT_SYT) 
	@echo FILES_CINT_SYM := $(FILES_CINT_SYM) 
	@echo FILES_CINT_G3 := $(FILES_CINT_G3) 
	@echo FILES_CINT_TAB := $(FILES_CINT_TAB) 
	@echo FILES_CINT_MOD := $(FILES_CINT_MOD)

	@echo FILES_ORD_H := $(FILES_ORD_H) 
	@echo FILES_SYT_H := $(FILES_SYT_H) 
	@echo FILES_SYM_H := $(FILES_SYM_H) 
	@echo FILES_G3_H  := $(FILES_G3_H) 
	@echo FILES_TAB_H := $(FILES_TAB_H) 
	@echo FILES_MOD_H := $(FILES_MOD_H)
	@echo FILES_DCINT := $(FILES_DCINT)
ifdef NT
	@echo NT          := $(NT)
	@echo MY_DLLNAME  := $(MY_DLLNAME)
	@echo MY_EXPLIB   := $(MY_EXPLIB)
	@echo MY_IMPLIB   := $(MY_IMPLIB)
	@echo MY_DEF      := $(MY_DEF)
	@echo MY_PDB      := $(MY_PDB)
	@echo LIB_PKG     := $(LIB_PKG)
	@echo ROOT_DIR    := $(ROOT_DIR)
	@echo INCLUDE     := $(INCLUDE)

endif #/* NT */
	@echo MY_SO       := $(MY_SO)
	@echo SL_NEW      := $(SL_NEW)
	@echo QWE         := "|"$(QWE)"|"
	@echo NQWE        := $(NQWE)
