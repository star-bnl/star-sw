# $Id: MakeDll.mk,v 1.79 1999/04/28 00:49:17 fisyak Exp $
# $Log: MakeDll.mk,v $
# Revision 1.79  1999/04/28 00:49:17  fisyak
# Once  more QWERTY
#
# Revision 1.77  1999/04/26 22:40:15  fisyak
# remove -lpgc for new pfg77, Victor has updated libpgf77S.so
#
# Revision 1.76  1999/04/24 13:15:22  fisyak
# Add --sillent mode for set SILENT environmnet variable
#
# Revision 1.75  1999/04/19 21:46:09  fisyak
# Fix case of Multiple ClassDef in the same h-file
#
# Revision 1.74  1999/04/19 21:29:37  fisyak
# Add SL_EXTRA_LIB
#
# Revision 1.73  1999/04/13 20:35:40  fisyak
# Add Stypes to dictionary
#
# Revision 1.72  1999/04/02 22:59:01  fisyak
# filter-out St_laser_Maker St_run_summary_Maker St_tpctest_Maker
#
# Revision 1.71  1999/03/21 20:41:00  fisyak
# Cleanup for SL99d
#
# Revision 1.70  1999/03/12 01:33:40  fisyak
# Take out -lI77 -lF77 for RedHat 5.1/5.2
#
# Revision 1.69  1999/03/04 00:18:26  fisyak
# Add svt library for global
#
# Revision 1.68  1999/03/03 03:53:14  perev
# Add additional sort to mechanism ln -s .so
#
# Revision 1.67  1999/02/28 20:25:37  fisyak
# Fix bug with dependencies for NODEBUG version
#
# Revision 1.66  1999/02/25 22:24:39  fisyak
# Add ROOTCINTD flag
#
# Revision 1.65  1999/02/16 15:37:34  fisyak
# Clean up HP stuff
#
# Revision 1.64  1999/02/14 23:10:08  fisyak
# split tables for HP, remove duplicates for root4star
#
# Revision 1.63  1999/02/12 02:50:30  fisyak
# Fix St_Tables, single module
#
# Revision 1.62  1999/02/10 14:12:42  fisyak
# Turn on StEventReaderMaker and StTrsMaker
#
# Revision 1.61  1999/02/09 19:14:49  fisyak
# Add objy
#
# Revision 1.60  1999/02/08 02:29:19  fisyak
# New Makefile scheme
#
# Revision 1.59  1999/01/31 23:23:40  fisyak
# Correct Template
#
# Revision 1.57  1999/01/31 20:54:56  fisyak
# Fix bugs
#
# Revision 1.56  1999/01/31 17:59:47  didenko
# Clean up
#
# Revision 1.55  1999/01/30 20:22:31  fisyak
# fix double St_Module
#
# Revision 1.54  1999/01/30 04:08:22  fisyak
# Add StRootEvent
#
# Revision 1.53  1999/01/28 17:54:50  fisyak
# Fix more typo
#
# Revision 1.52  1999/01/28 17:17:40  fisyak
# Fix typo
#
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
ifndef STAR_MAKE_HOME
  STAR_MAKE_HOME := $(STAR)/mgr
endif

ASU_MALLOC_OFF :=YES

include $(STAR_MAKE_HOME)/MakeEnv.mk
include $(STAR_MAKE_HOME)/MakeDirs.mk


#
#	Define .src dir. If does not exist EMPTY
#
ifdef PKGNAME
PKG := $(PKGNAME)
endif
ifndef SO_LIB
  ifdef NT
    SO_LIB := $(BIN_DIR)/lib$(PKG).$(SOEXT)
  else
    SO_LIB := $(LIB_DIR)/$(PKG).$(SOEXT)
    ifeq ($(SRC_DIR),$(GEN_DIR))
      SO_LIB := $(LIB_DIR)/St_$(PKG).$(SOEXT)
      LIBRARY := $(wildcard $(LIB_DIR)/lib$(PKG).a $(STAR_LIB)/lib$(PKG).a)
      ifneq (,$(findstring $(PKG),global))
        LIBRARY += $(wildcard $(LIB_DIR)/libsvt.a $(STAR_LIB)/libsvt.a)
      endif
    endif
  endif 
#  ifneq (,$(findstring $(STAR_SYS),sun4x_55 sun4x_56))
#    ifneq (,$(findstring $(PKG),StTrsMaker))
#       LIBRARY += /afs/rhic/star/packages/ObjectSpace/2.0m/lib/libospace.a
#    endif
#  endif
endif


#.
#	Includes

# 	Define internal and external includes dirs
INC_NAMES := $(addprefix StRoot/,St_base StChain xdf2root StarClassLibrary StRootEvent) \
              StRoot .share .share/tables .share/$(PKG) pams inc 
#                            StarClassLibrary/include
INC_DIRS  := $(wildcard $(SRC_DIR) $(SRC_DIR)/include)
INC_DIRS  += $(strip $(wildcard $(addprefix $(ROOT_DIR)/,$(INC_NAMES)))) $(ROOT_DIR) 
ifneq ($(ROOT_DIR),$(STAR))
INC_DIRS  += $(strip $(wildcard $(addprefix $(STAR)/,$(INC_NAMES)))) $(STAR)
endif
INCINT    := $(INC_DIRS) $(CERN_ROOT)/include $(ROOTSYS)/src
INC_DIRS   = $(INCINT) $(STAF_UTILS_INCS) 

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

DINCINT  :=  -DROOT_CINT $(filter-out -DST_NO_TEMPLATE_DEF_ARGS, $(CPPFLAGS)) $(ROOTCINTD) $(INCINT)
ifneq (,$(findstring $(STAR_SYS),sun4x_55 sun4x_56))
CXXFLAGS +=-ptr$(OBJ_DIR)
endif
#
#	If NO source , NOTHING to do
#	Skip up to the end
#
FILES_ALL := $(wildcard $(addprefix $(SRC_DIR)/, *.c *.cxx *.cc))
FILES_ALL += $(wildcard $(addprefix $(SRC_DIR)/src/, *.c *.cxx *.cc))
ifneq ($(GEN_DIR),$(SRC_DIR))
FILES_ALL += $(wildcard $(addprefix $(SRC_DIR)/, *.f *.F *.g))
endif
FILES_SRC  = $(filter-out      %Cint.cxx, $(FILES_ALL))
FILES_SRC := $(filter-out %/.share/%/*.F, $(FILES_SRC))
FILES_SRC := $(filter-out %/.share/%/*.f, $(FILES_SRC))
FILES_SRC := $(filter-out %/.share/%/*.c, $(FILES_SRC))
FILES_SRC := $(filter-out %/.share/%/*.g, $(FILES_SRC))
FILES_SRC := $(filter-out %_init.cc %_i.cc, $(FILES_SRC)) 
ifeq ($(PKG),xdf2root)
ifndef NT
  FILES_SRC  += $(wildcard $(STAR)/asps/staf/dsl/src/*.c)
  INPUT_DIRS := $(STAR)/asps/staf/dsl/src
else
  LIBS := $(LIBS) $(AFS_RHIC)/star/packages/dsl/intel_wnt/lib/dsl.lib 
endif #/* NT */
endif

DOIT := $(strip $(FILES_SRC))
ifneq (,$(DOIT))

OUTPUT_DIRS := $(LIB_DIR) $(OBJ_DIR) $(DEP_DIR) $(BIN_DIR) $(TMP_DIR) $(GEN_DIR) 
INPUT_DIRS  += $(SRC_DIR) $(SRC_DIR)/src $(ROOT_DIR)

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
ifneq (tables,$(PKGNAME))
FILES_HH   := $(wildcard $(SRC_DIR)/*.h $(SRC_DIR)/*.hh)
ifneq (,$(FILES_HH))
FILES_H    := $(foreach p, $(FILES_HH), $(shell grep -l ClassDef $(p)))
endif
else
FILES_H    := $(wildcard $(SRC_DIR)/St*Table.h)
endif
FILES_ST   := $(FILES_SYM) $(FILES_SYT) $(FILES_TAB) $(FILES_MOD) 
NAMES_ST   := $(basename $(notdir $(FILES_ST)))
FILES_ALL  := $(sort $(filter-out $(FILES_ST),$(FILES_SRC)))
FILES_ORD  := $(FILES_ALL)

ifdef FILES_SYM
  NAMES_SYM      := $(subst St_,, $(notdir $(basename $(FILES_SYM))))
  FILES_CINT_SYM := $(addprefix $(GEN_DIR)/St_,$(addsuffix Cint.cxx,$(NAMES_SYM)))
  FILES_H        := $(filter-out $(subst .cxx,.h,$(FILES_SYM)), $(FILES_H))
endif
ifdef FILES_SYT
  NAMES_SYT      := $(basename $(notdir $(FILES_SYT)))
  FILES_CINT_SYT := $(addprefix $(GEN_DIR)/,$(addsuffix Cint.cxx,$(NAMES_SYT)))
  FILES_H        := $(filter-out $(subst .cxx,.h,$(FILES_SYT)), $(FILES_H))
endif

ifdef FILES_TAB
  NAMES_TAB      := $(strip $(subst _Table,,$(subst St_,,$(basename $(notdir $(FILES_TAB))))))
  FILES_CINT_TAB := $(addprefix $(GEN_DIR)/St_,$(addsuffix _TableCint.cxx,$(NAMES_TAB)))
  FILES_H        := $(filter-out $(subst .cxx,.h,$(FILES_TAB)), $(FILES_H))
endif

ifdef FILES_MOD
  NAMES_MOD      := $(subst _Module,,$(subst St_,,$(basename $(notdir $(FILES_MOD)))))
  FILES_MOD_H    := $(addprefix $(SRC_DIR)/St_,$(addsuffix _Module.h,$(NAMES_MOD)))
  STAR_SRC       := $(subst $(ROOT_DIR),$(STAR),$(SRC_DIR))
  FILES_MOD_HS   := $(wildcard $(STAR_SRC)/St_*_Module.h)
  NAMES_MOD_HS   := $(subst _Module,,$(subst St_,,$(basename $(notdir $(FILES_MOD_HS)))))
  FILES_CINT_MOD := $(addprefix $(GEN_DIR)/St_,$(addsuffix _ModuleCint.cxx,$(NAMES_MOD)))
  FILES_CINT_MOD :=$(GEN_DIR)/$(PKG)_Cint.cxx
  FILES_H        := $(filter-out $(FILES_MOD_H), $(FILES_H))
endif
#    NAMES_ORD      := $(basename $(notdir $(FILES_H)))
define AWK
grep ClassDef $(FILES_H) | awk -F\( '{print $$2}' | awk -F\, '{print $$1}'
endef
define AWK2
grep StCollectionDef $(FILES_H) | awk -F\( '{print $$2}' | awk -F\) '{print $$1}' | awk -F\, '{print $$1}'
endef

ifdef FILES_ORD
  ifneq (,$(strip $(FILES_H)))
    NAMES_ORD  := $(filter-out \#\# QWERTY, $(shell $(AWK)))
    NAMES_DD   := $(shell $(AWK2))
    NAMES_DD   := $(strip $(filter-out %StArray.h, $(NAMES_DD)))
    ifneq (,$(NAMES_DD))
      NAMES_ORDD := $(addprefix St, $(addsuffix Collection, $(NAMES_DD))\
                                     $(addsuffix Iterator,   $(NAMES_DD))\
                                     $(addprefix  VecPtr,    $(NAMES_DD)))
      NAMES_ORD := $(filter-out $(NAMES_ORDD), $(NAMES_ORD))
      FILES_COL := $(shell grep -l StCollectionDef  $(FILES_H))
      FILES_GCO := $(notdir $(FILES_COL))
    endif
  endif
  LinkDef        :=$(wildcard $(SRC_DIR)/$(PKG)LinkDef.h $(SRC_DIR)/$(PKG)LinkDef.hh)
  ifneq (,$(LinkDef))
    NAMES_DEF    := $(shell  grep C++ $(LinkDef) | grep class | awk '{print $$5}')
    NAMES_DEF    += $(shell  grep C++ $(LinkDef) | grep global | awk '{print $$5}')
    NAMES_DEF    := $(subst ;, ,$(NAMES_DEF))    
    NAMES_DEF    := $(subst -, ,$(NAMES_DEF))   
    NAMES_DEF    := $(subst !, ,$(NAMES_DEF))
    ifneq (,$(NAMES_DEF))
      NAMES_ORD  := $(strip $(filter-out $(NAMES_DEF), $(NAMES_ORD)))
      NAMES_ORDD := $(strip $(filter-out $(NAMES_DEF), $(NAMES_ORDD)))
    endif
    ifneq (,$(NAMES_DEF))
      FILES_CINT_DEF :=$(GEN_DIR)/$(PKG)_DCint.cxx 
      FILES_DEF_H    := $(wildcard $(addprefix $(SRC_DIR)/,$(addsuffix .h,$(NAMES_DEF)) \
                                                           $(addsuffix .hh,$(NAMES_DEF))))
     ifneq (,$(wildcard $(SRC_DIR)/Stypes.h))
      FILES_DEF_H    += $(SRC_DIR)/Stypes.h
      NAMES_DEF      += Stypes
     endif
    endif
  endif
  ifneq (,$(NAMES_ORD))
    FILES_CINT_ORD :=$(GEN_DIR)/$(PKG)_Cint.cxx 
    FILES_ORD_H    := $(wildcard $(addprefix $(SRC_DIR)/,$(addsuffix .h,$(NAMES_ORD)) \
                                                         $(addsuffix .hh,$(NAMES_ORD))))
    ifneq (,$(NAMES_ORDD))
      NAMES_ORD      += $(addsuffix -, $(NAMES_ORDD))
    endif
  endif
endif

LINKDEF := $(GEN_DIR)/LinkDef.h

#	Define the common part of rootcint s
define  COMMON_LINKDEF
	@test -f $(LINKDEF) &&  $(RM) $(LINKDEF);\
	echo "#ifdef __CINT__"                  	>  $(LINKDEF);\
	echo "#pragma link off all globals;"    	>> $(LINKDEF);\
	echo "#pragma link off all classes;"    	>> $(LINKDEF);\
	echo "#pragma link off all functions;"  	>> $(LINKDEF);
endef

FILES_CINT := $(FILES_CINT_SYM)  $(FILES_CINT_SYT) $(FILES_CINT_TAB) 
FILES_DCINT:= $(addsuffix .d, $(addprefix $(DEP_DIR)/,$(basename $(notdir $(FILES_CINT)))))
FILES_CINTH:= $(subst .cxx,.h,$(FILES_CINT))
FILES_CINT += $(FILES_CINT_ORD) $(FILES_CINT_DEF) $(FILES_CINT_MOD)  
FILES_O := $(addprefix $(OBJ_DIR)/,$(addsuffix .$(O), $(notdir $(basename $(FILES_SRC) $(FILES_ORD) $(FILES_CINT)))))
FILES_O := $(sort $(FILES_O))
STAR_FILES_O := $(wildcard $(STAR_OBJ_DIR)/St_*Module*.$(O) $(STAR_OBJ_DIR)/St_*Table*.$(O) $(STAR_OBJ_DIR)/*Cint.$(O))
FILTER  := $(addprefix  $(STAR_OBJ_DIR)/,$(notdir $(FILES_O)))
STAR_FILES_O := $(filter-out $(FILTER),$(STAR_FILES_O))
FILES_D := $(addsuffix .d, $(addprefix $(DEP_DIR)/,$(basename $(notdir $(FILES_O)))))

ifeq (,$(FILES_O))
all: 
	@echo Nothing to do for $(PKG)
else

MY_SO  := $(SO_LIB)
ifndef NT
  QWE    :=$(strip $(sort $(wildcard $(MY_SO).*)))
  SL_NEW :=$(MY_SO).1000
ifneq (,$(QWE))
  NQWE :=$(words $(QWE))
  QWE  :=$(word $(NQWE),$(QWE))
  QWE  :=$(subst $(MY_SO).,,$(QWE))
  QWE  :=$(shell expr $(QWE) + 1)
  SL_NEW :=$(MY_SO).$(QWE)
endif
else #/* NT */
  SL_NEW := $(MY_SO)
endif
MY_AR  := $(addsuffix .a, $(basename $(MY_SO)))
#

.PHONY : all  RootCint Libraries  DeleteDirs

all:   RootCint Libraries  DeleteDirs


#  $(INCLUDES)
RootCint :  $(FILES_CINT)

#$(GEN_DIR)/St_ModuleCint.cxx
$(FILES_CINT_SYM) : $(SRC_DIR)/St_Module.h 
	$(COMMON_LINKDEF)
	@echo "#pragma link C++ class St_Module-;"	>> $(LINKDEF)
	@echo "#pragma link C++ enum EModuleTypes;"     >> $(LINKDEF)
	@echo "#endif"					>> $(LINKDEF)
	$(CAT) $(LINKDEF); 
ifndef NT
	cd $(GEN_DIR); \
	$(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c $(DINCINT) St_Module.h $(notdir $(LINKDEF))
else
	pushd $(subst /,\\,$(subst \,/,$(GEN_DIR) & $(CP) $(1ST_DEPS) . & )) \
	$(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c $(DINCINT)  $(notdir $(1ST_DEPS)) \
        St_DataSet.h $(notdir $(LINKDEF))
endif
$(GEN_DIR)/St_TableCint.cxx : $(SRC_DIR)/St_Table.h
	$(COMMON_LINKDEF)
	@echo "#pragma link C++ class St_Table-;"       >> $(LINKDEF)
	@echo "#pragma link C++ class table_head_st-!;" >> $(LINKDEF)
	@echo "#endif"                                  >> $(LINKDEF)
	@$(CAT) $(LINKDEF);
ifndef NT
	cd $(GEN_DIR); \
        $(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c $(DINCINT) St_Table.h $(LINKDEF)
else
	pushd $(subst /,\\,$(subst \,/,$(GEN_DIR) & $(CP) $(1ST_DEPS) . & )) \
	$(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c $(DINCINT) $(notdir $(1ST_DEPS)) $(LINKDEF)
endif
$(FILES_CINT_ORD) : $(FILES_ORD_H)   
	$(COMMON_LINKDEF)
	@for p in $(NAMES_ORD); do \
                                             echo "#pragma link C++ class $${p};" >> $(LINKDEF) ; \
                                             done
	@echo "#endif"                                  >> $(LINKDEF);
	@$(CAT) $(LINKDEF)
ifndef NT
ifneq (,$(FILES_GCO))
	@for p in $(FILES_GCO); do $(RM) $(GEN_DIR)/$$p; \
                                precint.pl $(SRC_DIR)/$$p > $(GEN_DIR)/$$p; \
                                done

endif
	cd $(GEN_DIR); \
        $(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c -I./ $(DINCINT) $(notdir $(FILES_ORD_H)) \
        $(notdir $(LINKDEF))
ifneq (,$(FILES_GCO))
	@for p in $(FILES_GCO); do $(RM) $(GEN_DIR)/$$p; \
                                done

endif
else
	pushd $(subst /,\\,$(subst \,/,$(GEN_DIR) & $(CP) $(1ST_DEPS) . & )) \
        $(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c $(DINCINT) $(notdir $(1ST_DEPS)) \
	 $(notdir $(LINKDEF))
endif

$(FILES_CINT_DEF) : $(FILES_DEF_H)  $(LinkDef)
	cd $(GEN_DIR); \
	$(CAT) $(LinkDef); \
        $(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c $(DINCINT) $(notdir $(FILES_DEF_H)) \
        $(notdir $(LinkDef))

$(FILES_CINT_TAB) : $(GEN_DIR)/St_%_TableCint.cxx : $(SRC_DIR)/St_%_Table.h 
	$(COMMON_LINKDEF)
	echo "#pragma link C++ class St_$(STEM)-;"	>> $(LINKDEF);
	echo "#pragma link C++ class $(STEM)_st-!;"	>> $(LINKDEF);
	echo "#endif"					>> $(LINKDEF);
	@$(CAT) $(LINKDEF);
ifndef NT
	cd $(GEN_DIR); \
	$(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c $(DINCINT) $(notdir $(1ST_DEPS)) $(notdir $(LINKDEF))
else
	pushd $(subst /,\\,$(subst \,/,$(GEN_DIR) & $(CP) $(1ST_DEPS) . & )) \
	$(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c $(DINCINT) $(notdir $(1ST_DEPS)) $(notdir $(LINKDEF))
endif

$(FILES_CINT_MOD) : $(FILES_MOD_H) $(FILES_MOD_HS)
	$(COMMON_LINKDEF)
	@for p in $(NAMES_MOD) $(NAMES_MOD_HS);\
         do echo "#pragma link C++ class St_$${p}-;" >> $(LINKDEF) ; \
            echo "#pragma link C++ global   $${p};" >> $(LINKDEF) ; \
         done
	@echo "#endif"                                  >> $(LINKDEF);
	@$(CAT) $(LINKDEF);
ifndef NT
	cd $(GEN_DIR); \
        $(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c $(DINCINT) $(sort $(notdir $(FILES_MOD_H) $(FILES_MOD_HS))) $(notdir $(LINKDEF))
else
	pushd $(subst /,\\,$(subst \,/,$(GEN_DIR) & )) \
        $(ROOTCINT) -f $(notdir $(ALL_TAGS)) -c $(DINCINT) $(notdir $(FILES_MOD_H)) $(notdir $(LINKDEF))
endif


Libraries : $(MY_SO) $(MY_SO_CINT) 


ifndef NT

$(MY_SO) : $(FILES_O) $(wildcard $(OBJ_DIR)/Templates.DB/*.$(O)) $(STAR_FILES_O) $(LIBRARY)
	cd $(OBJ_DIR);  \
        $(SO) $(SOFLAGS) $(SoOUT) $(SL_NEW) $(ALL_DEPS) $(SL_EXTRA_LIB) ; \
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
	$(SO) $(SOFLAGS) $(subst /,\\,$(subst \,/,$(SoOUT)$(MY_SO) $(sort $(OBJ_DIR)/*.$(O)  $(STAR_OBJ_DIR)/*.$(O) $(SYS_DIR)/obj/St_base/St_staf_dummies.$(O)) $(LIBS) $(LIBRARY) -PDB:$(MY_PDB))) 
#	$(SO) $(SOFLAGS) $(subst /,\\,$(subst \,/,$(SoOUT)$(MY_SO) $(FILES_O)  $(STAR_FILES_O) $(LIBS) $(LIBRARY) -PDB:$(MY_PDB)))
#	$(SO) $(SOFLAGS) $(subst /,\\,$(subst \,/,$(SoOUT)$(SL_NEW) $(FILES_O) $(LIBS) $(LIBRARY) -PDB:$(MY_PDB)))
#        $(RM) $(MY_SO)
endif
	@echo "           Shared library " $(MY_SO) " has been created"   
$(OBJ_DIR)/%.$(O) : %.c
ifdef NT
	$(CC)  -c $(subst \,/,$(CPPFLAGS) $(CFLAGS) $(INCLUDES) -Fd$(OBJ_DIR)\$(PKG) $(CINP)$(1ST_DEPS) $(COUT)$(ALL_TAGS))
else
	$(CC)  -c $(CPPFLAGS) $(CFLAGS) $(INCLUDES) $(CINP)$(1ST_DEPS) $(COUT)$(ALL_TAGS)
endif

$(OBJ_DIR)/%.$(O) : %.cc
ifdef NT
	$(CXX) -c $(subst \,/,$(CPPFLAGS) $(CXXFLAGS)  $(INCLUDES) $(CXXINP)$(1ST_DEPS) $(COUT)$(ALL_TAGS) -Fd$(OBJ_DIR)\$(PKG))
else
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS) $(INCLUDES) $(CXXINP)$(1ST_DEPS) $(COUT)$(ALL_TAGS)
endif
$(OBJ_DIR)/%.$(O) : %.cxx 
ifdef NT
	$(CXX) -c $(subst \,/,$(CPPFLAGS) $(CXXFLAGS)  $(INCLUDES) $(CXXINP)$(1ST_DEPS) $(COUT)$(ALL_TAGS)  -Fd$(OBJ_DIR)\$(PKG))
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
$(OBJ_DIR)/%.$(O):%.g $(GEN_DIR)/geant3.def
	cp $(1ST_DEPS) $(GEN_DIR); cd $(GEN_DIR); $(GEANT3) $(1ST_DEPS) -o  $(GEN_DIR)/$(STEM).F
	$(FOR72)  $(CPPFLAGS) $(FFLAGS) -c $(GEN_DIR)/$(STEM).F  -o  $(ALL_TAGS)
$(OBJ_DIR)/%.$(O): %.F
	$(FC)  $(CPPFLAGS)  $(INCLUDES) $(FFLAGS) $(FEXTEND)   -c $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).$(O)
ifdef $(LIB_PKG)
	$(AR) $(ARFLAGS) $(LIB_PKG) $(OBJ_DIR)/$(STEM).$(O); $(RM) $(OBJ_DIR)/$(STEM).$(O)
endif
endif #/* NT */
$(OBJ_DIR)/%.$(O): %.cdf
	$(KUIPC) $(KUIPC_FLAGS) $(1ST_DEPS) $(GEN_DIR)/$(STEM).c
include $(STAR_MAKE_HOME)/MakeDep.mk
endif
DeleteDirs :
ifndef NT
#	rm -rf  $(TMP_DIR)
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
ifneq (, $(strip $(FILES_D))) 
include $(FILES_D)
endif                               #
endif
endif

else
.PHONY : all
all: 
	@echo "Nothing to be done for Dll: $(SRC_DIR)"
endif # end of DoIt
test: 
	@echo MAKE        := $(MAKE)
	@echo MAKEFLAGS   := $(MAKEFLAGS)
	@echo MAKEFILES   := $(MAKEFILES)
	@echo OUT_DIR     := $(OUT_DIR)
	@echo INP_DIR     := $(INP_DIR) 
	@echo PKG         := $(PKG) 
	@echo SRC_DIR     := $(SRC_DIR)
	@echo GEN_DIR     := $(GEN_DIR) 
	@echo LIB_DIR     := $(LIB_DIR)
	@echo OBJ_DIR     := $(OBJ_DIR)
	@echo DEP_DIR     := $(DEP_DIR) 
	@echo TMP_DIR     := $(TMP_DIR)
	@echo SRC_DIR     := $(SRC_DIR) 
	@echo BIN_DIR     := $(BIN_DIR) 
	@echo OUTPUT_DIRS := $(OUTPUT_DIRS)
	@echo INPUT_DIRS  := $(INPUT_DIRS)
	@echo INCLUDES    := $(INCLUDES)
	@echo VPATH       := $(VPATH)
	@echo OSFID       := $(OSFID)
	@echo SO_LIB      := $(SO_LIB)
	@echo MY_SO       := $(MY_SO)
	@echo SL_NEW      := $(SL_NEW)
	@echo QWE         := "|"$(QWE)"|"
	@echo NQWE        := $(NQWE)
	@echo EWQ        := "|"$(EWQ)"|"
	@echo NEWQ       := $(NEWQ)
	@echo LIBRARY     := $(LIBRARY)
	@echo STAR_OBJ_DIR:= $(STAR_OBJ_DIR)
	@echo STAR_FILES_O:= $(STAR_FILES_O)
	@echo FILES_MOD_HS:= $(FILES_MOD_HS)
	@echo STAR_SRC    := $(STAR_SRC)
	@echo NAMES_MOD_HS:= $(NAMES_MOD_HS)
	@echo MY_SO_CINT := $(MY_SO_CINT)
	@echo SL_NEW_CINT := $(SL_NEW_CINT)

	@echo FILES_SRC   := $(FILES_SRC)
	@echo FILES_D     := $(FILES_D)
	@echo FILES_O     := $(FILES_O)
	@echo FILES_ALL := $(FILES_ALL)
	@echo FILES_ORD := $(FILES_ORD)
	@echo FILES_DEF := $(FILES_DEF)
	@echo FILES_SYM := $(FILES_SYM)
	@echo FILES_SYT := $(FILES_SYT)
	@echo FILES_TAB := $(FILES_TAB)
	@echo FILES_MOD := $(FILES_MOD) 
	@echo FILES_CINT:= $(FILES_CINT)
	@echo FILES_H   := $(FILES_H)

	@echo NAMES_ORD := $(NAMES_ORD) 
	@echo NAMES_ORDD:= $(NAMES_ORDD) 
	@echo NAMES_DD  := $(NAMES_DD) 
	@echo NAMES_DEF := $(NAMES_DEF) 
	@echo NAMES_SYT := $(NAMES_SYT) 
	@echo NAMES_SYM := $(NAMES_SYM) 
	@echo NAMES_TAB := $(NAMES_TAB)
	@echo NAMES_MOD := $(NAMES_MOD)


	@echo FILES_CINT_ORD := $(FILES_CINT_ORD) 
	@echo FILES_CINT_DEF := $(FILES_CINT_DEF) 
	@echo FILES_CINT_SYT := $(FILES_CINT_SYT) 
	@echo FILES_CINT_SYM := $(FILES_CINT_SYM) 
	@echo FILES_CINT_TAB := $(FILES_CINT_TAB) 
	@echo FILES_CINT_MOD := $(FILES_CINT_MOD)

	@echo FILES_ORD_H := $(FILES_ORD_H) 
	@echo FILES_DEF_H := $(FILES_DEF_H) 
	@echo FILES_MOD_H := $(FILES_MOD_H)
	@echo FILES_DCINT := $(FILES_DCINT)
	@echo FILES_COL   := $(FILES_COL)
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
	@echo DOIT        := $(DOIT)
	@echo PKGNAME     := $(PKGNAME)	
	@echo PKG         := $(PKG)
