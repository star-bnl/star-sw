# $Id: MakePam.mk,v 1.124 1999/10/04 14:35:35 fisyak Exp $
# $Log: MakePam.mk,v $
# Revision 1.124  1999/10/04 14:35:35  fisyak
# Add StRoot in include path
#
# Revision 1.123  1999/09/30 20:07:37  fisyak
# Synchronize cons and makel Path
#
# Revision 1.122  1999/09/26 19:16:02  fisyak
# Merge directory structures with cons
#
# Revision 1.121  1999/09/24 22:18:28  fisyak
# Add new Table method (VF), fix bug in VPATH
#
# Revision 1.120  1999/09/18 01:12:06  fisyak
# Add CERN_ROOT/include
#
# Revision 1.118  1999/09/13 22:19:49  fisyak
# Add inlclude and include/tables in CPP path
#
# Revision 1.117  1999/09/12 01:47:27  fisyak
# merge cons and makel tables h-files into include/tables
#
# Revision 1.116  1999/09/09 23:02:17  fisyak
# Suppress library versioning for users
#
# Revision 1.115  1999/09/07 21:01:10  fine
# new macro for Stypes.h TableImpl has been introduced
#
# Revision 1.114  1999/09/07 19:31:04  fine
# table descriptor access has been changed. All tables are affected and must be re-compiled
#
# Revision 1.113  1999/09/03 20:44:58  fisyak
# Fix typo
#
# Revision 1.112  1999/09/03 17:49:51  fisyak
# Make makel and cons compartible in OBJ
#
# Revision 1.111  1999/08/22 23:09:35  fisyak
# remove dir path from library linkage
#
# Revision 1.110  1999/08/20 23:07:28  fisyak
# Fix new StAF include path
#
# Revision 1.109  1999/08/20 13:13:30  fisyak
# Devorce StAF and STAR Library
#
# Revision 1.108  1999/08/20 01:42:59  fisyak
# Devorce StAF and STAR Library
#
# Revision 1.107  1999/08/16 17:03:05  fisyak
# Add protection against empty directories
#
# Revision 1.105  1999/08/15 19:09:05  fisyak
# keep objects for pams
#
# Revision 1.104  1999/08/14 00:37:35  fisyak
# New Cons stuff
#
# Revision 1.103  1999/07/28 14:16:24  fisyak
# Simplify for St_base, take out geant3.def
#
# Revision 1.102  1999/07/20 00:24:55  fisyak
# Remove Objy
#
# Revision 1.101  1999/07/07 14:08:37  fisyak
# Extract dependencies from standard make path, add pathes to StarClassLibrary and MySql
#
# Revision 1.100  1999/06/30 15:29:15  fisyak
# Add StarClassLibrary for pams
#
# Revision 1.99  1999/06/29 22:51:08  fisyak
# Add Objectspace for pams
#
# Revision 1.98  1999/06/29 17:53:27  fisyak
# Fix Table streamer
#
# Revision 1.97  1999/06/27 22:44:02  fisyak
# Merge StRootEvent and StEvent
#
# Revision 1.96  1999/06/19 20:31:51  fisyak
# Fix bugs with include path
#
# Revision 1.95  1999/06/16 12:37:02  fisyak
# Changes for egcs-1.1.2 on Solaris
#
# Revision 1.94  1999/06/11 15:41:56  fisyak
# Move mortran generated files to OBJ instead of GEN
#
# Revision 1.93  1999/06/08 11:30:14  fisyak
# take out NT stuff for the moment
#
# Revision 1.92  1999/04/24 13:15:24  fisyak
# Add --sillent mode for set SILENT environmnet variable
#
# Revision 1.91  1999/04/18 23:35:29  fisyak
# Add SL_EXTRA_LIB
#
# Revision 1.90  1999/03/30 15:51:53  fisyak
# Make silent test for geant3.def
#
# Revision 1.89  1999/03/06 20:13:30  fisyak
# fix cleanup
#
# Revision 1.88  1999/03/04 00:18:26  fisyak
# Add svt library for global
#
# Revision 1.87  1999/03/03 03:52:09  perev
# Add additional sort to mechanism ln -s .so
#
# Revision 1.86  1999/02/19 14:40:13  fisyak
# remove extra dependencies for idls
#
# Revision 1.85  1999/02/16 15:37:37  fisyak
# Clean up HP stuff
#
# Revision 1.84  1999/02/13 01:52:09  didenko
# Fix bug with list of tables
#
# Revision 1.83  1999/02/12 15:21:07  fisyak
# Add all tables to the list, not only call by module
#
# Revision 1.82  1999/02/12 02:50:32  fisyak
# Fix St_Tables, single module
#
# Revision 1.79  1999/01/31 22:31:44  fisyak
# Ad includes in dependencies
#
# Revision 1.78  1999/01/30 04:08:23  fisyak
# Add StRootEvent
#
# Revision 1.77  1999/01/25 23:49:14  fisyak
# Add MAKEFLAG
#
# Revision 1.76  1999/01/21 02:15:31  fisyak
# New StChain w/o automatical streamer generation
#
# Revision 1.75  1999/01/20 02:16:51  fisyak
# Active STAR_HOST_SYS for egcs
#
# Revision 1.74  1998/12/29 20:06:56  didenko
# take back .didl
#
# Revision 1.73  1998/12/12 00:58:36  fisyak
# remove STAF
#
# Revision 1.72  1998/12/11 22:21:18  fisyak
# Remove doublet in idl and inc directories
#
# Revision 1.71  1998/12/10 22:47:55  fine
# Correction for fit Window NT branch
#
# Revision 1.70  1998/12/09 13:53:54  fisyak
# Remove dependencies from module idl
#
# Revision 1.69  1998/12/02 20:42:47  perev
# cleanup
#
# Revision 1.68  1998/12/01 01:53:28  fisyak
# Merge with NT
#
# Revision 1.67  1998/11/25 21:51:07  fisyak
# remove tcc trg and l3 from directories list for hp_ux102
#
# Revision 1.66  1998/11/17 02:27:04  fisyak
# Add log
#
# Revision 1.65  1998/11/16 01:26:45  fisyak
# New merging with NT
#
# Revision 1.63  1998/11/13 15:48:44  fisyak
# Merged version with NT
#
# Revision 1.62  1998/11/13 00:19:30  fisyak
# Add flags for SCL St_trs_Maker
#
# Revision 1.61  1998/10/29 23:34:26  fisyak
# set ASU_MALLOC_OFF for PAMS
#
# Revision 1.60  1998/10/20 01:42:00  fisyak
# debug only for db
#
# Revision 1.59  1998/10/19 01:03:12  fisyak
# Add also separate OBJ directory for NODEBUG version
#
# Revision 1.58  1998/10/18 21:50:29  fisyak
# separate dependencies for debug and Nodebug libraries
#
# Revision 1.57  1998/10/11 23:32:50  fisyak
# reastore  -D__ROOT__
#
# Revision 1.56  1998/10/06 19:52:30  fisyak
# Add g2t script
#
# Revision 1.55  1998/09/26 02:26:06  fisyak
# Fix NOROOT option
#
# Revision 1.54  1998/09/18 22:35:24  fisyak
# correct dependencies
#
# Revision 1.53  1998/09/18 13:43:52  fisyak
# Add dependencies for St_*Tables from base
#
# Revision 1.51  1998/09/16 21:52:13  fisyak
# Add dependencies for StRoot
#
# Revision 1.50  1998/09/15 22:15:28  fisyak
# Fix root/noroot options
#
# Revision 1.49  1998/08/28 02:10:14  nevski
# Add standard tables include
#
# Revision 1.48  1998/08/26 01:59:22  fisyak
# Remove system from ROOT path
#
# Revision 1.47  1998/08/25 02:34:27  fisyak
# Library directory nodeb -> LIB
#
# Revision 1.46  1998/08/25 02:10:09  fisyak
# Add nodebug
#
# Revision 1.45  1998/08/19 21:41:41  fisyak
# Split base -> base + xdf2root
#
# Revision 1.44  1998/08/19 00:03:59  fisyak
# Add NOROOT option
#
# Revision 1.43  1998/08/18 18:53:16  fisyak
# Add root I/O
#
# Revision 1.42  1998/08/17 14:44:28  didenko
# ****
#
# Revision 1.41  1998/08/12 22:15:05  fisyak
# Add check for %_i.cc
#
# Revision 1.40  1998/08/10 23:20:53  fisyak
# Add test for base and tables
#
MAKEFLAGS := $(filter-out w, $(MAKEFLAGS))
ifndef STAR_MAKE_HOME
  STAR_MAKE_HOME := $(STAR)/mgr
endif

ASU_MALLOC_OFF :=YES

include $(STAR_MAKE_HOME)/MakeEnv.mk
include $(STAR_MAKE_HOME)/MakeDirs.mk
ifndef OUT_DIR
  OUT_DIR := $(word 1, $(subst /pams, ,$(INP_DIR)))
endif
ifeq (,$(strip $(filter /%,$(OUT_DIR))))
  override OUT_DIR := $(CWD)/$(OUT_DIR)
endif
IDLS    := $(wildcard $(SRC_DIR)/*.idl $(SRC_DIR)/*/*.idl)
ifneq (,$(IDLS))       
  FILES_IDM := $(shell egrep -l 'interface.*:.*amiModule' $(IDLS))
  NAMES_IDM := $(basename $(notdir $(FILES_IDM)))
  SRC_DIRR  := $(addprefix $(STAR), $(subst $(OUT_DIR),,$(SRC_DIR)))
  IDLSS     := $(wildcard $(SRC_DIRR)/*.idl $(SRC_DIRR)/*/*.idl)
  ifneq (,$(IDLSS))
    FILES_IDMS:= $(shell egrep -l 'interface.*:.*amiModule' $(IDLSS))
  endif
  NAMES_IDMS:= $(filter-out $(NAMES_IDM), $(basename $(notdir $(FILES_IDMS))))
endif
FILES_G  := $(wildcard $(SRC_DIR)/*.g $(SRC_DIR)/*/*.g)
#_________________________________________________________________________
SUFFIXES := .c .cc .C .cxx .f .F .g .h .hh .hpp .inc .idl
sources :=$(strip $(sort $(dir $(foreach s, $(SUFFIXES), $(wildcard $(SRC_DIR)/*$(s) $(SRC_DIR)/*/*$(s) $(SRC_DIR)/*/*/*$(s))))))
ifeq (,$(sources))
  all:
	@echo Nothing to do for package $(PKG), no source files
  depend:
	@echo Nothing to do for package $(PKG), no source files
else
SRC_DIRS:= $(subst /TAIL, ,$(addsuffix TAIL, $(sources)))
INC_NAMES  := include StRoot
INC_DIRS  += $(strip $(wildcard $(addprefix $(ROOT_DIR)/,$(INC_NAMES))))
ifneq ($(ROOT_DIR),$(STAR))
  INC_DIRS  += $(strip $(wildcard $(addprefix $(STAR)/,$(INC_NAMES))))
endif
INC_DIRS  += $(STAF)/inc
INC_DIRS+= $(STAF_UTILS_INCS)
DINCINT  :=  -DROOT_CINT $(filter-out -DST_NO_TEMPLATE_DEF_ARGS, $(CPPFLAGS)) $(ROOTCINTD) $(INCINT)
ifeq ($(STAR_HOST_SYS),sun4x_56)
  CXXFLAGS +=-ptr$(OBJ_DIR)
endif
VPATH   := $(wildcard $(SRC_DIRS)) $(GEN_TAB) $(GEN_TAB_INC) $(OBJ_DIR) $(IDL_DIRS) $(INC_DIRS)
ifneq (,$(FILES_IDM))
  VPATH   += $(GEN_DIR)
endif
define MAKE_TABLE_CXX
	@$(RM) $(GEN_TAB)/St_$(STEM)_Table.cxx;\
echo "#include \"St_$(STEM)_Table.h\""         >$(GEN_TAB)/St_$(STEM)_Table.cxx;\
echo "/////////////////////////////////////////////////////////////////////////" >>$(GEN_TAB)/St_$(STEM)_Table.cxx;\
echo "//                                          ">>$(GEN_TAB)/St_$(STEM)_Table.cxx;\
echo "//  Class St_$(STEM) wraps the STAF table $(STEM) ">>$(GEN_TAB)/St_$(STEM)_Table.cxx;\
echo "//  It has been generated "by automatic". Please don't change it \"by hand\" ">>$(GEN_TAB)/St_$(STEM)_Table.cxx;\
echo "//                                          ">>$(GEN_TAB)/St_$(STEM)_Table.cxx;\
echo "/////////////////////////////////////////////////////////////////////////  ">>$(GEN_TAB)/St_$(STEM)_Table.cxx;\
echo "                                            ">>$(GEN_TAB)/St_$(STEM)_Table.cxx;\
echo "#include \"Stypes.h\"                       ">>$(GEN_TAB)/St_$(STEM)_Table.cxx;\
echo "TableImpl($(STEM))                     ">>$(GEN_TAB)/St_$(STEM)_Table.cxx;
endef
define MAKE_TABLE_H
	@$(RM) 	$(GEN_TAB_INC)/St_$(STEM)_Table.h ;\
echo "#ifndef STAF_St_$(STEM)_Table         ">$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "#define STAF_St_$(STEM)_Table        ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "                                           ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "#include \"St_Table.h\"                      ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "                                           ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "#include \"$(STEM).h\"                 ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "                                           ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "class St_$(STEM) : public St_Table   ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "{                                          ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "protected:                                 ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "  static St_tableDescriptor *fgColDescriptors;">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "  virtual St_tableDescriptor *GetDescriptorPointer() const { return fgColDescriptors;}">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "  virtual void SetDescriptorPointer(St_tableDescriptor *list) const { fgColDescriptors = list;}">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "public:                                    ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "  St_$(STEM)() : St_Table(\"$(STEM)\",sizeof($(STEM)_st)) {SetType(\"$(STEM)\");}           ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h ;\
echo "  St_$(STEM)(Text_t *name) : St_Table(name,sizeof($(STEM)_st)) {SetType(\"$(STEM)\");}          ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "  St_$(STEM)(Int_t n): St_Table(\"$(STEM)\",n,sizeof($(STEM)_st)) {SetType(\"$(STEM)\");}   ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "  St_$(STEM)(Text_t *name,Int_t n): St_Table(name,n,sizeof($(STEM)_st)) {SetType(\"$(STEM)\");} ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "  $(STEM)_st *GetTable(Int_t i=0){ return ($(STEM)_st *)s_Table+i;}                           ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "  $(STEM)_st &operator[](Int_t i){ assert(i<0 || i >= GetNRows()); return *GetTable(i); }     ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "                                           ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "  ClassDef(St_$(STEM),0) // class particle STAF tables  ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "};                                                            ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "                                                              ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h;\
echo "#endif                                                        ">>$(GEN_TAB_INC)/St_$(STEM)_Table.h
endef
#-------------------------------includes----------------------------
STICFLAGS =  $(addprefix -I,  $(INC_DIRS))
CXXFLAGS   += -DASU_MALLOC_OFF

INCLUDES += $(addprefix -I,$(INC_DIRS)) -I$(CERN_ROOT)/include

ifneq ($(OUT_DIR),$(STAR))        
#  INCLUDES += -I$(STAR)/.share/$(DOMAIN) -I$(STAR)/.share/tables
endif                          
CPPFLAGS += $(INCLUDES)
FFLAGS   += -DCERNLIB_TYPE  -I$(CERN_ROOT)/include -I$(CERN_ROOT)/include/geant321 
#                 I have idl- or g-files
FILES_CC := $(wildcard $(addsuffix /*.cc, $(SRC_DIRS)))
FILES_CXX:= $(wildcard $(addsuffix /*.cxx, $(SRC_DIR)))
FILES_C  := $(wildcard $(addsuffix /*.c , $(SRC_DIRS)))
FILES_F  := $(wildcard $(addsuffix /*.F , $(SRC_DIRS)))
FILES_CDF:= $(wildcard $(addsuffix /*.cdf , $(SRC_DIRS)))

#NAMES_G  := $(basename $(notdir $(FILES_G)))
#NAMES_CC := $(basename $(notdir $(FILES_CC)))
#NAMES_CXX:= $(filter-out %Cint St_%_Module St_%_Table,$(basename $(notdir $(FILES_CXX))))
#NAMES_C  := $(basename $(notdir $(FILES_C)))
#NAMES_F  := $(basename $(notdir $(FILES_F)))
#NAMES_CDF:= $(basename $(notdir $(FILES_CDF)))

NAMES_G  := $(basename $(subst $(SRC_DIR),,$(FILES_G)))
NAMES_CC := $(basename $(subst $(SRC_DIR),,$(FILES_CC)))
NAMES_CXX:= $(filter-out %Cint St_%_Module St_%_Table,$(basename $(subst $(SRC_DIR),,$(FILES_CXX))))
NAMES_C  := $(basename $(subst $(SRC_DIR),,$(FILES_C)))
NAMES_F  := $(basename $(subst $(SRC_DIR),,$(FILES_F)))
NAMES_CDF:= $(basename $(subst $(SRC_DIR),,$(FILES_CDF)))
#.________________________  modules ____________________________________________
FILES_IDT := $(notdir $(wildcard $(OUT_DIR)/pams/$(DOMAIN)/idl/*.idl $(STAR)/pams/$(DOMAIN)/idl/*.idl))
ifneq (,$(FILES_IDM))
  FILES_ICC := $(addprefix $(GEN_DIR)/, $(subst .idl,_i.cc,  $(notdir $(FILES_IDM))))
  FILES_IH  := $(addprefix $(GEN_INC)/, $(subst .idl,.h,     $(notdir $(FILES_IDM))))
  FILES_INC := $(addprefix $(GEN_INC)/, $(subst .idl,.inc,   $(notdir $(FILES_IDM))))
  FILES_MOD := $(addprefix $(GEN_DIR)/St_,$(subst .idl,_Module.cxx, $(notdir $(FILES_IDM))))
  FILES_MHH := $(addprefix $(GEN_DIR_INC)/St_,$(subst .idl,_Module.h  , $(notdir $(FILES_IDM))))
  FILES_ALL_MOD := $(FILES_SYM) $(FILES_ICC) $(FILES_IH) $(FILES_INC) $(FILES_MOD) $(FILES_MHH)
  FILES_IDT += $(foreach IDM, $(FILES_IDM), $(shell $(STIC) -T -q $(STICFLAGS) $(IDM))) 
endif    
FILES_IDT := $(sort $(FILES_IDT))
#._________________________ Tables _____________________________________________
ifneq (,$(FILES_IDT))
  FILES_TAH := $(addprefix $(GEN_INC)/, $(addsuffix .h,   $(sort $(basename $(notdir $(FILES_IDT))))))
  FILES_TAI := $(addprefix $(GEN_INC)/, $(addsuffix .inc, $(sort $(basename $(notdir $(FILES_IDT))))))
  FILES_TAB := $(addprefix $(GEN_TAB)/St_, $(addsuffix _Table.cxx, $(sort $(basename $(notdir $(FILES_IDT))))))
  FILES_THH := $(addprefix $(GEN_TAB_INC)/St_, $(addsuffix _Table.h, $(sort $(basename $(notdir $(FILES_IDT))))))
  FILES_ALL_TAB := $(FILES_SYT) $(FILES_TAH) $(FILES_TAI) $(FILES_TAB) $(FILES_THH)
endif
FILES_O  := $(strip $(addprefix $(OBJ_DIR), $(addsuffix .$(O), $(NAMES_F) $(NAMES_C) $(NAMES_CC))))
ifndef NODEPEND                
  FILES_D  := $(addprefix $(DEP_DIR)/, $(addsuffix .d,   $(basename $(notdir $(FILES_O)))))
  FILES_DM := $(addprefix $(GEN_DIR)/, $(addsuffix .didl, $(NAMES_IDM)))                         
endif                          
FILES_O  += $(addprefix $(OBJ_DIR)/, $(addsuffix .$(O),   $(notdir $(basename $(FILES_ICC)))))
DIRS_O    = $(sort $(dir $(FILES_O)))
# *.cc moved to sl $(NAMES_CC)
ifneq (,$(strip $(FILES_IDM) $(FILES_G) $(FILES_CDF))) 
  SL_PKG  := $(LIB_DIR)/$(PKG).sl
  ifeq ($(ROOT_DIR),$(STAR))
    QWE  := $(sort $(wildcard $(SL_PKG).*))
    SL_NEW := $(SL_PKG).1000
    ifneq (,$(QWE))
      NQWE := $(words $(QWE))
      QWE  := $(word $(NQWE),$(QWE))
      QWE  := $(subst $(SL_PKG).,,$(QWE))
      QWE  := $(shell expr $(QWE) + 1)
      SL_NEW := $(SL_PKG).$(QWE)
    endif
  else
    SL_NEW :=$(SL_PKG).9999
  endif
endif
SL_PKG_NOTDIR := $(notdir $(SL_PKG))
SL_NEW_NOTDIR := $(notdir $(SL_NEW))

ifneq (,$(NAMES_IDM))          
  FILES_init  := $(addprefix $(OBJ_DIR)/, $(PKG)_init.$(O))
endif                      
ifneq (,$(NAMES_CDF))          
  FILES_O    += $(addprefix $(OBJ_DIR), $(addsuffix .$(O), $(NAMES_CDF)))
endif                          
ifneq (,$(NAMES_G))            
  FILES_OG    := $(addprefix $(OBJ_DIR), $(addsuffix .$(O), $(NAMES_G)))
  FILES_O     += $(FILES_OG) 
  FILES_D     += $(addprefix $(DEP_DIR)/, $(addsuffix .d,   $(basename $(notdir $(FILES_OG)))))
endif
ifneq (,$(NAMES_CC))            
  FILES_SL    += $(filter-out $(FILES_o), $(addprefix $(OBJ_DIR), $(addsuffix .$(O), $(NAMES_CC))))
  FILES_D     += $(addprefix $(DEP_DIR)/, $(addsuffix .d,   $(basename $(notdir $(FILES_CC)))))
endif                          
ifneq (,$(NAMES_CXX))            
  FILES_SL    += $(filter-out $(FILES_o), $(addprefix $(OBJ_DIR), $(addsuffix .$(O), $(NAMES_CXX))))
  FILES_D     += $(addprefix $(DEP_DIR)/,$(addsuffix .d, $(basename $(notdir $(FILES_CXX)))))
endif                          
ifneq (,$(strip $(FILES_O) $(FILES_OG)))
  LIB_PKG := $(LIB_DIR)/lib$(DOMAIN).$(A)
endif
ifndef LIBRARIES
  LIBRARIES := $(LIB_PKG)	               
endif
ifneq ($(STAR_PATH),$(OUT_DIR))
  ifneq ($(LIB_PKG),)
    LIBRARIES += $(wildcard  $(STAR_LIB)/lib$(PKG).$(A))
  endif                           
endif                           
qwe     := $(shell test ! -f $(LIB_PKG) ||  $(AR) $(ARFLAGS) $(LIB_PKG))
OBJS    := $(FILES_O);
ifeq (,$(strip $(LIB_PKG) $(SL_PKG)))
  all:
	@echo Nothing to do for package $(PKG)
else
#-------------------------------rules-------------------------------
# phony - not a file
.PHONY               : MakeInc lib sl_lib depend clean test
all                  : MakeInc $(LIB_PKG) $(SL_PKG) 
  ifndef NOROOT
MakeInc  : $(FILES_ALL_TAB) $(FILES_ALL_MOD) 
  else
MakeInc  : $(FILES_TAI) $(FILES_TAH)
  endif
# all files:
  ifneq (,$(strip $(FILES_O) $(FILES_SL) $(FILES_OG) $(FILES_init))) 
#                 I have NO idl- and NO g-files
    ifneq ($(FILES_O),)    
$(LIB_PKG):$(OBJS) 
	$(AR) $(ARFLAGS) $(LIB_PKG) $(FILES_O); 
    endif                          
    ifneq ($(strip $(FILES_SL) $(FILES_OG) $(FILES_init)),)   
$(SL_PKG): $(FILES_SL) $(FILES_OG) $(FILES_init) $(LIB_PKG) $(wildcard $(OBJ_DIR)/Templates.DB/*.$(O))
	$(SO) $(SOFLAGS) $(FILES_SL) $(FILES_OG) $(FILES_init)  -o $(SL_NEW)  $(LIBRARIES) $(SL_EXTRA_LIB) && \
        $(RM) $(SL_PKG) &&  cd $(LIB_DIR) &&  $(LN) $(SL_NEW_NOTDIR) $(SL_PKG_NOTDIR) 
	@echo "           Shared library " $(SL_PKG) " has been created"   
#--------- module --------- 
      ifneq ($(NAMES_IDM),)           
$(OBJ_DIR)/$(PKG)_init.$(O): $(FILES_IDM)  
	@if [ -f $(GEN_DIR)/$(PKG)_init.cc ]; then  rm $(GEN_DIR)/$(PKG)_init.cc ; fi
	@echo '/* '$(PKG)' package interface to STAF */' > $(GEN_DIR)/$(PKG)_init.cc
	@echo '/* automatically generated file */'      >> $(GEN_DIR)/$(PKG)_init.cc
	@for p in $(NAMES_IDM) $(NAMES_IDMS); do echo $$p; echo '#include "'$$p'.h"'   \
                                                        >> $(GEN_DIR)/$(PKG)_init.cc ; done
	@echo 'extern "C" int  $(PKG)_init (void);'     >> $(GEN_DIR)/$(PKG)_init.cc
	@echo 'extern "C" int  $(PKG)_start(void);'     >> $(GEN_DIR)/$(PKG)_init.cc
	@echo 'extern "C" int  $(PKG)_stop (void);'     >> $(GEN_DIR)/$(PKG)_init.cc
	@echo 'extern "C" void $(PKG)_init_();  '       >> $(GEN_DIR)/$(PKG)_init.cc
	@echo 'void $(PKG)_init_() {$(PKG)_start();}'   >> $(GEN_DIR)/$(PKG)_init.cc
	@echo 'int  $(PKG)_init () { return 1; }'       >> $(GEN_DIR)/$(PKG)_init.cc
	@echo 'int  $(PKG)_start() {'                   >> $(GEN_DIR)/$(PKG)_init.cc
	@for p in $(NAMES_IDM) $(NAMES_IDMS); do echo "      $${p}_load_ami(ami);" \
                                                        >> $(GEN_DIR)/$(PKG)_init.cc ; done
	@echo '                       return 1; }'      >> $(GEN_DIR)/$(PKG)_init.cc
	@echo 'int  $(PKG)_stop () { return 1; }'       >> $(GEN_DIR)/$(PKG)_init.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $(GEN_DIR)/$(PKG)_init.cc -o $(ALL_TAGS)
      endif                           
    endif                           # NO idl- or g-files
#_________________dependencies_____________________________
    ifndef NODEPEND
      ifneq (,$(strip $(FILES_DM)))
include $(FILES_DM)
      endif                               # 
      ifneq (,$(strip $(FILES_D))) 
include $(FILES_D)
      endif                               #
    endif                            # end if of FILES_O FILES_SL
  endif
#--------  idm, idl --------
  ifneq (,$(FILES_ALL_TAB))
$(FILES_TAH) : $(GEN_INC)/%.h : %.idl
	$(CP) $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP); $(STIC) -H -q $(STEM).idl; \
        $(CP) $(STEM).h $(GEN_INC)/$(STEM).h
$(FILES_TAI) : $(GEN_INC)/%.inc : %.idl
	$(CP) $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP); $(STIC) -H -q $(STEM).idl; \
        $(CP) $(STEM).inc $(GEN_INC)/$(STEM).inc
    ifndef NOROOT
#------------------------------------------- ---------------------------------
#$(FILES_TAB) : $(GEN_TAB)/St_%_Table.cxx : $(GEN_TAB_INC)/%.h
$(GEN_TAB)/St_%_Table.cxx:
	$(MAKE_TABLE_CXX)
#$(FILES_THH) : $(GEN_TAB_INC)/St_%_Table.h : $(GEN_TAB_INC)/%.h
$(GEN_TAB_INC)/St_%_Table.h:
	$(MAKE_TABLE_H)
    endif #NOROOT
  endif #ALL_TAB
#--- compilation -
$(OBJ_DIR)/%.$(O):%.g
	test -d $(dir $(OBJ_DIR)/$(STEM)) || mkdir -p $(dir $(OBJ_DIR)/$(STEM)) && echo "mkdir -p $(dir $(OBJ_DIR)/$(STEM))"
	$(CP) $(1ST_DEPS) $(dir $(OBJ_DIR)/$(STEM)); cd $(dir $(OBJ_DIR)/$(STEM)); $(GEANT3) $(notdir $(1ST_DEPS)) -o  $(OBJ_DIR)/$(STEM).F
	$(FOR72)  $(CPPFLAGS) $(FFLAGS) -c $(OBJ_DIR)/$(STEM).F  -o  $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o: %.cxx
	test -d $(dir $(OBJ_DIR)/$(STEM)) || mkdir -p $(dir $(OBJ_DIR)/$(STEM)) && echo "mkdir -p $(dir $(OBJ_DIR)/$(STEM))"
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o: %.cc
	test -d $(dir $(OBJ_DIR)/$(STEM)) || mkdir -p $(dir $(OBJ_DIR)/$(STEM)) && echo "mkdir -p $(dir $(OBJ_DIR)/$(STEM))"
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.$(O): %.F
	test -d $(dir $(OBJ_DIR)/$(STEM)) || mkdir -p $(dir $(OBJ_DIR)/$(STEM)) && echo "mkdir -p $(dir $(OBJ_DIR)/$(STEM))"
	$(FC)  $(CPPFLAGS) $(FFLAGS) $(FEXTEND)   -c $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.$(O): %.c
	test -d $(dir $(OBJ_DIR)/$(STEM)) || mkdir -p $(dir $(OBJ_DIR)/$(STEM)) && echo "mkdir -p $(dir $(OBJ_DIR)/$(STEM))"
	$(CC)  $(CPPFLAGS) $(CFLAGS)   -c $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.$(O): %.cdf
	test -d $(dir $(OBJ_DIR)/$(STEM)) || mkdir -p $(dir $(OBJ_DIR)/$(STEM)) && echo "mkdir -p $(dir $(OBJ_DIR)/$(STEM))"
	$(KUIPC) $(KUIPC_FLAGS) $(1ST_DEPS) $(OBJ_DIR)/$(STEM).c
	$(CC)  $(CPPFLAGS) $(CFLAGS)   -c $(OBJ_DIR)/$(STEM).c $(COUT) $(OBJ_DIR)/$(STEM).$(O); \
#_______________________dependencies_________________________________
  ifneq (,$(FILES_IDM))
$(FILES_ICC) : $(GEN_DIR)/%_i.cc : %.idl
	$(CP) $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP); $(STIC) -r -q $(STICFLAGS) $(1ST_DEPS); \
	$(MV) $(STEM)_i.cc $(GEN_DIR)/; 
$(FILES_IH)  : $(GEN_INC)/%.h    : %.idl
	$(CP) $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP); $(STIC) -r -q $(STICFLAGS) $(1ST_DEPS); \
	$(MV) $(STEM).h $(GEN_INC)/; 
$(FILES_INC) : $(GEN_INC)/%.inc  : %.idl
	$(CP) $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP); $(STIC) -r -q $(STICFLAGS) $(1ST_DEPS); \
	$(MV) $(STEM).inc $(GEN_INC)/; 
$(FILES_MHH) : $(GEN_DIR_INC)/St_%_Module.h  : %.idl
	$(CP) $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP); $(STIC) -r -q $(STICFLAGS) $(1ST_DEPS); \
	$(MV) St_$(STEM)_Module.h $(GEN_DIR_INC)/; 
$(FILES_MOD) : $(GEN_DIR)/St_%_Module.cxx : %.idl
	$(CP) $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP); $(STIC) -r -q $(STICFLAGS) $(1ST_DEPS); \
	$(MV) St_$(STEM)_Module.cxx $(GEN_DIR)/; 
$(GEN_DIR)/%.didl: %.idl
	$(CP) $(1ST_DEPS) $(GEN_TMP)/ ; $(RM)  $(GEN_DIR)/$(STEM).didl; \
        $(MAKEDEPEND) -x c $(STICFLAGS) $(1ST_DEPS) | \
        sed -e 's/$(STEM)\.idl\.$(O)/$(subst .,\., $(subst /,\/, $(GEN_DIR)))\/$(STEM)\.didl/g' \
        > $(GEN_DIR)/$(STEM).didl;  
	cd $(GEN_TMP); $(STIC) -q $(STICFLAGS) $(1ST_DEPS) >>  $(GEN_DIR)/$(STEM).didl;
  endif #IDM
include $(STAR_MAKE_HOME)/MakeDep.mk
#-----cleaning------------------------------
clean: clean_obj clean_lib clean_dep
clean_share:
	rm -rf $(GEN_DIR) $(FILES_ALL_TAB)
clean_obj:
	rm -rf $(OBJ_DIR) 
clean_dep:
	rm -rf $(DEP_DIR) 
clean_lib:
	rm -rf $(DEP_DIR) $(OBJ_DIR) $(wildcard $(LIB_PKG) $(SL_PKG))
endif
endif
test: test_dir test_files test_mk
test_files:
	@echo LEVEL     = $(LEVEL)
	@echo IDLS      = $(IDLS)
	@echo FILES_IDM = $(FILES_IDM)
	@echo FILES_IDT = $(FILES_IDT)
	@echo FILES_G   = $(FILES_G)
	@echo FILES_CC  = $(FILES_CC)
	@echo FILES_CXX = $(FILES_CXX)
	@echo FILES_C   = $(FILES_C)
	@echo FILES_F   = $(FILES_F)
	@echo FILES_CA  = $(FILES_CA)
	@echo FILES_ICC = $(FILES_ICC)
	@echo FILES_IH  = $(FILES_IH)
	@echo FILES_INC = $(FILES_INC)
	@echo FILES_MOD = $(FILES_MOD)
	@echo FILES_MHH = $(FILES_MHH)
	@echo FILES_ALL_MOD = $(FILES_ALL_MOD)
	@echo FILES_TAH  = $(FILES_TAH)
	@echo FILES_TAI  = $(FILES_TAI)
	@echo FILES_TAB  = $(FILES_TAB)
	@echo FILES_THH  = $(FILES_THH)
	@echo FILES_ALL_TAB  = $(FILES_ALL_TAB)

	@echo FILES_O   = $(FILES_O)
	@echo FILES_D   = $(FILES_D)
	@echo FILES_CDF = $(FILES_CDF)
	@echo FILES_SL  = $(FILES_SL)
	@echo NAMES_IDM = $(NAMES_IDM)
	@echo NAMES_G   = $(NAMES_G)
	@echo FILES_OG  = $(FILES_OG)
	@echo NAMES_CC  = $(NAMES_CC)
	@echo NAMES_CXX = $(NAMES_CXX)
	@echo NAMES_C   = $(NAMES_C)
	@echo NAMES_F   = $(NAMES_F)
	@echo NAMES_CDF = $(NAMES_CDF)
#	@echo "OBJS     =" $(OBJS)
test_mk:
	@echo "STAR_HOST_SYS= "$(STAR_HOST_SYS)" ; OPSYS = "$(OPSYS)
	@echo "HOST      =" $(HOST)"  ; STAR_HOST_SYS = "$(STAR_HOST_SYS)
	@echo MAKE      = $(MAKE)  
	@echo VPATH     = $(VPATH) 
	@echo SHELL     = $(SHELL) 
	@echo MAKELEVEL = $(MAKELEVEL) 
	@echo MAKEFILE  = $(MAKEFILE) 
	@echo MAKEFLAGS = $(MAKEFLAGS) 
	@echo SUFFIXES  = $(SUFFIXES) 
	@echo STIC      = $(STIC) 
	@echo STICFLAGS	= $(STICFLAGS)
	@echo AR        = $(AR)
	@echo ARFLAGS 	= $(ARFLAGS)
	@echo "AS       ="$(AS)"; ASFLAGS 	="	$(ASFLAGS)
	@echo "CC       ="$(CC)"; CFLAGS 	="	$(CFLAGS)
	@echo "CXX      =" $(CXX)"	; CXXFLAGS 	="	$(CXXFLAGS)
	@echo "CPP      =" $(CPP)"	; CPPFLAGS 	="	$(CPPFLAGS)
	@echo "FC       =" $(FC)"	; FFLAGS 	="	$(FFLAGS)
	@echo "CFLAGS   =" $(CFLAGS)
	@echo "FEXTEND  =" $(FEXTEND)
	@echo "SO       =" $(SO)"	; SOFLAGS	="	$(SOFLAGS)
	@echo "FLIBS  =" $(FLIBS)" ; CLIBS	="	$(CLIBS)
	@echo "LDS      =" $(LDS)"     ; LDS_FLAGS   	="     $(LDS_FLAGS)
	@echo RM        = $(RM)
	@echo LIBRARIES = $(LIBRARIES)
	@echo CERN_LIBS = $(CERN_LIBS)
	@echo DIRS      = $(DIRS)
	@echo GEANT3    = $(GEANT3)
	@echo STIC      = $(STIC)
	@echo KUIPC     = $(KUIPC)
	@echo KUIPC_FLAGS= $(KUIPC_FLAGS)
test_dir:
	@echo CWD           = $(CWD)  
	@echo OUT_DIR       = $(OUT_DIR)
	@echo SYS_DIR       = $(SYS_DIR)
	@echo LIB_DIR       = $(LIB_DIR)
	@echo OBJ_DIR       = $(OBJ_DIR)
	@echo DEP_DIR       = $(DEP_DIR)
	@echo DIR_GEN       = $(DIR_GEN)
	@echo GEN_DIR       = $(GEN_DIR)
	@echo GEN_TAB       = $(GEN_TAB)
	@echo GEN_TAB_INC   = $(GEN_TAB_INC)
	@echo GEN_TMP       = $(GEN_TMP)
	@echo DOMAIN        = $(DOMAIN)
	@echo PGK           = $(PKG) 
	@echo SL_PGK        = $(SL_PKG)
	@echo LIB_PKG       = $(LIB_PKG)
	@echo INP_DIR       = $(INP_DIR)
	@echo DOM_DIRS      = $(DOM_DIRS)
	@echo SRC_DIR       = $(SRC_DIR)
	@echo IDL_DIRS      = $(IDL_DIRS)
	@echo INC_DIRS      = $(INC_DIRS)
	@echo SRC_DIRS      = $(SRC_DIRS)
	@echo INP_DIR       = $(INP_DIR)
	@echo sources       = $(sources)
	@echo SRC_DIRS      = $(SRC_DIRS)
	@echo FILES_init    = $(FILES_init)
	@echo STAR_MAKE_HOME= $(STAR_MAKE_HOME)
	@echo CERNLIB       = $(MAKECERNLIB)
	@echo FILES_O       = $(FILES_O) 
	@echo SRC_DIRR      = $(SRC_DIRR)
	@echo FILES_IDMS    = $(FILES_IDMS)
	@echo NAMES_IDM     = $(NAMES_IDM)
	@echo NAMES_IDMS    = $(NAMES_IDMS)
	@echo STAF_SYS_INCS = $(STAF_SYS_INCS)
	@echo DIRS_O        = $(DIRS_O)
