# $Id: MakePam.mk,v 1.65 1998/11/16 01:26:45 fisyak Exp $
# $Log: MakePam.mk,v $
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
ifndef STAF_MAKE_HOME
  STAF_MAKE_HOME := $(STAR)/mgr
endif

ASU_MALLOC_OFF :=YES

include $(STAF_MAKE_HOME)/MakeEnv.mk
include $(STAF_MAKE_HOME)/MakeArch.mk

ifdef SILENT
  .SILENT:
endif
#	INPUT DIR
ifndef INP_DIR
  INP_DIR := $(CWD)
endif

inp_dir = $(word 2,$(subst :, ,$(INP_DIR)))
ifneq (,$(inp_dir))
  override INP_DIR := $(inp_dir)
endif
ifeq (,$(strip $(filter /%,$(INP_DIR))))
  override INP_DIR := $(CWD)/$(INP_DIR)
endif

LEVEL   := $(words  $(subst /, ,$(subst $(word 1, $(subst /pams, ,$(INP_DIR))),, $(INP_DIR))))
ifeq (,$(findstring $(LEVEL),0 1))
  ifndef OUT_DIR
    OUT_DIR := $(word 1, $(subst /pams, ,$(INP_DIR)))
  endif
  ifeq (,$(strip $(filter /%,$(OUT_DIR))))
    override OUT_DIR := $(CWD)/$(OUT_DIR)
  endif


  PKG     := $(notdir $(INP_DIR))
  D       := $(subst /, ,$(subst $(OUT_DIR),,$(INP_DIR)))
  DOMAIN  := $(word 2, $(D))
  ifeq ($(DOMAIN),gen)
    DOMAIN  := $(word 3, $(D))
  endif
  ifeq ($(DOMAIN),sim)
    DOMAIN  := $(word 3, $(D))
  endif
  ifneq (,$(DOMAIN)$(PKG)) 
    SRC_DIR := $(INP_DIR)
    SYS_DIR := $(OUT_DIR)/.$(STAR_HOST_SYS)
    ifndef NODEBUG
      LIB_DIR := $(SYS_DIR)/lib
      DEP_DIR := $(SYS_DIR)/dep/$(DOMAIN)
      OBJ_DIR := $(SYS_DIR)/obj/$(DOMAIN)
    else
      LIB_DIR := $(SYS_DIR)/LIB
      DEP_DIR := $(SYS_DIR)/DEP/$(DOMAIN)
      OBJ_DIR := $(SYS_DIR)/OBJ/$(DOMAIN)
    endif
    DIR_GEN := $(OUT_DIR)/.share
    GEN_TMP := $(DIR_GEN)/tmp
    GEN_TAB := $(DIR_GEN)/tables
    GEN_DIR := $(DIR_GEN)/$(DOMAIN)
    DOM_DIRS:= $(filter-out CVS, $(notdir $(wildcard $(OUT_DIR)/pams/*)))
#.
ifndef NT
    check_out   := $(shell test -d $(OUT_DIR) || mkdir -p $(OUT_DIR)) 
    check_sys   := $(shell test -d $(SYS_DIR) || mkdir -p $(SYS_DIR)) 
    check_lib   := $(shell test -d $(LIB_DIR) || mkdir -p $(LIB_DIR))
    check_obj   := $(shell test -d $(OBJ_DIR) || mkdir -p $(OBJ_DIR))
    check_dep   := $(shell test -d $(DEP_DIR) || mkdir -p $(DEP_DIR))
    check_gen   := $(shell test -d $(DIR_GEN) || mkdir -p $(DIR_GEN))
    check_neg   := $(shell test -d $(GEN_DIR) || mkdir -p $(GEN_DIR))
    check_tab   := $(shell test -d $(GEN_TAB) || mkdir -p $(GEN_TAB))
    check_tmp   := $(shell test -d $(GEN_TMP) || mkdir -p $(GEN_TMP))
else # /* NT */
    check_out   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(OUT_DIR))))
    check_sys   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(SYS_DIR))))
    check_lib   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(LIB_DIR))))
    check_obj   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(OBJ_DIR))))
#   check_dep   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(DEP_DIR))))
    check_gen   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(DIR_GEN))))
    check_neg   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(GEN_DIR))))
    check_tab   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(GEN_TAB))))
    check_tmp   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(GEN_TMP))))
endif #/* NT */
    IDLS    := $(wildcard $(SRC_DIR)/*.idl $(SRC_DIR)/*/*.idl)
    ifneq (,$(IDLS))       
ifndef NT
      FILES_IDM := $(shell egrep -l 'interface.*:.*amiModule' $(IDLS))
endif #/* NT */
    endif
    FILES_G  := $(wildcard $(SRC_DIR)/*.g $(SRC_DIR)/*/*.g)
#_________________________________________________________________________
SUFFIXES := .c .cc .C .cxx .f .F .g .h .hpp .inc .idl
sources := $(strip $(sort $(dir $(foreach s, $(SUFFIXES), $(wildcard $(SRC_DIR)/*$(s) $(SRC_DIR)/*/*$(s) $(SRC_DIR)/*/*/*$(s))))))
SRC_DIRS:= $(subst /TAIL, ,$(addsuffix TAIL, $(sources)))
IDL_DIRS:= $(wildcard $(OUT_DIR)/pams/*/idl $(STAR)/pams/*/idl)
INC_DIRS:= $(wildcard $(OUT_DIR)/pams/*/inc $(STAR)/pams/*/inc)
VPATH   := $(wildcard $(SRC_DIRS)) $(GEN_DIR) $(GEN_TAB) $(OBJ_DIR) $(IDL_DIRS)
#-------------------------------includes----------------------------
STICFLAGS =  $(addprefix -I,  $(STAR)/inc $(SRC_DIR) $(IDL_DIRS))
ifndef NT
CPPFLAGS += -I. -I../ -I/usr/include -I$(STAR)/inc \
             $(addprefix -I, $(SRC_DIR) $(GEN_TAB) $(GEN_DIR) $(INC_DIRS)) \
            -I$(CERN_ROOT)/include
else #/* NT */

#  I had to move -I$(STAR)/inc  and  -I$(CERN_ROOT)/include to set INCLUDE to reduce the line size
  CPPFLAGS += -I../  $(addprefix -I, $(SRC_DIR) $(GEN_TAB) $(GEN_DIR) $(INC_DIRS)) 
  INCLUDE  +=  -I$(STAR)/inc -I$(CERN_ROOT)/include
endif #/* NT */
ifneq ($(OUT_DIR),$(STAR))        
CPPFLAGS +=  -I$(STAR)/.share/$(DOMAIN) -I$(STAR)/.share/tables
endif                          
FFLAGS   += -DCERNLIB_TYPE
#                                   -I$(CERN_ROOT)/src/geant321 
#                 I have idl- or g-files
FILES_CC := $(wildcard $(addsuffix /*.cc, $(SRC_DIRS)))
FILES_CXX:= $(wildcard $(addsuffix /*.cxx, $(SRC_DIR)))
FILES_C  := $(wildcard $(addsuffix /*.c , $(SRC_DIRS)))
FILES_F  := $(wildcard $(addsuffix /*.F , $(SRC_DIRS)))
FILES_CDF:= $(wildcard $(addsuffix /*.cdf , $(SRC_DIRS)))

NAMES_IDM:= $(basename $(notdir $(FILES_IDM)))
NAMES_G  := $(basename $(notdir $(FILES_G)))
NAMES_CC := $(basename $(notdir $(FILES_CC)))
NAMES_CXX:= $(filter-out %Cint St_%_Module St_%_Table,$(basename $(notdir $(FILES_CXX))))
NAMES_C  := $(basename $(notdir $(FILES_C)))
NAMES_F  := $(basename $(notdir $(FILES_F)))
NAMES_CDF:= $(basename $(notdir $(FILES_CDF)))
#.________________________  modules ____________________________________________
ifneq (,$(FILES_IDM))
FILES_ICC := $(addprefix $(GEN_DIR)/, $(subst .idl,_i.cc,  $(notdir $(FILES_IDM))))
FILES_IH  := $(addprefix $(GEN_DIR)/, $(subst .idl,.h,     $(notdir $(FILES_IDM))))
FILES_INC := $(addprefix $(GEN_DIR)/, $(subst .idl,.inc,   $(notdir $(FILES_IDM))))
FILES_MOD := $(addprefix $(GEN_DIR)/St_,$(subst .idl,_Module.cxx, $(notdir $(FILES_IDM))))
FILES_MHH := $(addprefix $(GEN_DIR)/St_,$(subst .idl,_Module.h  , $(notdir $(FILES_IDM))))
FILES_ALL_MOD := $(FILES_SYM) $(FILES_ICC) $(FILES_IH) $(FILES_INC) $(FILES_MOD) $(FILES_MHH)
#list :=  $(STIC) -T -q $(STICFLAGS) 
#FILES_IDT := $(notdir $(wildcard $(OUT_DIR)/pams/$(DOMAIN)/idl/*.idl $(STAR)/pams/$(DOMAIN)/idl/*.idl))
FILES_IDT += $(foreach IDM, $(FILES_IDM), $(shell $(STIC) -T -q $(STICFLAGS) $(IDM))) 
FILES_IDT := $(sort $(FILES_IDT))
#FILES_IDT := $(shell  for IDM in $(FILES_IDM) ;  do [$(list) $(IDM)] ; done ) 
#            for name [ in word; ] do list ; done                       
#._________________________ Tables _____________________________________________
ifneq (,$(FILES_IDT))
FILES_TAH := $(addprefix $(GEN_TAB)/, $(addsuffix .h,   $(sort $(basename $(notdir $(FILES_IDT))))))
FILES_TAI := $(addprefix $(GEN_TAB)/, $(addsuffix .inc, $(sort $(basename $(notdir $(FILES_IDT))))))
FILES_TAB := $(addprefix $(GEN_TAB)/St_, $(addsuffix _Table.cxx, $(sort $(basename $(notdir $(FILES_IDT))))))
FILES_THH := $(addprefix $(GEN_TAB)/St_, $(addsuffix _Table.h, $(sort $(basename $(notdir $(FILES_IDT))))))
FILES_ALL_TAB := $(FILES_SYT) $(FILES_TAH) $(FILES_TAI) $(FILES_TAB) $(FILES_THH)
endif    
endif
FILES_O  := $(strip $(addprefix $(OBJ_DIR)/, $(addsuffix .$(O), $(NAMES_F) $(NAMES_C) $(NAMES_CC))))
ifndef NODEPEND                
FILES_D  := $(addprefix $(DEP_DIR)/, $(addsuffix .d,   $(basename $(notdir $(FILES_O)))))
FILES_DM := $(addprefix $(GEN_DIR)/, $(addsuffix .didl, $(NAMES_IDM)))                         
endif                          
FILES_O  += $(addprefix $(OBJ_DIR)/, $(addsuffix .$(O),   $(notdir $(basename $(FILES_ICC)))))
NAMES_O   = $(notdir $(FILES_O))
# *.cc moved to sl $(NAMES_CC)
ifndef NT
ifneq (,$(strip $(FILES_IDM) $(FILES_G) $(FILES_CDF))) 
  SL_PKG  := $(LIB_DIR)/$(PKG).sl
  QWE  := $(wildcard $(SL_PKG).*)
  SL_NEW := $(SL_PKG).1000
ifdef QWE
  NQWE := $(words $(QWE))
  QWE  := $(word $(NQWE),$(QWE))
  QWE  := $(subst $(SL_PKG).,,$(QWE))
  QWE  := $(shell expr $(QWE) + 1)
endif
endif                          
ifneq (,$(strip $(FILES_O)))
LIB_PKG := $(LIB_DIR)/lib$(DOMAIN).$(A)
ifndef NT
qwe     := $(shell test ! -f $(LIB_PKG) ||  $(AR) $(ARFLAGS) $(LIB_PKG))
OBJS    := $(LIB_PKG)($(NAMES_O))
endif #/* NT */
endif
ifneq (,$(FILES_IDM))   
IDLSD    := $(wildcard $(STAR)/pams/$(DOMAIN)/*/*.idl $(STAR)/pams/$(DOMAIN)/*/*/*.idl $(STAR)/pams/$(DOMAIN)/*/*/*/*.idl)
ifndef NT
ifneq (,$(IDLSD))      
FILES_DD := $(shell egrep -l 'interface.*:.*amiModule' $(IDLSD))
NAMES_IDM+= $(basename $(notdir $(FILES_DD)))
override  NAMES_IDM := $(sort $(NAMES_IDM))
endif                           
endif #/* NT */
endif                       
ifneq (,$(NAMES_IDM))          
FILES_init  := $(addprefix $(OBJ_DIR)/, $(PKG)_init.$(O))
endif                          
ifneq (,$(NAMES_CDF))          
FILES_O    += $(addprefix $(OBJ_DIR)/, $(addsuffix .$(O), $(NAMES_CDF)))
endif                          
ifneq (,$(NAMES_G))            
FILES_OG    := $(addprefix $(OBJ_DIR)/, $(addsuffix .$(O), $(NAMES_G)))
FILES_D     += $(addprefix $(DEP_DIR)/, $(addsuffix .d,   $(basename $(notdir $(FILES_OG)))))
endif
ifneq (,$(NAMES_CC))            
FILES_SL    += $(filter-out $(FILES_o), $(addprefix $(OBJ_DIR)/, $(addsuffix .$(O), $(NAMES_CC))))
FILES_D     += $(addprefix $(DEP_DIR)/, $(addsuffix .d,   $(basename $(notdir $(FILES_CC)))))
endif                          
ifneq (,$(NAMES_CXX))            
FILES_SL    += $(filter-out $(FILES_o), $(addprefix $(OBJ_DIR)/, $(addsuffix .$(O), $(NAMES_CXX))))
FILES_D     += $(addprefix $(DEP_DIR)/, $(addsuffix .d,   $(basename $(notdir $(FILES_CXX)))))
endif                          
ifndef LIBRARIES
  LIBRARIES := $(LIB_PKG)	               
ifneq ($(STAR_PATH),$(OUT_DIR))   
  ifneq ($(LIB_PKG),)
    LIBRARIES += $(wildcard  $(STAR_LIB)/lib$(PKG).$(A))
  endif                           
endif                           
####LIBRARIES += -L$(STAR)/asps/../.$(STAR_HOST_SYS)/lib -ltls -lmsg
endif                          
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
ifndef NT
$(LIB_PKG):$(OBJS) 
#	$(AR) $(ARFLAGS) $(LIB_PKG) $(FILES_O); $(RM) $(FILES_O)
else #/* NT */
$(LIB_PKG): $(FILES_O)	
	$(AR) $(ARFLAGS) $(subst /,\\,$(subst \,/,$(LOUT)$(LIB_PKG) $(OBJ_DIR)\*.$(O)))
endif #/* NT */
endif                          
ifneq ($(strip $(FILES_SL) $(FILES_OG) $(FILES_init)),)   
$(SL_PKG): $(FILES_SL) $(FILES_OG) $(FILES_init) $(LIB_PKG)
	$(SO) $(SOFLAGS) $(FILES_SL) $(FILES_OG)  $(FILES_init)  -o $(SL_NEW) \
        $(LIBRARIES)
	$(RM) $(SL_PKG)
	$(LN) $(SL_NEW) $(SL_PKG)
	@echo "           Shared library " $(SL_PKG) " has been created"   
#--------- module --------- 
ifneq ($(NAMES_IDM),)           
$(OBJ_DIR)/$(PKG)_init.$(O): $(FILES_IDM) 
	@if [ -f $(GEN_DIR)/$(PKG)_init.cc ]; then  rm $(GEN_DIR)/$(PKG)_init.cc ; fi
	@echo '/* '$(PKG)' package interface to STAF */' > $(GEN_DIR)/$(PKG)_init.cc
	@echo '/* automatically generated file */'      >> $(GEN_DIR)/$(PKG)_init.cc
	@for p in $(NAMES_IDM); do echo $p; echo '#include "'$$p'.h"'   \
                                                        >> $(GEN_DIR)/$(PKG)_init.cc ; done
	@echo 'extern "C" int  $(PKG)_init (void);'     >> $(GEN_DIR)/$(PKG)_init.cc
	@echo 'extern "C" int  $(PKG)_start(void);'     >> $(GEN_DIR)/$(PKG)_init.cc
	@echo 'extern "C" int  $(PKG)_stop (void);'     >> $(GEN_DIR)/$(PKG)_init.cc
	@echo 'extern "C" void $(PKG)_init_();  '       >> $(GEN_DIR)/$(PKG)_init.cc
	@echo 'void $(PKG)_init_() {$(PKG)_start();}'   >> $(GEN_DIR)/$(PKG)_init.cc
	@echo 'int  $(PKG)_init () { return 1; }'       >> $(GEN_DIR)/$(PKG)_init.cc
	@echo 'int  $(PKG)_start() {'                   >> $(GEN_DIR)/$(PKG)_init.cc
	@for p in $(NAMES_IDM); do echo "      $${p}_load_ami(ami);" \
                                                        >> $(GEN_DIR)/$(PKG)_init.cc ; done
	@echo '                       return 1; }'      >> $(GEN_DIR)/$(PKG)_init.cc
	@echo 'int  $(PKG)_stop () { return 1; }'       >> $(GEN_DIR)/$(PKG)_init.cc
ifndef NT
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $(GEN_DIR)/$(PKG)_init.cc -o $(ALL_TAGS)
else #/* NT */
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(CXXOPT) $(subst \,/,-c $(CXXINP)$(GEN_DIR)/$(PKG)_init.cc $(COUT)$(ALL_TAGS))
endif #/* NT */
endif                           
endif                           # NO idl- or g-files
#_________________dependencies_____________________________
ifneq (, $(strip $(FILES_DM)))
include $(FILES_DM)
endif                               # 
ifneq (, $(strip $(FILES_D))) 
include $(FILES_D)
endif                               #
endif                            # end if of FILES_O FILES_SL
#--------  idm, idl --------
ifneq (,$(FILES_ALL_TAB))
$(FILES_TAH) : $(GEN_TAB)/%.h : %.idl
	$(CP) $(1ST_DEPS) $(GEN_TAB)/ ; cd $(GEN_TAB); $(STIC) -H -q $(STICFLAGS) $(STEM).idl; $(RM) $(STEM).idl
$(FILES_TAI) : $(GEN_TAB)/%.inc : %.idl
	$(CP) $(1ST_DEPS) $(GEN_TAB)/ ; cd $(GEN_TAB); $(STIC) -H -q $(STICFLAGS) $(STEM).idl; $(RM) $(STEM).idl
ifndef NOROOT
$(GEN_TAB)/.rootrc:
	@echo '# ROOT Environment settings are handled via the class TEnv. To see' > $(ALL_TAGS)
	@echo '# which values are active do: gEnv->Print(). '>>  $(ALL_TAGS)
 
	@echo '# Path used by dynamic loader to find shared libraries and macros '>>  $(ALL_TAGS)
	@echo '# Paths are different for Unix and Windows. The example shows the defaults'>>  $(ALL_TAGS)
	@echo '# for all ROOT applications for either Unix or Windows.'>>  $(ALL_TAGS)
	@echo 'Unix.*.Root.DynamicPath:    .:.$$(STAR_SYS)/lib:$$(STAR)/lib:$$(STAF_LIB)'>>  $(ALL_TAGS)
	@echo 'Unix.*.Root.MacroPath:      .:./StRoot/macros:$$(STAR)/StRoot/macros:$$(STAR)/StRoot/test:$$(STAR)/.share/tables:$$(ROOTSYS)/macros'>>  $(ALL_TAGS)
	@echo 'WinNT.*.Root.DynamicPath:   ./;$$(ROOTSYS)/star/bin;//Sol/afs_rhic/star/packages/dev/.intel_wnt/bin;$$(ROOTSYS);$$(ROOTSYS)/bin;$$(PATH)'>>  $(ALL_TAGS)
	@echo 'WinNT.*.Root.MacroPath:     ./;$$(home)/root/macros;$$(ROOTSYS)/tutorials;$$(ROOTSYS)/star/macros;//Sol/afs_rhic/star/packages/dev/.intel_wnt/bin;$$(ROOTSYS)/macros'>>  $(ALL_TAGS)
	@echo '# Show where library or macro is found (in path specified above)'>>  $(ALL_TAGS)
	@echo 'Root.ShowPath:           false'>>  $(ALL_TAGS)
 
	@echo '# Activate memory statistics (size and cnt is used to trap allocation of'>>  $(ALL_TAGS)
	@echo '# blocks of a certain size after cnt times)'>>  $(ALL_TAGS)
	@echo 'Root.MemStat:            0'>>  $(ALL_TAGS)
	@echo 'Root.MemStat.size:      -1'>>  $(ALL_TAGS)
	@echo 'Root.MemStat.cnt:       -1'>>  $(ALL_TAGS)
	@echo 'Root.ObjectStat:         1'>>  $(ALL_TAGS)
 
	@echo '# Rint (interactive ROOT executable) specific alias, logon and logoff macros'>>  $(ALL_TAGS)
	@echo 'Rint.Logon:              ./gentable.C'>>  $(ALL_TAGS)
	@echo 'Rint.History:             /dev/null'>>  $(ALL_TAGS)

$(FILES_TAB) : $(GEN_TAB)/St_%_Table.cxx : $(GEN_TAB)/%.h $(GEN_TAB)/.rootrc
	@echo "{" >   $(GEN_TAB)/gentable.C;
	@echo "   gSystem->Load(\"St_base\");" >> $(GEN_TAB)/gentable.C;
	@echo "#pragma Ccomment on"  >> $(GEN_TAB)/gentable.C;
	@echo "G__loadfile(\"$(GEN_TAB)/$(STEM).h\");" >> $(GEN_TAB)/gentable.C;
	@echo "St_Table tabs(\"$(STEM)\",1);" >> $(GEN_TAB)/gentable.C;
	@echo "tabs.StafStreamer();" >> $(GEN_TAB)/gentable.C;
	@echo "}" >> $(GEN_TAB)/gentable.C;
	cd $(GEN_TAB); root.exe -b -q /dev/null > /dev/null; $(RM) gentable.C
$(FILES_THH) : $(GEN_TAB)/St_%_Table.h : $(GEN_TAB)/%.h $(GEN_TAB)/.rootrc 
	@echo "{" >   $(GEN_TAB)/gentable.C;
	@echo "   gSystem->Load(\"St_base\");" >> $(GEN_TAB)/gentable.C;
	@echo "#pragma Ccomment on"  >> $(GEN_TAB)/gentable.C;
	@echo "G__loadfile(\"$(GEN_TAB)/$(STEM).h\");" >> $(GEN_TAB)/gentable.C;
	@echo "St_Table tabs(\"$(STEM)\",1);" >> $(GEN_TAB)/gentable.C;
	@echo "tabs.StafStreamer();" >> $(GEN_TAB)/gentable.C;
	@echo "}" >> $(GEN_TAB)/gentable.C;
	cd $(GEN_TAB); root.exe -b -q /dev/null > /dev/null ; $(RM) gentable.C
endif #NOROOT
endif #ALL_TAB
#--- compilation -
$(GEN_DIR)/geant3.def: $(STAR)/asps/agi/gst/geant3.def
	test -h $(GEN_DIR)/geant3.def || $(RM)  $(GEN_DIR)/geant3.def
	test -h $(GEN_DIR)/geant3.def || ln -s $(STAR)/asps/agi/gst/geant3.def  $(GEN_DIR)/geant3.def 
ifndef NT
$(FILES_OG): $(OBJ_DIR)/%.o:%.g $(GEN_DIR)/geant3.def
	cp $(1ST_DEPS) $(GEN_DIR); cd $(GEN_DIR); $(GEANT3) $(1ST_DEPS) -o  $(GEN_DIR)/$(STEM).F
	$(FOR72)  $(CPPFLAGS) $(FFLAGS) -c $(GEN_DIR)/$(STEM).F  -o  $(ALL_TAGS)
$(FILES_OBJ) $(FILES_ORJ) $(FILES_OTJ): $(OBJ_DIR)/%.o: %.cxx
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o
$(FILES_SL) : $(OBJ_DIR)/%.o: %.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o
$(LIB_PKG)(%.o): %.F
	$(FC)  $(CPPFLAGS) $(FFLAGS) $(FEXTEND)   -c $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o
	$(AR) $(ARFLAGS) $(LIB_PKG) $(OBJ_DIR)/$(STEM).o; $(RM) $(OBJ_DIR)/$(STEM).o
$(LIB_PKG)(%.o): %.c
	$(CC)  $(CPPFLAGS) $(CFLAGS)   -c $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o
	$(AR) $(ARFLAGS) $(LIB_PKG) $(OBJ_DIR)/$(STEM).o; $(RM) $(OBJ_DIR)/$(STEM).o
$(LIB_PKG)(%.o): %.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $(1ST_DEPS) -o $(OBJ_DIR)/$(STEM).o
	$(AR) $(ARFLAGS) $(LIB_PKG) $(OBJ_DIR)/$(STEM).o; $(RM) $(OBJ_DIR)/$(STEM).o
$(LIB_PKG)(%.o): %.cdf
else #/* NT */
$(FILES_OG): $(OBJ_DIR)/%.$(O):%.g $(GEN_DIR)/geant3.def
	$(CP)$(1ST_DEPS) $(GEN_DIR); cd $(GEN_DIR); $(GEANT3) $(1ST_DEPS) -o  $(GEN_DIR)/$(STEM).F
	$(FOR72) $(subst /,\\,$(subst \,/,$(CPPFLAGS) $(FFLAGS) -c $(GEN_DIR)/$(STEM).F  $(FOUT)$(ALL_TAGS)))
$(FILES_OBJ) $(FILES_ORJ) $(FILES_OTJ): $(OBJ_DIR)/%.$(O): %.cxx
	$(CXX) $(CXXFLAGS) $(CXXOPT) $(subst \,/, $(CPPFLAGS) -c $(CXXINP)$(1ST_DEPS) $(COUT)$(OBJ_DIR)/$(STEM).$(O))
$(FILES_SL) : $(OBJ_DIR)/%.$(O): %.cc
	$(CXX) $(CXXFLAGS) $(CXXOPT) $(subst \,/, $(CPPFLAGS) -c $(CXXINP)$(1ST_DEPS) $(COUT)$(OBJ_DIR)/$(STEM).$(O))
$(OBJ_DIR)/%.$(O): %.F
	$(RM) $(subst /,\\,$(subst \,/,$(OBJ_DIR)/$(STEM).$(O)))
	$(FC) $(subst /,\\,$(subst \,/,$(CPPFLAGS)  $(FFLAGS) -c $(1ST_DEPS) $(FOUT)$(OBJ_DIR)/$(STEM).$(O)))
$(OBJ_DIR)/%.$(O): %.c
	$(RM) $(subst /,\\,$(subst \,/,$(OBJ_DIR)/$(STEM).$(O)))
	$(CC) $(subst \,/,$(CPPFLAGS) $(CFLAGS) $(COPT) -c $(CINP)$(1ST_DEPS) $(COUT)$(OBJ_DIR)/$(STEM).$(O))
$(OBJ_DIR)/%.$(O): %.cc
	$(RM) $(subst /,\\,$(subst \,/,$(OBJ_DIR)/$(STEM).$(O)))
	$(CXX) $(subst \,/,$(CPPFLAGS) $(CXXFLAGS) $(CXXOPT) -c $(CXXINP)$(1ST_DEPS) $(COUT)$(OBJ_DIR)/$(STEM).$(O))
$(OBJ_DIR)/%.$(O): %.cdf
	$(RM) $(subst /,\\,$(subst \,/,$(OBJ_DIR)/$(STEM).$(O)))
endif #/* NT */
	$(KUIPC) $(KUIPC_FLAGS) $(1ST_DEPS) $(GEN_DIR)/$(STEM).c
ifndef NT
	$(CC)  $(CPPFLAGS) $(CFLAGS)   -c $(GEN_DIR)/$(STEM).c $(COUT) $(OBJ_DIR)/$(STEM).$(O); \
        $(RM)  $(GEN_DIR)/$(STEM).c
else #/* NT */
	$(CC)  $(subst \,/,$(CPPFLAGS) $(CFLAGS) $(COPT) -c $(CINP)$(GEN_DIR)/$(STEM).c $(COUT)$(OBJ_DIR)/$(STEM).$(O)) &&  \
        $(RM)  $(subst /,\\,$(subst \,/,$(GEN_DIR)/$(STEM).c))
endif #/* NT */
ifndef NT
	$(AR) $(ARFLAGS) $(LIB_PKG) $(OBJ_DIR)/$(STEM).$(O); $(RM) $(OBJ_DIR)/$(STEM).$(O)
endif #/* NT */
#_______________________dependencies_________________________________
ifneq (,$(FILES_IDM))
$(FILES_ICC) : $(GEN_DIR)/%_i.cc : %.idl
	$(CP) $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP);
	cd $(GEN_TMP); $(STIC) -r -q $(STICFLAGS) $(1ST_DEPS); 
	cd $(GEN_TMP); $(MV) $(STEM)_i.cc  $(STEM).h $(STEM).inc $(GEN_DIR)/; 
ifndef NOROOT
	cd $(GEN_TMP); $(MV) St_$(STEM)_Module.cxx  St_$(STEM)_Module.h $(GEN_DIR)/;
endif
	cd $(GEN_TMP); $(MV) *.h *.inc $(GEN_TAB)/;         $(RM) *.h *.inc *.template;
$(FILES_IH)  : $(GEN_DIR)/%.h    : %.idl
	$(CP) $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP);
	cd $(GEN_TMP); $(STIC) -r -q $(STICFLAGS) $(1ST_DEPS); 
	cd $(GEN_TMP); $(MV) $(STEM)_i.cc  $(STEM).h $(STEM).inc $(GEN_DIR)/; 
ifndef NOROOT
	cd $(GEN_TMP); $(MV) St_$(STEM)_Module.cxx  St_$(STEM)_Module.h $(GEN_DIR)/;
endif
	cd $(GEN_TMP); $(MV) *.h *.inc $(GEN_TAB)/;         $(RM) *.h *.inc *.template;
$(FILES_INC) : $(GEN_DIR)/%.inc  : %.idl
	$(CP) $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP);
	cd $(GEN_TMP); $(STIC) -r -q $(STICFLAGS) $(1ST_DEPS); 
	cd $(GEN_TMP); $(MV) $(STEM)_i.cc  $(STEM).h $(STEM).inc $(GEN_DIR)/; 
ifndef NOROOT
	cd $(GEN_TMP); $(MV) St_$(STEM)_Module.cxx  St_$(STEM)_Module.h $(GEN_DIR)/;
endif
	cd $(GEN_TMP); $(MV) *.h *.inc $(GEN_TAB)/;         $(RM) *.h *.inc *.template;
$(FILES_MHH) : $(GEN_DIR)/St_%_Module.h  : %.idl
	$(CP) $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP);
	cd $(GEN_TMP); $(STIC) -r -q $(STICFLAGS) $(1ST_DEPS); 
	cd $(GEN_TMP); $(MV) $(STEM)_i.cc  $(STEM).h $(STEM).inc $(GEN_DIR)/; 
ifndef NOROOT
	cd $(GEN_TMP); $(MV) St_$(STEM)_Module.cxx  St_$(STEM)_Module.h $(GEN_DIR)/;
endif
	cd $(GEN_TMP); $(MV) *.h *.inc $(GEN_TAB)/;         $(RM) *.h *.inc *.template;
$(FILES_MOD) : $(GEN_DIR)/St_%_Module.cxx : %.idl
	$(CP) $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP);
	cd $(GEN_TMP); $(STIC) -r -q $(STICFLAGS) $(1ST_DEPS); 
	cd $(GEN_TMP); $(MV) $(STEM)_i.cc  $(STEM).h $(STEM).inc $(GEN_DIR)/; 
ifndef NOROOT
	cd $(GEN_TMP); $(MV) St_$(STEM)_Module.cxx  St_$(STEM)_Module.h $(GEN_DIR)/;
endif
	cd $(GEN_TMP); $(MV) *.h *.inc $(GEN_TAB)/;         $(RM) *.h *.inc *.template;
$(GEN_DIR)/%.didl $(GEN_DIR)/%_i.cc $(GEN_DIR)/%.h $(GEN_DIR)/%.inc: %.idl
ifndef NT
	$(RM)  $(GEN_DIR)/$(STEM)_i.cc
	$(CP) $(1ST_DEPS) $(GEN_TMP)/ ;
	cd $(GEN_TMP); $(STIC) -q $(STICFLAGS) $(1ST_DEPS); 
	$(MAKEDEPEND)    -x c $(STICFLAGS) $(1ST_DEPS) | \
        sed -e 's/$(STEM)\.idl\.$(O)/$(subst .,\., $(subst /,\/, $(GEN_DIR)))\/$(STEM)\.didl/g' \
        > $(GEN_DIR)/$(STEM).didl; 
	cd $(GEN_TMP); $(STIC) -q $(STICFLAGS) $(1ST_DEPS) >>  $(GEN_DIR)/$(STEM).didl; 
	cd $(GEN_TMP); $(MV) $(STEM)_i.cc  $(STEM).h $(STEM).inc $(GEN_DIR)/; 
	cd $(GEN_TMP); $(MV) *.h *.inc $(GEN_TAB)/;         $(RM) *.h *.inc *.template;
else
	@echo PLEASE, Run this make with UNIX first !
endif #/* NT */
endif #IDM
$(DEP_DIR)/%.d:%.cc 
	$(MAKEDEPEND)  $(CPPFLAGS) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.$(O)/$(subst .,\.,$(subst /,\/,$(LIB_PKG)($(STEM).$(O)))) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
$(DEP_DIR)/%.d:%.c
	$(MAKEDEPEND)  $(CPPFLAGS) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.$(O)/$(subst .,\.,$(subst /,\/,$(LIB_PKG)($(STEM).$(O)))) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
$(DEP_DIR)/%.d:%.F
	$(MAKEDEPEND) -traditional -x c  $(CPPFLAGS) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.F\.$(O)/$(subst .,\.,$(subst /,\/,$(LIB_PKG)($(STEM).$(O)))) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
$(DEP_DIR)/%.d:%.g
	$(MAKEDEPEND) -traditional -x c  $(CPPFLAGS) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.g\.$(O)/$(subst .,\.,$(subst /,\/,$(LIB_PKG)($(STEM).$(O)))) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
$(DEP_DIR)/%.d:%.cdf
	cd $(SRC_DIR); \
        echo "$(notdir $(STEM)).c $(ALL_TAGS): $(ALL_DEPS)" > $(ALL_TAGS) ;
        echo "$(STEM).$(O): $(STEM).c" >> $(ALL_TAGS)
#-----test variables------------------------
endif
endif
endif
#-----cleaning------------------------------
clean: clean_obj clean_lib clean_dep
clean_share:
ifndef NT
	rm -rf $(GEN_DIR) $(FILES_ALL_TAB)
endif #/* NT */
clean_obj:
ifndef NT
	rm -rf $(OBJ_DIR) 
else #/* NT */
	if exist $(subst /,\\,$(subst \,/,$(OBJ_DIR)/. )) $(RMDIR) $(subst /,\\,$(subst \,/,$(OBJ_DIR) ))
endif #/* NT */
clean_dep:
ifndef NT
	rm -rf $(DEP_DIR) 
endif #/* NT */
clean_lib:
ifndef NT
	rm -rf $(SL_PKG) $(LIB_PKG)
endif #/* NT */
endif
test: test_dir test_files test_mk
test_files:
	@echo LEVEL     = $(LEVEL)
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
	@echo "HOST      =" $(HOST)"  ; STAR_SYS = "$(STAR_SYS)
	@echo MAKE      = $(MAKE)  
	@echo VPATH     = $(VPATH) 
	@echo SHELL     = $(SHELL) 
	@echo MAKE      = $(MAKE) 
	@echo MAKELEVEL = $(MAKELEVEL) 
	@echo MAKEFILE  = $(MAKEFILE) 
	@echo MAKFLAGS  = $(MAKEFLAGS) 
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
	@echo STAF_MAKE_HOME= $(STAF_MAKE_HOME)
	@echo CERNLIB       = $(MAKECERNLIB)
	@echo FILES_O       = $(FILES_O) 
