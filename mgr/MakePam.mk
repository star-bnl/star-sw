#  $Log: MakePam.mk,v $
#  Revision 1.6  1998/04/04 14:45:50  fisyak
#  Fix bug with geant3.def
#
#  Revision 1.5  1998/03/27 14:32:54  fisyak
#  Simplify MakePam
#
#  Revision 1.3  1998/03/23 02:31:42  fisyak
#  move staff in group_dir
#
#  Revision 1.2  1998/03/09 14:36:30  fisyak
#  Switch varibales
#
#  Revision 1.1  1998/03/09 13:31:50  fisyak
#  Remove environment Variables
#
#  Revision 1.13  1998/02/22 02:08:25  fisyak
#  CPPFLAGS for HPX
#
#  Revision 1.12  1998/02/21 18:33:54  fisyak
#  Add util domain
#
#  Revision 1.11  1998/02/21 00:58:31  fisyak
#  Add tls library
#
#  Revision 1.10  1998/02/18 22:58:03  fisyak
#  Move staf
#
#  Revision 1.9  1998/02/17 18:06:46  fisyak
#  Add dropit for PATH
#
#  Revision 1.8  1998/02/14 02:08:20  fisyak
#  Add silent mode for make
#
#  Revision 1.7  1998/02/13 14:18:19  fisyak
#  Simplify Makefile, reduce SLibrary
#
#  Revision 1.5  1998/02/10 00:06:07  fisyak
#  SL98a second version
#
#  Revision 1.4  1998/01/31 23:32:51  fisyak
#  New Environment variables
#
#  Revision 1.3  1998/01/30 12:42:14  fisyak
#  Save changes before moving to SL97b
#
#  Revision 1.2  1998/01/01 03:28:11  fisyak
#  New make.kumac
#
#  Revision 1.1.1.1  1997/12/31 14:35:23  fisyak
#
#           Last modification $Date: 1998/04/04 14:45:50 $ 
#  #. default setings
include $(STAR)/mgr/MakeSYS.mk
PWD       = /bin/pwd
CWD      := $(shell $(PWD))
ifdef SILENT
.SILENT:
endif       
ifndef MAKEFILE
MAKEFILE = $(STAR)/mgr/MakePam.mk
endif          
ifndef INP_DIR 
INP_DIR := $(CWD)
endif           
NAME    := $(notdir $(INP_DIR))
# define level pams -> domain -> package from *.idl and *.g files
#======================= level ===========================
PAMS    :=pams
pams    :=$(findstring $(PAMS),$(INP_DIR))
LEVEL   := $(words  $(subst /, ,$(subst $(word 1, $(subst /pams, ,$(INP_DIR))),, $(INP_DIR))))
ifeq ($(LEVEL),$(ZERO))
	ROOT    :=$(INP_DIR)
	SUBDIRS :=$(shell test -d pams && echo pams)
else
	DIRS    := $(strip $(wildcard *))
	SUBDIRS := $(foreach dir, $(DIRS), $(shell test -d $(dir) && echo $(dir))) 
	SUBDIRS := $(filter-out inc, $(SUBDIRS))
	SUBDIRS := $(filter-out idl, $(SUBDIRS))
	SUBDIRS := $(filter-out doc, $(SUBDIRS))
	SUBDIRS := $(filter-out CVS, $(SUBDIRS))
	SUBDIRS := $(filter-out wrk, $(SUBDIRS))
	SUBDIRS := $(filter-out src, $(SUBDIRS))
	SUBDIRS := $(filter-out exa, $(SUBDIRS))
	SUBDIRS := $(strip    $(sort $(SUBDIRS)))
#       SUBDIRS := $(filter util, $(SUBDIRS)) $(filter-out util, $(SUBDIRS))
        SUBDIRS := $(filter-out util, $(SUBDIRS))
endif
ifeq ($(LEVEL), $(ONE))  #pams level
	ROOT    := $(shell cd $(INP_DIR)/../; $(PWD))
endif
ifeq ($(LEVEL), $(TWO))  #default is domain
	ROOT    := $(shell cd $(INP_DIR)/../../; $(PWD))
	DOM_DIR := $(CWD)
	PKG     := $(notdir $(DOM_DIR))
ifeq (gen,$(PKG))              
	PKG     :=
endif                          
endif
ifeq ($(LEVEL), $(THREE)) #package level
	ROOT    := $(shell cd $(INP_DIR)/../../../; $(PWD))
	DOM_DIR := $(shell cd $(INP_DIR)/../; $(PWD))
	PKG     := $(NAME)
	SUBDIRS:=
endif                       
ifeq ($(LEVEL),$(FOUR)) #subpackage level
	ROOT    := $(shell cd $(INP_DIR)/../../../../; $(PWD))
	DOM_DIR := $(shell cd $(INP_DIR)/../../; $(PWD))
	PKG     := $(notdir $(shell cd $(INP_DIR)/../; $(PWD)))
endif                          
ifndef OUT_DIR                 
	override OUT_DIR := $(shell cd $(ROOT); $(PWD))/lib
endif                          
ifeq ($(NAME),$(PKG))          
	SUBDIRS :=
endif                          
ifneq ($(LEVEL)$(SUBDIRS),$(ZERO)) 
ifneq ($(EMPTY),$(SUBDIRS))     
#          I have subdrs
.PHONY               : all test clean clean_lib clean_share clean_obj
#      I_have_subdirs
all:  $(addsuffix _all, $(SUBDIRS))
%_all:
	$(MAKE) -f $(MAKEFILE) -C $(STEM) all 
test: $(addsuffix _test, $(SUBDIRS))
%_test: 
	$(MAKE) -f $(MAKEFILE) -C $(STEM) test 
clean: $(addsuffix _clean, $(SUBDIRS))
%_clean: 
	$(MAKE) -f $(MAKEFILE) -C $(STEM) clean 
clean_lib: $(addsuffix _clean_lib, $(SUBDIRS))
%_clean_lib: 
	$(MAKE) -f $(MAKEFILE) -C $(STEM) clean_lib 
clean_share: $(addsuffix _clean_share, $(SUBDIRS))
%_clean_share: 
	$(MAKE) -f $(MAKEFILE) -C $(STEM) clean_share 
clean_obj: $(addsuffix _clean_obj, $(SUBDIRS))
%_clean_obj: 
	$(MAKE) -f $(MAKEFILE) -C $(STEM) clean 
else # I have no subdirs
SRC_DIR := $(INP_DIR)
IDLS    := $(wildcard $(SRC_DIR)/*.idl $(SRC_DIR)/*/*.idl)
ifneq ($(EMPTY),$(IDLS))       
FILES_IDM := $(shell egrep -l 'interface.*:.*amiModule' $(IDLS))
endif                          
FILES_G  := $(wildcard $(SRC_DIR)/*.g $(SRC_DIR)/*/*.g)
#=========================================================
ifeq ($(LEVEL),$(FOUR))        
.PHONY               : default
all:
	@echo "Please run make in parent directory"
else                           
ifndef RANLIB                  
override RANLIB := /bin/true
endif                          
ROOTD   := $(shell cd $(ROOT)/..; $(PWD) )
LIB_DIR := $(OUT_DIR)/$(STAR_HOST_SYS)
DOMAIN  := $(notdir $(DOM_DIR))
OBJ_DIR := $(LIB_DIR)/$(DOMAIN).obj
DIR_GEN := $(OUT_DIR)/share
GEN_DIR := $(OUT_DIR)/share/$(DOMAIN).gen
DOM_DIRS:= $(filter-out CVS, $(shell cd $(ROOT)/$(PAMS); ls))
#.
check_out   := $(shell test -d $(OUT_DIR) || mkdir $(OUT_DIR)) 
check_lib   := $(shell test -d $(LIB_DIR) || mkdir $(LIB_DIR))
check_obj   := $(shell test -d $(OBJ_DIR) || mkdir $(OBJ_DIR))
check_gen   := $(shell test -d $(DIR_GEN) || mkdir $(DIR_GEN))
check_neg   := $(shell test -d $(GEN_DIR) || mkdir $(GEN_DIR))
#.
sources := $(strip $(sort $(dir $(wildcard $(SRC_DIR)/*.* $(SRC_DIR)/*/*.*))))
SRC_DIRS:= $(subst /TAIL, ,$(addsuffix TAIL, $(sources)))
IDL_DIRS:= $(wildcard $(ROOT)/$(PAMS)/*/idl $(STAR)/$(PAMS)/*/idl)
INC_DIRG:= $(wildcard $(STAR)/lib/share/*.gen)
INC_DIRS:= $(wildcard $(ROOT)/$(PAMS)/*/inc $(STAR)/$(PAMS)/*/inc)
VPATH   := $(wildcard $(SRC_DIRS)) $(GEN_DIR) $(OBJ_DIR)
VPATH   += $(IDL_DIRS)
#VPATH   := $(filter-out $(DOM_DIR)/idl, $(VPATH))
#                 I have idl- or g-files
FILES_CC := $(wildcard $(addsuffix /*.cc, $(SRC_DIRS)))
FILES_C  := $(wildcard $(addsuffix /*.c , $(SRC_DIRS)))
FILES_F  := $(wildcard $(addsuffix /*.F , $(SRC_DIRS)))
FILES_CDF:= $(wildcard $(addsuffix /*.cdf , $(SRC_DIRS)))

NAMES_IDM:= $(basename $(notdir $(FILES_IDM)))
NAMES_G  := $(basename $(notdir $(FILES_G)))
NAMES_CC := $(basename $(notdir $(FILES_CC)))
NAMES_C  := $(basename $(notdir $(FILES_C)))
NAMES_F  := $(basename $(notdir $(FILES_F)))
NAMES_CDF:= $(basename $(notdir $(FILES_CDF)))

FILES_I  := $(addprefix $(GEN_DIR)/, $(addsuffix .inc, $(NAMES_IDM)))
FILES_H  := $(addprefix $(GEN_DIR)/, $(addsuffix .h,   $(NAMES_IDM)))
FILES_CA := $(addprefix $(GEN_DIR)/, $(addsuffix _i.cc,$(NAMES_IDM)))
FILES_O  := $(strip \
            $(addprefix $(OBJ_DIR)/, $(addsuffix .o,   $(NAMES_F) \
                                                       $(NAMES_C) $(NAMES_CC))))
ifeq ($(DOMAIN),gen)           
ifneq ($(PKG),$(EMPTY))         
		PKG_LIB := lib$(PKG).a
endif                           
else                           
ifneq ($(DOMAIN),$(EMPTY))      
		PKG_LIB := lib$(DOMAIN).a
endif                           
endif                          
ifneq ($(FILES_O),$(EMPTY))    
LIB_PKG := $(LIB_DIR)/$(PKG_LIB) 
endif                          
ifneq ($(EMPTY),$(PKG))        
	PKG_SL  := $(PKG).sl
ifneq ($(EMPTY),$(strip $(FILES_IDM) $(FILES_G) $(FILES_CDF))) 
        SL_PKG  := $(LIB_DIR)/$(PKG_SL)
endif                           
endif                          
MKDEPFLAGS:= -traditional -MG -MM -w -x c
ifndef NODEPEND                
FILES_D  :=                          $(addsuffix .d,   $(basename $(FILES_O)))
FILES_DM := $(addprefix $(GEN_DIR)/, $(addsuffix .didl, $(NAMES_IDM)))                         
endif                          
FILES_O  += $(addprefix $(OBJ_DIR)/, $(addsuffix .o,   $(notdir $(basename $(FILES_CA)))))
NAMES_O   = $(notdir $(FILES_O))
ifeq ($(LEVEL),$(TWO))          # domain level: add all domain IDM
ifneq ($(EMPTY),$(FILES_IDM))   
IDLSD    := $(wildcard $(STAR)/$(PAMS)/$(DOMAIN)/*/*.idl $(STAR)/$(PAMS)/$(DOMAIN)/*/*/*.idl)
ifneq ($(EMPTY),$(IDLSD))      
FILES_DD := $(shell egrep -l 'interface.*:.*amiModule' $(IDLSD))
NAMES_IDM+= $(basename $(notdir $(FILES_DD)))
override  NAMES_IDM := $(sort $(NAMES_IDM))
endif                          
endif                           
endif                          
ifneq (,$(NAMES_IDM))          
FILES_SL  := $(addprefix $(OBJ_DIR)/, $(PKG)_init.o)
endif                          
ifneq (,$(NAMES_CDF))          
FILES_SL  += $(addprefix $(OBJ_DIR)/, $(addsuffix .o, $(NAMES_CDF)))
endif                          
ifneq (,$(NAMES_G))            
FILES_SL  += $(addprefix $(OBJ_DIR)/, $(addsuffix .o, $(NAMES_G)))
endif                          
#-------------------------------includes----------------------------
STICFLAGS =  $(addprefix -I,  $(STAR)/asps/staf/inc $(SRC_DIR) $(IDL_DIRS))
ifneq ($(STAR_SYS),hp_ux102)   
CPPFLAGS += -D$(STAR_SYS) $(strip -D$(shell uname)) 
endif                          
CPPFLAGS += -I. -I../ -I/usr/include -I$(STAR)/asps/staf/inc \
             $(addprefix -I, $(SRC_DIR) $(GEN_DIR) $(INC_DIRS)) -I$(CERN_ROOT)/src/cfortran
ifneq ($(ROOT),$(STAR))        
CPPFLAGG :=  $(addprefix -I, $(INC_DIRG))
endif                          
FFLAGS   += -DCERNLIB_TYPE -I$(CERN_ROOT)/src -I$(CERN_ROOT)/src/geant321 -I$(CERN_ROOT)/src/packlib/zebra  -I$(CERN_ROOT)/src/graflib/dzdoc 
ifndef NODEBUG                 
FFLAGS   += -g
CFLAGS   += -g
CXXFLAGS += -g
CPPFLAGS += -DDEBUG
endif                          
ifeq ($(EMPTY),$(findstring $(STAR_HOST_SYS),hp_ux102 hp_ux102_aCC))
ifndef CERN_LIBS               
    CERN_LIBS := $(shell cernlib mathlib kernlib)
endif
else
    CERN_LIBS :=
endif                          
ifndef LIBRARIES
		LIBRARIES := $(LIB_PKG)	               
ifneq ($(STAR_PATH),$(ROOTD))   
ifneq ($(LIB_PKG),$(EMPTY))
		LIBRARIES += $(shell test -f $(LIB_PKG) && echo $(LIB_PKG)) -L$(STAR_LIB)
endif                           
endif                           
LIBRARIES += -L$(STAR)/asps/../.$(STAR_HOST_SYS)/lib -ltls -lmsg
endif                          
#-------------------------------rules-------------------------------
# phony - not a file
.PHONY               : $(PKG) depend clean test
all                  : $(PKG)  
# all files:
ifneq ($(EMPTY),$(strip $(FILES_O) $(FILES_SL))) 
#                 I have NO idl- and NO g-files
$(PKG)               : $(SL_PKG) $(LIB_PKG)
ifneq ($(FILES_O),$(EMPTY))    
$(LIB_PKG): $(FILES_O)
	$(AR) $(ARFLAGS) $(LIB_PKG) $(FILES_O)
	@echo "          Library " $(LIB_PKG) " has been updated"
endif                          
ifneq ($(FILES_SL),$(EMPTY))   
$(SL_PKG): $(FILES_SL) $(LIB_PKG)
	$(LD) $(LDFLAGS) $(FILES_SL) -o $(SL_PKG) \
        $(LIBRARIES) $(CERN_LIBS) $(LD_LIBS) $(CC_LIBS) 
	@echo "          Shared library " $(SL_PKG) " has been created"
#--------- module ---------
ifneq ($(NAMES_IDM),)           
$(OBJ_DIR)/$(PKG)_init.o: $(FILES_IDM) 
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
	$(CXX) $(CPPFLAGS) $(CPPFLAGG) $(CXXFLAGS) -c $(GEN_DIR)/$(PKG)_init.cc -o $(ALL_TAGS)
endif                           
endif                           # NO idl- or g-files
#-----cleaning------------------------------
clean: clean_obj clean_lib
clean_share:
	rm -rf $(GEN_DIR) 
clean_obj:
	rm -rf $(OBJ_DIR) 
clean_lib:
	rm -rf $(SL_PKG) $(LIB_PKG)
#-----dependencies--------------------------
ifneq ($(EMPTY), $(strip $(FILES_D))) 
include $(FILES_D)
endif                               #
ifneq ($(EMPTY), $(strip $(FILES_DM)))
include $(FILES_DM)
endif                               # 
endif                            # end if of FILES_O FILES_SL
endif       # LEVEL 4
#--------  idm, idl --------
$(GEN_DIR)/%.h $(GEN_DIR)/%.inc %.h %.inc: %.idl
	cp  $(FIRST_DEP) $(GEN_DIR)/ ; cd $(GEN_DIR); $(STIC) $(STICFLAGS) $(FIRST_DEP); $(RM) $(STEM).idl
#--- compilation -
$(GEN_DIR)/geant3.def: $(STAR)/asps/agi/gst/geant3.def
	test -h $(GEN_DIR)/geant3.def || $(RM)  $(GEN_DIR)/geant3.def
	test -h $(GEN_DIR)/geant3.def || ln -s $(STAR)/asps/agi/gst/geant3.def  $(GEN_DIR)/geant3.def 
$(OBJ_DIR)/%.o:%.g $(GEN_DIR)/geant3.def
#	test -h $(GEN_DIR)/geant3.def || ln -s $(STAR)/asps/agi/gst/geant3.def  $(GEN_DIR)/geant3.def
	cp $(FIRST_DEP) $(GEN_DIR); cd $(GEN_DIR); geant3 $(FIRST_DEP) -o  $(GEN_DIR)/$(STEM).f 
	$(FC) $(FFLAGS) -c $(GEN_DIR)/$(STEM).f  -o  $(ALL_TAGS)
$(OBJ_DIR)/%.o: %.F
	$(FC)  $(CPPFLAGS) $(FFLAGS) $(F_EXTENDED)   -c $(FIRST_DEP) -o $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o: %.c
	$(CC)  $(CPPFLAGS) $(CFLAGS)   -c $(FIRST_DEP) -o $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o: %.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $(FIRST_DEP) -o $(OBJ_DIR)/$(STEM).o
$(OBJ_DIR)/%.o: %.cdf
	$(KUIPC) $(KUIPC_FLAGS) $(FIRST_DEP) $(GEN_DIR)/$(STEM).c
	$(CC)  $(CPPFLAGS) $(CFLAGS)   -c $(GEN_DIR)/$(STEM).c -o $(OBJ_DIR)/$(STEM).o; \
        $(RM)  $(GEN_DIR)/$(STEM).c
#-----dependencies-------
$(GEN_DIR)/%.didl $(GEN_DIR)/%_i.cc $(GEN_DIR)/%.h $(GEN_DIR)/%.inc: %.idl
	cp  $(FIRST_DEP) $(GEN_DIR)/ ; cd $(GEN_DIR);\
        $(STIC) $(STICFLAGS) $(FIRST_DEP); \
        gcc  $(MKDEPFLAGS) $(STICFLAGS) $(FIRST_DEP) | \
        sed -e 's/.idl.o/.didl/g' > $(GEN_DIR)/$(STEM).didl; \
        $(STIC) -M  $(STICFLAGS) $(FIRST_DEP) | grep ":" \
        >> $(GEN_DIR)/$(STEM).didl; $(RM) $(STEM).idl
#       temporarly, until stic is fixed:
	@sed -e 's/broker->newInvoker(\(.*\),/broker->deleteInvoker(\1); broker->newInvoker(\1,/' \
                $(GEN_DIR)/$(STEM)_i.cc > temp
	@mv  -f temp $(GEN_DIR)/$(STEM)_i.cc
$(OBJ_DIR)/%.d: %.cc 
	gcc $(MKDEPFLAGS) $(CPPFLAGS) $(FIRST_DEPQ) | \
        sed -e 's/$(notdir $(STEM)).o/$(subst /,\/,$(OBJ_DIR)/$(STEM).o) $(subst /,\/,$(ALL_TAGS))/g'\
        > $(ALL_TAGS)
$(OBJ_DIR)/%.d: %.c
	gcc $(MKDEPFLAGS) $(CPPFLAGS) $(FIRST_DEP) | \
        sed -e 's/$(notdir $(STEM)).o/$(subst /,\/,$(OBJ_DIR)/$(STEM).o) $(subst /,\/,$(ALL_TAGS))/g'\
        > $(ALL_TAGS)
$(OBJ_DIR)/%.d: %.F
	gcc  $(MKDEPFLAGS) $(CPPFLAGS) $(FIRST_DEP) | \
        sed -e 's/$(notdir $(STEM)).o/$(subst /,\/,$(OBJ_DIR)/$(STEM).o) $(subst /,\/,$(ALL_TAGS))/g'\
        > $(ALL_TAGS)
$(OBJ_DIR)/%.d: %.cdf
	cd $(SRC_DIR); \
        echo "$(notdir $(STEM)).c $(ALL_TAGS): $(ALL_DEPS)" > $(ALL_TAGS) ;
        echo "$(STEM).o: $(STEM).c" >> $(ALL_TAGS)
endif                           # end if of SUBDIR loop
#-----test variables------------------------
test: test_dir test_files test_mk
test_files:
	@echo "FILES_IDM =" $(FILES_IDM)
	@echo "FILES_G   =" $(FILES_G)
	@echo "FILES_CC  =" $(FILES_CC)
	@echo "FILES_C   =" $(FILES_C)
	@echo "FILES_F   =" $(FILES_F)
	@echo "FILES_I   =" $(FILES_I)
	@echo "FILES_H   =" $(FILES_H)
	@echo "FILES_CA  =" $(FILES_CA)
	@echo "FILES_O   =" $(FILES_O)
	@echo "FILES_D   =" $(FILES_D)
	@echo "FILES_CDF =" $(FILES_CDF)
	@echo "FILES_SL  =" $(FILES_SL)
	@echo "NAMES_IDM =" $(NAMES_IDM)
	@echo "NAMES_G   =" $(NAMES_G)
	@echo "NAMES_CC  =" $(NAMES_CC)
	@echo "NAMES_C   =" $(NAMES_C)
	@echo "NAMES_F   =" $(NAMES_F)
	@echo "NAMES_CDF =" $(NAMES_CDF)
test_mk:
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
test_dir:
	@echo "CWD       =" $(CWD)  
	@echo "ROOT      =" $(ROOT) "; ROOTD = " $(ROOTD)
	@echo "DOMAIN    =" $(DOMAIN)
	@echo "NAME      =" $(NAME) 
	@echo "PGK       =" $(PKG) 
	@echo "PKG_SL    =" $(PKG_SL)
	@echo "PKG_LIB   =" $(PKG_LIB)
	@echo "SL_PGK    =" $(SL_PKG)
	@echo "LIB_PKG   =" $(LIB_PKG)
	@echo "INP_DIR   =" $(INP_DIR)
	@echo "DOM_DIR   =" $(DOM_DIR)
	@echo "DOM_DIRS  =" $(DOM_DIRS)
	@echo "OUT_DIR   =" $(OUT_DIR)
	@echo "SRC_DIR   =" $(SRC_DIR)
	@echo "IDL_DIRS  =" $(IDL_DIRS)
	@echo "INC_DIRS  =" $(INC_DIRS)
	@echo "LIB_DIR   =" $(LIB_DIR)
	@echo "OBJ_DIR   =" $(OBJ_DIR)
	@echo "GEN_DIR   =" $(GEN_DIR)
	@echo "SRC_DIRS  =" $(SRC_DIRS)
	@echo "INP_DIR   =" $(INP_DIR)
	@echo "LEVEL     =" $(LEVEL)
	@echo "SUBDIRS   =" $(SUBDIRS)
	@echo "sources   =" $(sources)
	@echo "SRC_DIRS  =" $(SRC_DIRS)
	@echo "IDLS      =" $(IDLS)
	@echo "IDLSD     =" $(IDLSD)
	@echo "FILES_DD  =" $(FILES_DD)
else
.PHONY               : default
all:
	@echo "No PAMS. Take standard libraries"
endif                           # LEVEL=0 







