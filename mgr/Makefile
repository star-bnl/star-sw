#  $Log: Makefile,v $
#  Revision 1.6  1998/02/12 13:35:09  fisyak
#  Add versioning, new Makefile with domain/package libraries
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
#             Last modification $Date: 1998/02/12 13:35:09 $ 
#.SILENT:
ALL_DEPS  = $^
FIRST_DEP = $<
ALL_TAGS  = $@
STEM      = $*
STIC     = stic
PWD      = /bin/pwd
CWD     := $(shell $(PWD))
EMPTY   :=
FOUR    :=4
LBRAKET :="("
RBRAKET :=")"
ifndef MAKEFILE
	MAKEFILE = $(STAR)/mgr/Makefile
endif
ifndef INP_DIR
	override INP_DIR := $(CWD)
endif
NAME    := $(notdir $(INP_DIR))
# define level pams -> domain -> package from *.idl and *.g files
#======================= level ===========================
PAMS    :=pams
DIR_LIST:= $(subst /, ,$(INP_DIR)) 
pams    := $(findstring $(PAMS),$(DIR_LIST))
ifeq ($(EMPTY),$(pams)) # ROOT level
	LEVEL   :=0
	ROOT:=$(INP_DIR)
	SUBDIRS :=$(PAMS)
else
	LIST    := $(DIR_LIST)
	N       := $(words $(LIST))
	SN      := $(word  $(N), $(DIR_LIST))
	DIRS    := $(strip $(wildcard *))
	SUBDIRS := $(foreach dir, $(DIRS), $(shell test -d $(dir) && echo $(dir))) 
	SUBDIRS := $(filter-out staf,$(SUBDIRS))
	SUBDIRS := $(filter-out sys, $(SUBDIRS))
	SUBDIRS := $(filter-out inc, $(SUBDIRS))
	SUBDIRS := $(filter-out idl, $(SUBDIRS))
	SUBDIRS := $(filter-out doc, $(SUBDIRS))
	SUBDIRS := $(filter-out CVS, $(SUBDIRS))
	SUBDIRS := $(filter-out wrk, $(SUBDIRS))
	SUBDIRS := $(filter-out src, $(SUBDIRS))
	SUBDIRS := $(filter-out exa, $(SUBDIRS))
	SUBDIRS := $(strip $(sort $(SUBDIRS)))
ifeq ($(PAMS), $(strip $(SN)))                #pams level
		LEVEL   :=1
		ROOT    := $(shell cd $(INP_DIR)/../; $(PWD))
else                             #domain level
		LIST    := $(filter-out $(SN), $(LIST))
		N       := $(words  $(LIST))
		SN_1    := $(word  $(N), $(DIR_LIST))
ifeq ($(PAMS), $(strip $(SN_1))) 
			LEVEL   :=2
#                       default is domain
			ROOT    := $(shell cd $(INP_DIR)/../../; $(PWD))
			DOM_DIR := $(CWD)
			PKG     := $(notdir $(DOM_DIR))
ifeq (gen,$(PKG))
				PKG     :=
endif
else                             #package level
			LIST    := $(filter-out $(SN_1), $(LIST))
			N       := $(words  $(LIST))
			SN_2    := $(word  $(N), $(DIR_LIST))
			PKG     := $(NAME)
ifeq ($(PAMS),$(strip $(SN_2))) #subpackage level
				LEVEL   :=3
				ROOT    := $(shell cd $(INP_DIR)/../../../; $(PWD))
				DOM_DIR := $(shell cd $(INP_DIR)/../; $(PWD))
				SUBDIRS:=
else
				LIST    := $(filter-out $(SN_2), $(LIST))
				N       := $(words  $(LIST))
				SN_3    := $(word  $(N), $(DIR_LIST))
ifeq ($(PAMS),$(strip $(SN_3)))
				LEVEL   :=4
				ROOT    := $(shell cd $(INP_DIR)/../../../../; $(PWD))
				DOM_DIR := $(shell cd $(INP_DIR)/../../; $(PWD))
				PKG     := $(notdir $(shell cd $(INP_DIR)/../; $(PWD)))
else
				LEVEL   :=-1
				ROOT    :=$(INP_DIR)
				SUBDIRS :=$(PAMS)
endif
endif
endif
endif
endif
ifndef OUT_DIR
	override OUT_DIR := $(shell cd $(ROOT); $(PWD))/lib
endif
ifeq ($(NAME),$(PKG))
	SUBDIRS :=
endif
LIB_DIR := $(OUT_DIR)/$(SYS_HOST_STAR)
DOMAIN  := $(notdir $(DOM_DIR))
OBJ_DIR := $(LIB_DIR)/$(DOMAIN).obj
DIR_GEN := $(OUT_DIR)/share
GEN_DIR := $(OUT_DIR)/share/$(DOMAIN).gen
DOM_DIRS:= $(filter-out CVS, $(shell cd $(ROOT)/pams; ls))
#.
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
ifeq ($(EMPTY),$(SUBDIRS))
#          I have no subdrs
# all files:
ifeq ($(EMPTY),$(strip $(FILES_IDM) $(FILES_G)))
#                 I have NO idl- and NO g-files
.PHONY               : default
all:
	@echo "Nothing to do for domain/module" $(PKG)
else
#                                                       $(IDL_DIRS)
ifndef RANLIB
			override RANLIB := /bin/true
endif
#.
sources := $(strip $(sort $(dir $(wildcard $(SRC_DIR)/*.* $(SRC_DIR)/*/*.*))))
SRC_DIRS:= $(subst /TAIL, ,$(addsuffix TAIL, $(sources)))
IDL_DIRS:= $(wildcard $(ROOT)/pams/*/idl $(STAR)/pams/*/idl)
INC_DIRS:= $(wildcard $(ROOT)/pams/*/inc $(STAR)/pams/*/inc)
VPATH   := $(wildcard $(SRC_DIRS)) $(GEN_DIR) $(OBJ_DIR)
VPATH   := $(filter-out $(DOM_DIR)/idl, $(VPATH))
#                 I have idl- or g-files
FILES_CC := $(wildcard $(addsuffix /*.cc, $(SRC_DIRS)))
FILES_C  := $(wildcard $(addsuffix /*.c , $(SRC_DIRS)))
FILES_F  := $(wildcard $(addsuffix /*.F , $(SRC_DIRS)))

NAMES_IDM:= $(basename $(notdir $(FILES_IDM)))
NAMES_G  := $(basename $(notdir $(FILES_G)))
NAMES_CC := $(basename $(notdir $(FILES_CC)))
NAMES_C  := $(basename $(notdir $(FILES_C)))
NAMES_F  := $(basename $(notdir $(FILES_F)))

FILES_I  := $(addprefix $(GEN_DIR)/, $(addsuffix .inc, $(NAMES_IDM)))
FILES_H  := $(addprefix $(GEN_DIR)/, $(addsuffix .h,   $(NAMES_IDM)))
FILES_CA := $(addprefix $(GEN_DIR)/, $(addsuffix _i.cc,$(NAMES_IDM)))
FILES_O  := $(addprefix $(OBJ_DIR)/, $(addsuffix .o,   $(NAMES_F) $(NAMES_f) \
                                                       $(NAMES_C) $(NAMES_CC)))
ifeq ($(DOMAIN),gen)
ifneq ($(PKG),$(EMPTY))
		PKG_LIB := lib$(PKG).a
endif
else
ifneq ($(DOMAIN),$(EMPTY))
		PKG_LIB := lib$(DOMAIN).a
endif
endif
ifneq ($(EMPTY),$(PKG))
	PKG_SL  := $(PKG).sl
endif
MKDEPFLAGS:= -traditional -MG -MM -x c
ifndef NODEPEND
FILES_D  :=                          $(addsuffix .d,   $(basename $(FILES_O)))
FILES_DM := $(addprefix $(GEN_DIR)/, $(addsuffix .didl, $(NAMES_IDM)))                         
endif
FILES_O  += $(addprefix $(OBJ_DIR)/, $(addsuffix .o,   $(NAMES_G)))
FILES_O  += $(addprefix $(OBJ_DIR)/, $(addsuffix .o,   $(notdir $(basename $(FILES_CA)))))
ifneq (,$(NAMES_IDM))
FILES_K  := $(addprefix $(OBJ_DIR)/, $(PKG)_init.o)
FILES_O  += $(FILES_K)
endif
#-------------------------------includes----------------------------
STICFLAGS =  $(addprefix -I, $(SRC_DIR) $(IDL_DIRS))
ifneq ($(SYS_STAR),hp_ux102)
CPPFLAGS  = -D$(SYS_STAR) -D$(shell uname) 
endif
CPPFLAGS +=              -I. -I../ -I/usr/include \
             $(addprefix -I, $(SRC_DIR) $(GEN_DIR) $(INC_DIRS)) 
FFLAGS   += -DCERNLIB_TYPE -I$(CERN_ROOT)/src -I$(CERN_ROOT)/src/geant321
ifndef NODEBUG
FFLAGS   += -g
CFLAGS   += -g
CXXFLAGS += -g
CPPFLAGS += -DDEBUG
endif
ifndef CERN_LIBS
    CERN_LIBS := $(shell cernlib mathlib kernlib)
endif
ifndef LIBRARIES
ifeq ($(LIB_STAR),$(LIB_DIR))
		LIBRARIES :=  $(LIB_STAR)/$(PKG_LIB)  -L$(STAR)/staf/$(SYS_HOST_STAR)  -lmsg   
else
		LIBRARIES :=  $(LIB_DIR)/$(PKG_LIB) $(LIB_STAR)/$(PKG_LIB) -L$(STAR)/staf/$(SYS_HOST_STAR) -lmsg   
endif
endif
#-------------------------------rules-------------------------------
# phony - not a file
.PHONY               : workdirs  $(PKG) depend clean test
all                  : workdirs $(GEN_DIR)/$(PKG)_init.cc $(PKG)  
$(PKG)               : $(LIB_DIR)/$(PKG_SL)
$(LIB_DIR)/lib$(PKG).a $(LIB_DIR)/$(PKG_SL): $(FILES_O) 
#	test -f $(LIB_DIR)/lib$(PKG).a || $(AR) $(ARFLAGS) $(LIB_DIR)/$(PKG_LIB)
	$(AR) $(ARFLAGS) $(LIB_DIR)/$(PKG_LIB) $(FILES_O)
#	$(RANLIB)        $(LIB_DIR)/$(PKG_LIB)
	$(LD) $(LDFLAGS) $(FILES_O) -o $(LIB_DIR)/$(PKG_SL) \
         $(LIBRARIES) $(CERN_LIBS) $(LD_LIBS) $(CC_LIBS) 
#--------- module ---------
$(GEN_DIR)/$(PKG)_init.cc: $(GEN_DIR) $(FILES_IDM) 
	@if [ -f $(ALL_TAGS) ]; then  rm $(ALL_TAGS) ; fi
	@echo '/* '$(PKG)' package interface to STAF */' > $(ALL_TAGS)
	@echo '/* automatically generated file */'       >> $(ALL_TAGS)
ifneq ($(NAMES_IDM),)
	@for p in $(NAMES_IDM); do echo $p; echo '#include "'$$p'.h"'   >> $(ALL_TAGS) ; done
	@echo 'extern "C" int  $(PKG)_init (void);'     >> $(ALL_TAGS)
	@echo 'extern "C" int  $(PKG)_start(void);'     >> $(ALL_TAGS)
	@echo 'extern "C" int  $(PKG)_stop (void);'     >> $(ALL_TAGS)
	@echo 'extern "C" void $(PKG)_init_();  '       >> $(ALL_TAGS)
	@echo 'void $(PKG)_init_() {$(PKG)_start();}'  >> $(ALL_TAGS)
	@echo 'int  $(PKG)_init () { return 1; }'       >> $(ALL_TAGS)
	@echo 'int  $(PKG)_start() {'                   >> $(ALL_TAGS)
	@for p in $(NAMES_IDM); do echo "      $${p}_load_ami(ami);" >> $(ALL_TAGS) ; done
	@echo '                       return 1; }'       >> $(ALL_TAGS)
	@echo 'int  $(PKG)_stop () { return 1; }'       >> $(ALL_TAGS)
endif    #

#--------  idm, idl --------
#$(GEN_DIR)/%.h $(GEN_DIR)/%.inc %.h %.inc: %.idl
#	cd $(GEN_DIR); $(STIC) $(STICFLAGS) $(ALL_DEPS)
#--- compilation -
$(OBJ_DIR)/%.o: %.g
	test -h $(GEN_DIR)/geant3.def || ln -s $(STAR)/bin/share/geant3.def  $(GEN_DIR)/geant3.def
	cd $(GEN_DIR); geant3    $(FIRST_DEP)  -o  $(GEN_DIR)/$(STEM).f 
	$(FC) $(FFLAGS) -c $(GEN_DIR)/$(STEM).f  -o  $(ALL_TAGS)

$(OBJ_DIR)/%.o: %.F
	$(FC)  $(CPPFLAGS) $(FFLAGS) $(F_EXTENDED)   -c $(FIRST_DEP) -o $(ALL_TAGS)

$(OBJ_DIR)/%.o: %.c
	$(CC)  $(CPPFLAGS) $(CFLAGS)   -c $(FIRST_DEP) -o $(ALL_TAGS)

$(OBJ_DIR)/%.o: %.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $(FIRST_DEP) -o $(ALL_TAGS)

#-----working dirs-------
workdirs:   $(GEN_DIR)  $(LIB_DIR)  $(OBJ_DIR) 
$(DIR_GEN): $(OUT_DIR)
$(GEN_DIR): $(DIR_GEN)
$(LIB_DIR): $(OUT_DIR)
$(OBJ_DIR): $(LIB_DIR)
$(OUT_DIR) $(DIR_GEN) $(GEN_DIR) $(LIB_DIR)  $(OBJ_DIR):
	@test -d $(ALL_TAGS) ||  mkdir -p $(ALL_TAGS)
#-----dependencies-------
$(GEN_DIR)/%.didl $(GEN_DIR)/%_i.cc $(GEN_DIR)/%.h $(GEN_DIR)/%.inc: %.idl
	test -d $(GEN_DIR) || mkdir -p $(GEN_DIR)
	cd $(GEN_DIR); $(STIC) $(STICFLAGS) $(FIRST_DEP); \
        gcc  $(MKDEPFLAGS) $(STICFLAGS) $(FIRST_DEP) | \
        sed -e 's/.idl.o/.didl/g' > $(GEN_DIR)/$(STEM).didl
	$(STIC) -M  $(STICFLAGS) $(FIRST_DEP) | grep ":" >> $(GEN_DIR)/$(STEM).didl
#       temporarly, until stic is fixed:
	@sed -e 's/broker->newInvoker(\(.*\),/broker->deleteInvoker(\1); broker->newInvoker(\1,/' \
                $(GEN_DIR)/$(STEM)_i.cc > temp
	@mv  -f temp $(GEN_DIR)/$(STEM)_i.cc
$(OBJ_DIR)/%.d: %.cc 
	test -d $(OBJ_DIR) || mkdir -p $(OBJ_DIR);
	gcc $(MKDEPFLAGS) $(CPPFLAGS) $(FIRST_DEP) | \
        sed -e 's/$(notdir $(STEM)).o/$(subst /,\/,$(OBJ_DIR)/$(STEM)).o $(subst /,\/,$(ALL_TAGS))/g' > $(ALL_TAGS)
$(OBJ_DIR)/%.d: %.c
	test -d $(OBJ_DIR) || mkdir -p $(OBJ_DIR);
	gcc $(MKDEPFLAGS) $(CPPFLAGS) $(FIRST_DEP) | \
        sed -e 's/$(notdir $(STEM)).o/$(subst /,\/,$(OBJ_DIR)/$(STEM)).o $(subst /,\/,$(ALL_TAGS))/g' > $(ALL_TAGS)
$(OBJ_DIR)/%.d: %.F
	test -d $(OBJ_DIR) || mkdir -p $(OBJ_DIR);
	gcc  $(MKDEPFLAGS) $(CPPFLAGS) $(FIRST_DEP) | \
        sed -e 's/$(notdir $(STEM)).o/$(subst /,\/,$(OBJ_DIR)/$(STEM)).o $(subst /,\/,$(ALL_TAGS))/g' > $(ALL_TAGS)
$(OBJ_DIR)/%.d: %.cdf
	cd $(SRC_DIR); \
        echo "$(notdir $(STEM)).c $(ALL_TAGS): $(ALL_DEPS)" > $(ALL_TAGS) ;/
        echo "$(STEM).o: $(STEM).c" >> $(ALL_TAGS)
#-----cleaning------------------------------
clean: clean_obj clean_lib
clean_share:
	rm -rf $(GEN_DIR) 
clean_obj:
	rm -rf $(OBJ_DIR) 
clean_lib:
	rm -rf $(LIB_DIR)/$(PKG_SL) $(LIB_DIR)/$(DOMAIN).a
#-----dependencies--------------------------
ifneq ($(EMPTY), $(strip $(FILES_D)))
include $(FILES_D)
endif       #
ifneq ($(EMPTY), $(strip $(FILES_DM)))
include $(FILES_DM)
endif       # 
endif # NO idl- or g-files
else
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
endif # end if of SUBDIR loop
endif # LEVEL 4
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
	@echo "NAMES_IDM =" $(NAMES_IDM)
	@echo "NAMES_G   =" $(NAMES_G)
	@echo "NAMES_CC  =" $(NAMES_CC)
	@echo "NAMES_C   =" $(NAMES_C)
	@echo "NAMES_F   =" $(NAMES_F)
test_mk:
	@echo "MAKEFILES =" $(MAKEFILES)
	@echo "VPATH     =" $(VPATH)
	@echo "SHELL     =" $(SHELL)
	@echo "MAKE      =" $(MAKE)
	@echo "MAKELEVEL =" $(MAKELEVEL)
	@echo "MAKEFILES =" $(MAKEFILES)"; MAKEFILE     ="      $(MAKEFILE)
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
	@echo "DIRS      =" $(DIRS)
test_dir:
	@echo "CWD       =" $(CWD)  
	@echo "ROOT      =" $(ROOT)
	@echo "DOMAIN    =" $(DOMAIN)
	@echo "NAME      =" $(NAME) 
	@echo "PGK       =" $(PKG) 
	@echo "PKG_SL    =" $(PKG_SL)
	@echo "PKG_LIB   =" $(PKG_LIB)
	@echo "INP_DIR   =" $(INP_DIR)
	@echo "DOM_DIR   =" $(DOM_DIR)
	@echo "DOM_DIRS  =" $(DOM_DIRS)
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
	@echo "DIR_LIST  =" $(DIR_LIST)
	@echo "LIST      =" $(LIST)
	@echo "LEVEL     =" $(LEVEL)
	@echo "SUBDIRS   =" $(SUBDIRS)
	@echo "sources   =" $(sources)
	@echo "SRC_DIRS  =" $(SRC_DIRS)