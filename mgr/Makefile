#  $Log: Makefile,v $
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
#             Last modification $Date: 1998/01/31 23:32:51 $ 
#.SILENT:
ALL_DEPS  = $^
FIRST_DEP = $<
ALL_TAGS  = $@
STEM      = $*
STIC     = stic
PWD      = /bin/pwd
CWD     := $(shell $(PWD))
EMPTY   :=
ifndef MAKEFILE
  MAKEFILE = $(ROOT_STAR)/mgr/Makefile
endif
ifndef INP_DIR
  override INP_DIR := $(CWD)
endif
NAME    := $(notdir $(INP_DIR))
# define level pams -> domain -> package from *.idlm and *.g files
SRC_DIR := $(INP_DIR)
SUBDIRS :=
# default is domain
FILES_IDM := $(shell egrep -l 'interface.*:.*amiModule' $(SRC_DIR)/*/*.idlm) 
FILES_G  := $(wildcard $(SRC_DIR)/*/*.g)
ifneq ($(EMPTY),$(strip $(FILES_IDM) $(FILES_G)))  # domain level ?
ROOT    := $(shell cd $(INP_DIR)/../../; $(PWD))
DOM_DIR := $(shell cd $(INP_DIR); $(PWD))
sources := $(SRC_DIR)/*
else # module level
FILES_IDM := $(shell egrep -l 'interface.*:.*amiModule' $(SRC_DIR)/*.idlm)  
FILES_G  := $(wildcard $(SRC_DIR)/*.g)
ifneq ($(EMPTY),$(strip $(FILES_IDM) $(FILES_G)))  # module level
ROOT    := $(shell cd $(INP_DIR)/../../../; $(PWD))
DOM_DIR := $(shell cd $(INP_DIR)/../; $(PWD))
sources := $(SRC_DIR)
else # pams level
FILES_IDM := $(shell egrep -l 'interface.*:.*amiModule' $(SRC_DIR)/*/*/*.idlm)  
FILES_G  := $(wildcard $(SRC_DIR)/*/*/*.g)
ifneq ($(EMPTY),$(strip $(FILES_IDM) $(FILES_G)))  # pam level
DIRS    := $(strip $(wildcard *))
SUBDIRS := $(foreach dir, $(DIRS), $(shell test -d $(dir) && echo $(dir))) 
SUBDIRS := $(filter-out sys, $(SUBDIRS))
SUBDIRS := $(filter-out inc, $(SUBDIRS))
SUBDIRS := $(filter-out idl, $(SUBDIRS))
SUBDIRS := $(filter-out doc, $(SUBDIRS))
SUBDIRS := $(filter-out CVS, $(SUBDIRS))
SUBDIRS := $(filter-out wrk, $(SUBDIRS))
SUBDIRS := $(filter-out src, $(SUBDIRS))
SUBDIRS := $(filter-out exa, $(SUBDIRS))
SUBDIRS := $(strip $(sort $(SUBDIRS)))
endif                                              # pam level
endif                                              # module level
endif                                              # domain level     
ifeq ($(EMPTY),$(SUBDIRS))
#          I have no subdrs
# all files:
ifeq ($(EMPTY),$(strip $(FILES_IDM) $(FILES_G)))
#                 I have NO idlm- and NO g-files
.PHONY               : default
all:
	@echo "Nothing to do for domain/module" $(NAME)
else
ifndef OUT_DIR
  override OUT_DIR := $(shell cd $(ROOT); $(PWD))/lib
endif
DOMAIN  := $(notdir $(DOM_DIR))
DOM_DIRS:= $(addprefix $(ROOT)/pams/,$(shell cd $(INP_DIR)/../; ls))
IDL_DIRS:= $(wildcard $(ROOT)/pams/*/idl $(ROOT_STAR)/pams/*/idl)
INC_DIRS:= $(wildcard $(ROOT)/pams/*/inc $(ROOT_STAR)/pams/*/inc)
LIB_DIR := $(OUT_DIR)/$(SYS_HOST_STAR)
OBJ_DIR := $(LIB_DIR)/$(DOMAIN).obj
DIR_GEN := $(OUT_DIR)/share
GEN_DIR := $(OUT_DIR)/share/$(DOMAIN).gen
VPATH   := $(wildcard $(sources)) $(GEN_DIR) $(OBJ_DIR) $(IDL_DIRS)
#                 I have idlm- or g-files
FILES_CC := $(wildcard $(sources)/*.cc)
FILES_C  := $(wildcard $(sources)/*.c)
FILES_F  := $(wildcard $(sources)/*.F)

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
MKDEPFLAGS:= -traditional -MG -MM -x c
ifndef NODEPEND
FILES_D  :=                          $(addsuffix .d,   $(basename $(FILES_O)))
endif
FILES_O  += $(addprefix $(OBJ_DIR)/, $(addsuffix .o,   $(NAMES_G)))
FILES_O  += $(addprefix $(OBJ_DIR)/, $(addsuffix .o,   $(notdir $(basename $(FILES_CA)))))
ifneq (,$(NAMES_IDM))
FILES_O  += $(addprefix $(OBJ_DIR)/, $(NAME)_init.o)                      
endif
ifndef NODEPEND
FILES_DM := $(addprefix $(GEN_DIR)/, $(addsuffix .didlm, $(NAMES_IDM)))                         
endif
#
#-------------------------------includes----------------------------
STICFLAGS =  $(addprefix -I, $(SRC_DIR) $(IDL_DIRS))
ifneq ($(SYS_STAR),hp_ux102)
CPPFLAGS  = -D$(SYS_STAR) -D$(shell uname) 
endif
CPPFLAGS +=              -I. -I/usr/include \
             $(addprefix -I, $(SRC_DIR) $(GEN_DIR) $(INC_DIRS)) 
FFLAGS   += -DCERNLIB_TYPE -I$(CERN_ROOT)/src -I$(CERN_ROOT)/src/geant321
ifndef NODEBUG
FFLAGS   += -g
CFLAGS   += -g
CXXFLAGS += -g
CPPFLAGS += -DDEBUG
endif
#ifndef CERN_LIBS
#   CERN_LIBS := -L$(CERN_ROOT)/lib -lmathlib -lkernlib
    CERN_LIBS := $(shell cernlib mathlib kernlib)
#endif
ifndef LIBRARIES
  LIBRARIES :=  -L$(ROOT_STAR)/staf/$(SYS_HOST_STAR) \
                -L$(LIB_DIR) -L$(LIB_STAR)  -l$(DOMAIN)   -lmsg  -ltls  
#                                                                
#                                            
endif
#-------------------------------rules-------------------------------
# phony - not a file
.PHONY               : workdirs  $(NAME) depend clean test
all                  : workdirs $(GEN_DIR)/$(NAME)_init.cc $(NAME)  
$(NAME)              : $(LIB_DIR)/$(NAME).sl
$(LIB_DIR)/$(NAME).sl: $(FILES_O) 
	$(AR) $(ARFLAGS) $(LIB_DIR)/lib$(DOMAIN).a $(FILES_O)
	$(LD) $(LDFLAGS) $(FILES_O) -o $(LIB_DIR)/$(NAME).sl $(LIBRARIES) $(CERN_LIBS) $(LD_LIBS) $(CC_LIBS) 
#hpux	$(LD) $(CPPFLAGS) $(CXXFLAGS) $(GEN_DIR)/$(NAME)_init.cc $(LDFLAGS) $(FILES_O) -o $(LIB_DIR)/$(NAME).sl $(LIBRARIES) 
#--------- module ---------
$(GEN_DIR)/$(NAME)_init.cc: $(GEN_DIR) $(FILES_IDM) 
	@if [ -f $(ALL_TAGS) ]; then  rm $(ALL_TAGS) ; fi
	@echo '/* '$(NAME)' package interface to STAF */' > $(ALL_TAGS)
	@echo '/* automatically generated file */'       >> $(ALL_TAGS)
ifneq ($(NAMES_IDM),)
	@for p in $(NAMES_IDM); do echo $p; echo '#include "'$$p'.h"'   >> $(ALL_TAGS) ; done
	@echo 'extern "C" int  $(NAME)_init (void);'     >> $(ALL_TAGS)
	@echo 'extern "C" int  $(NAME)_start(void);'     >> $(ALL_TAGS)
	@echo 'extern "C" int  $(NAME)_stop (void);'     >> $(ALL_TAGS)
	@echo 'extern "C" void $(NAME)_init_();  '       >> $(ALL_TAGS)
	@echo 'void $(NAME)_init_() {$(NAME)_start();}'  >> $(ALL_TAGS)
	@echo 'int  $(NAME)_init () { return 1; }'       >> $(ALL_TAGS)
	@echo 'int  $(NAME)_start() {'                   >> $(ALL_TAGS)
	@for p in $(NAMES_IDM); do echo "      $${p}_load_ami(ami);" >> $(ALL_TAGS) ; done
	@echo '                       return 1; }'       >> $(ALL_TAGS)
	@echo 'int  $(NAME)_stop () { return 1; }'       >> $(ALL_TAGS)
endif    #

#--------  idm, idl --------
$(GEN_DIR)/%.h $(GEN_DIR)/%.inc %.h %.inc: %.idl
	cd $(GEN_DIR); $(STIC) $(STICFLAGS) $(ALL_DEPS)
#--- compilation -
$(OBJ_DIR)/%.o: %.g
	test -h $(GEN_DIR)/geant3.def || ln -s $(ROOT_STAR)/bin/share/geant3.def  $(GEN_DIR)/geant3.def
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
$(GEN_DIR)/%.didlm $(GEN_DIR)/%_i.cc $(GEN_DIR)/%.h $(GEN_DIR)/%.inc: %.idlm 
	test -d $(GEN_DIR) || mkdir -p $(GEN_DIR)
	cd $(GEN_DIR); $(STIC) $(STICFLAGS) $(FIRST_DEP); \
        gcc  $(MKDEPFLAGS) $(STICFLAGS) $(FIRST_DEP) | \
        sed -e 's/.idlm.o/.didlm/g' > $(GEN_DIR)/$(STEM).didlm
	$(STIC) -M  $(STICFLAGS) $(FIRST_DEP) | grep ":" >> $(GEN_DIR)/$(STEM).didlm
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
clean:
	rm -rf $(OBJ_DIR) $(GEN_DIR) $(LIB_DIR)/$(NAME).sl $(LIB_DIR)/$(DOMAIN).a
#-----test variables------------------------
#-----dependencies--------------------------
ifneq ($(EMPTY), $(strip $(FILES_D)))
include $(FILES_D)
endif       #
ifneq ($(EMPTY), $(strip $(FILES_DM)))
include $(FILES_DM)
endif       # 
endif # NO idlm- or g-files
test: 
	@echo "ROOT      =" $(ROOT)
	@echo "CWD       =" $(CWD)  
	@echo "NAME      =" $(NAME) 
	@echo "DOMAIN    =" $(DOMAIN)
	@echo "INP_DIR   =" $(INP_DIR)
	@echo "DOM_DIR   =" $(DOM_DIR)
	@echo "DOM_DIRS  =" $(DOM_DIRS)
	@echo "OUT_DIR   =" $(OUT_DIR)
	@echo "SRC_DIR   =" $(SRC_DIR)
	@echo "IDL_DIR   =" $(IDL_DIR)
	@echo "IDL_DIRS  =" $(IDL_DIRS)
	@echo "INC_DIRS  =" $(INC_DIRS)
	@echo "LIB_DIR   =" $(LIB_DIR)
	@echo "OBJ_DIR   =" $(OBJ_DIR)
	@echo "GEN_DIR   =" $(GEN_DIR)
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
	@echo "MAKEFILES =" $(MAKEFILES)
	@echo "VPATH     =" $(VPATH)
	@echo "SHELL     =" $(SHELL)
	@echo "MAKE      =" $(MAKE)
	@echo "MAKELEVEL =" $(MAKELEVEL)
	@echo "MAKEFILE  =" $(MAKEFILE) 
	@echo "MAKFLAGS  =" $(MAKEFLAGS)
	@echo "SUFFIXES  =" $(SUFFIXES)
	@echo "STIC      =" $(STIC)	"; STICFLAGS	="	$(STICFLAGS)
	@echo "AR        =" $(AR)	"; ARFLAGS 	="	$(ARFLAGS)
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
