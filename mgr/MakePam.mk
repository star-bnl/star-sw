ifndef STAF_MAKE_HOME
  STAF_MAKE_HOME := $(STAR)/mgr
endif

include $(STAF_MAKE_HOME)/MakeEnv.mk
include $(STAF_MAKE_HOME)/MakeArch.mk

ifdef SILENT
  .SILENT:
endif
#	INPUT DIR
ifndef INP_DIR
  INP_DIR := $(CWD)
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
    LIB_DIR := $(SYS_DIR)/lib
    OBJ_DIR := $(SYS_DIR)/obj/$(DOMAIN)
    DEP_DIR := $(SYS_DIR)/dep/$(DOMAIN)
    DIR_GEN := $(OUT_DIR)/.share
    GEN_TMP := $(DIR_GEN)/tmp
    GEN_TAB := $(DIR_GEN)/tables
    GEN_DIR := $(DIR_GEN)/$(DOMAIN)
    DOM_DIRS:= $(filter-out CVS, $(shell cd $(OUT_DIR)/pams/; ls))
#.
    check_out   := $(shell test -d $(OUT_DIR) || mkdir -p $(OUT_DIR)) 
    check_sys   := $(shell test -d $(SYS_DIR) || mkdir -p $(SYS_DIR)) 
    check_lib   := $(shell test -d $(LIB_DIR) || mkdir -p $(LIB_DIR))
    check_obj   := $(shell test -d $(OBJ_DIR) || mkdir -p $(OBJ_DIR))
    check_dep   := $(shell test -d $(DEP_DIR) || mkdir -p $(DEP_DIR))
    check_gen   := $(shell test -d $(DIR_GEN) || mkdir -p $(DIR_GEN))
    check_gen   := $(shell test -d $(GEN_TMP) || mkdir -p $(GEN_TMP))
    check_neg   := $(shell test -d $(GEN_DIR) || mkdir -p $(GEN_DIR))
    check_tab   := $(shell test -d $(GEN_TAB) || mkdir -p $(GEN_TAB))
    check_tmp   := $(shell test -d $(GEN_TMP) || mkdir -p $(GEN_TMP))
    IDLS    := $(wildcard $(SRC_DIR)/*.idl $(SRC_DIR)/*/*.idl)
    ifneq (,$(IDLS))       
      FILES_IDM := $(shell egrep -l 'interface.*:.*amiModule' $(IDLS))
    endif
    FILES_G  := $(wildcard $(SRC_DIR)/*.g $(SRC_DIR)/*/*.g)
#_________________________________________________________________________
SUFFIXES := .c .cc .C .cxx .f .F .g .h .hpp .inc .idl
sources := $(strip $(sort $(dir $(foreach s, $(SUFFIXES), $(wildcard $(SRC_DIR)/*$(s) $(SRC_DIR)/*/*$(s) $(SRC_DIR)/*/*/*$(s))))))
SRC_DIRS:= $(subst /TAIL, ,$(addsuffix TAIL, $(sources)))
IDL_DIRS:= $(sort $(wildcard $(OUT_DIR)/pams/*/idl $(STAR)/pams/*/idl))
INC_DIRS:= $(sort $(wildcard $(OUT_DIR)/pams/*/inc $(STAR)/pams/*/inc))
VPATH   := $(wildcard $(SRC_DIRS)) $(GEN_DIR) $(GEN_TAB) $(OBJ_DIR) $(IDL_DIRS)
#-------------------------------includes----------------------------
STIC       := $(STAR_BIN)/stic
GEANT3     := $(STAR_BIN)/geant3
STICFLAGS =  $(addprefix -I,  $(STAR)/asps/staf/inc $(SRC_DIR) $(IDL_DIRS))
ifneq ($(STAR_SYS),hp_ux102)   
CPPFLAGS += -D$(STAR_SYS) $(strip -D$(shell uname)) 
endif                          
CPPFLAGS += -I. -I../ -I/usr/include -I$(STAR)/asps/staf/inc \
             $(addprefix -I, $(SRC_DIR) $(GEN_TAB) $(GEN_DIR) $(INC_DIRS)) \
            -I$(CERN_ROOT)/include
ifneq ($(OUT_DIR),$(STAR))        
CPPFLAGG :=  -I$(STAR)/.share/$(DOMAIN) -I$(STAR)/.share/tables
endif                          
FFLAGS   += -DCERNLIB_TYPE
#                                   -I$(CERN_ROOT)/src/geant321 
ifndef NODEBUG                 
FFLAGS   += -g
CFLAGS   += -g
CXXFLAGS += -g
CPPFLAGS += -DDEBUG
else
FFLAGS   += -O
CFLAGS   += -O
CXXFLAGS += -O
endif                          
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
FILES_IDT := $(foreach IDM, $(FILES_IDM), $(shell $(STIC) -T -q $(STICFLAGS) $(IDM))) 
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
FILES_O  := $(strip $(addprefix $(OBJ_DIR)/, $(addsuffix .o, $(NAMES_F) $(NAMES_C))))
# *.cc moved to sl $(NAMES_CC)
ifneq ($(FILES_O),)    
	LIB_PKG := $(LIB_DIR)/lib$(PKG).a
	qwe     := $(shell test ! -f $(LIB_PKG) ||  $(AR) $(ARFLAGS) $(LIB_PKG))
endif                          
ifneq (,$(strip $(FILES_IDM) $(FILES_G) $(FILES_CDF))) 
        SL_PKG  := $(LIB_DIR)/$(PKG).sl
endif                          
MKDEPFLAGS:= -MG -MM -w -nostdinc
ifndef NODEPEND                
FILES_D  := $(addprefix $(DEP_DIR)/, $(addsuffix .d,   $(basename $(notdir $(FILES_O)))))
FILES_DM := $(addprefix $(GEN_DIR)/, $(addsuffix .didl, $(NAMES_IDM)))                         
endif                          
FILES_O  += $(addprefix $(OBJ_DIR)/, $(addsuffix .o,   $(notdir $(basename $(FILES_ICC)))))
NAMES_O   = $(notdir $(FILES_O))
ifneq (,$(NAMES_O))
OBJS     := $(LIB_PKG)($(NAMES_O))
endif
ifneq (,$(FILES_IDM))   
IDLSD    := $(wildcard $(STAR)/pams/$(DOMAIN)/*/*.idl $(STAR)/pams/$(DOMAIN)/*/*/*.idl $(STAR)/pams/$(DOMAIN)/*/*/*/*.idl)
ifneq (,$(IDLSD))      
FILES_DD := $(shell egrep -l 'interface.*:.*amiModule' $(IDLSD))
NAMES_IDM+= $(basename $(notdir $(FILES_DD)))
override  NAMES_IDM := $(sort $(NAMES_IDM))
endif                           
endif                          
ifneq (,$(NAMES_IDM))          
FILES_init  := $(addprefix $(OBJ_DIR)/, $(PKG)_init.o)
endif                          
ifneq (,$(NAMES_CDF))          
FILES_O    += $(addprefix $(OBJ_DIR)/, $(addsuffix .o, $(NAMES_CDF)))
endif                          
ifneq (,$(NAMES_G))            
FILES_OG    := $(addprefix $(OBJ_DIR)/, $(addsuffix .o, $(NAMES_G)))
FILES_D     += $(addprefix $(DEP_DIR)/, $(addsuffix .d,   $(basename $(notdir $(FILES_OG)))))
endif
ifneq (,$(NAMES_CC))            
FILES_SL  += $(filter-out $(FILES_o), $(addprefix $(OBJ_DIR)/, $(addsuffix .o, $(NAMES_CC))))
endif                          
ifeq (,$(findstring $(STAR_HOST_SYS),hp_ux102 hp_ux102_aCC))
ifndef CERN_LIBS               
    CERN_LIBS := $(shell cernlib mathlib packlib kernlib)
endif
else
    CERN_LIBS :=
endif                          
ifndef LIBRARIES
  LIBRARIES := $(LIB_PKG)	               
ifneq ($(STAR_PATH),$(OUT_DIR))   
  ifneq ($(LIB_PKG),)
    LIBRARIES += $(wildcard  $(STAR_LIB)/lib$(PKG).a)
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
MakeInc  : $(FILES_ALL_TAB) $(FILES_ALL_MOD) 
# all files:
ifneq (,$(strip $(FILES_O) $(FILES_SL) $(FILES_OG) $(FILES_init))) 
#                 I have NO idl- and NO g-files
ifneq ($(FILES_O),)    
$(LIB_PKG):$(OBJS) 
#	$(AR) $(ARFLAGS) $(LIB_PKG) $(FILES_O); $(RM) $(FILES_O)
endif                          
ifneq ($(strip $(FILES_SL) $(FILES_OG) $(FILES_init)),)   
$(SL_PKG): $(FILES_SL) $(FILES_OG) $(FILES_init) $(LIB_PKG)
	$(SO) $(SOFLAGS) $(FILES_SL) $(FILES_OG)  $(FILES_init)  -o $(SL_PKG) \
        $(LIBRARIES)  
	@echo "           Shared library " $(SL_PKG) " has been created"   
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
#_________________dependencies_____________________________
ifneq (, $(strip $(FILES_D))) 
include $(FILES_D)
endif                               #
ifneq (, $(strip $(FILES_DM)))
include $(FILES_DM)
endif                               # 
endif                            # end if of FILES_O FILES_SL
#--------  idm, idl --------
ifneq (,$(FILES_ALL_TAB))
$(FILES_TAH) : $(GEN_TAB)/%.h : %.idl
	cp  $(1ST_DEPS) $(GEN_TAB)/ ; cd $(GEN_TAB); $(STIC) -H -q $(STICFLAGS) $(STEM).idl; $(RM) $(STEM).idl
$(FILES_TAI) : $(GEN_TAB)/%.inc : %.idl
	cp  $(1ST_DEPS) $(GEN_TAB)/ ; cd $(GEN_TAB); $(STIC) -H -q $(STICFLAGS) $(STEM).idl; $(RM) $(STEM).idl
$(FILES_TAB) : $(GEN_TAB)/St_%_Table.cxx : %.idl
	cp  $(1ST_DEPS) $(GEN_TAB)/ ; cd $(GEN_TAB); $(STIC) -r -H -q $(STICFLAGS) $(STEM).idl; $(RM) $(STEM).idl
$(FILES_THH) : $(GEN_TAB)/St_%_Table.h   : %.idl
	cp  $(1ST_DEPS) $(GEN_TAB)/ ; cd $(GEN_TAB); $(STIC) -r -H -q $(STICFLAGS) $(STEM).idl; $(RM) $(STEM).idl
endif #ALL_TAB
#--- compilation -
$(GEN_DIR)/geant3.def: $(STAR)/asps/agi/gst/geant3.def
	test -h $(GEN_DIR)/geant3.def || $(RM)  $(GEN_DIR)/geant3.def
	test -h $(GEN_DIR)/geant3.def || ln -s $(STAR)/asps/agi/gst/geant3.def  $(GEN_DIR)/geant3.def 
$(FILES_OG): $(OBJ_DIR)/%.o:%.g $(GEN_DIR)/geant3.def
#	test -h $(GEN_DIR)/geant3.def || ln -s $(STAR)/asps/agi/gst/geant3.def  $(GEN_DIR)/geant3.def
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
	$(KUIPC) $(KUIPC_FLAGS) $(1ST_DEPS) $(GEN_DIR)/$(STEM).c
	$(CC)  $(CPPFLAGS) $(CFLAGS)   -c $(GEN_DIR)/$(STEM).c -o $(OBJ_DIR)/$(STEM).o; \
        $(RM)  $(GEN_DIR)/$(STEM).c
	$(AR) $(ARFLAGS) $(LIB_PKG) $(OBJ_DIR)/$(STEM).o; $(RM) $(OBJ_DIR)/$(STEM).o
#___________dependencies_________________________________
ifneq (,$(FILES_IDM))
$(FILES_ICC) : $(GEN_DIR)/%_i.cc : %.idl
	cp  $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP);\
        $(STIC) -r -q $(STICFLAGS) $(1ST_DEPS); \
        cp $(STEM)_i.cc  $(STEM).h $(STEM).inc St_$(STEM)_Module.cxx  St_$(STEM)_Module.h $(GEN_DIR)/; $(RM) *.*;
$(FILES_IH)  : $(GEN_DIR)/%.h    : %.idl
	cp  $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP);\
        $(STIC) -r -q $(STICFLAGS) $(1ST_DEPS); \
        cp $(STEM)_i.cc  $(STEM).h $(STEM).inc St_$(STEM)_Module.cxx  St_$(STEM)_Module.h $(GEN_DIR)/; $(RM) *.*;
#       cp $(STEM).h $(GEN_DIR)/; $(RM) *.*;
$(FILES_INC) : $(GEN_DIR)/%.inc  : %.idl
	cp  $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP);\
        $(STIC) -r -q $(STICFLAGS) $(1ST_DEPS); \
        cp $(STEM)_i.cc  $(STEM).h $(STEM).inc St_$(STEM)_Module.cxx  St_$(STEM)_Module.h $(GEN_DIR)/; $(RM) *.*;
#       cp $(STEM).inc $(GEN_DIR)/; $(RM) *.*;
$(FILES_MHH) : $(GEN_DIR)/St_%_Module.h  : %.idl
	cp  $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP);\
        $(STIC) -r -q $(STICFLAGS) $(1ST_DEPS); \
        cp $(STEM)_i.cc  $(STEM).h $(STEM).inc St_$(STEM)_Module.cxx  St_$(STEM)_Module.h $(GEN_DIR)/; $(RM) *.*;
#       cp St_$(STEM)_Module.h $(GEN_DIR)/; $(RM) *.*;
$(FILES_MOD) : $(GEN_DIR)/St_%_Module.cxx : %.idl
	cp  $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP);\
        $(STIC) -r -q $(STICFLAGS) $(1ST_DEPS); \
        cp $(STEM)_i.cc  $(STEM).h $(STEM).inc St_$(STEM)_Module.cxx  St_$(STEM)_Module.h $(GEN_DIR)/; $(RM) *.*;
#       cp St_$(STEM)_Module.cxx $(GEN_DIR)/; $(RM) *.*;
$(GEN_DIR)/%.didl $(GEN_DIR)/%_i.cc $(GEN_DIR)/%.h $(GEN_DIR)/%.inc: %.idl
	cp  $(1ST_DEPS) $(GEN_TMP)/ ; cd $(GEN_TMP);\
        $(STIC) -q $(STICFLAGS) $(1ST_DEPS); \
        gcc  $(MKDEPFLAGS)  -x c $(STICFLAGS) $(1ST_DEPS) | \
        sed -e 's/$(STEM)\.idl\.o/$(subst .,\., $(subst /,\/, $(GEN_DIR)))\/$(STEM)\.didl/g' \
        > $(GEN_DIR)/$(STEM).didl; 
#        cp $(STEM)_i.cc $(STEM).h $(STEM).inc $(GEN_DIR); $(RM) *.*; 
endif #IDM
$(DEP_DIR)/%.d:%.cc 
	gcc $(MKDEPFLAGS) $(CPPFLAGS) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.o/$(subst .,\.,$(subst /,\/,$(LIB_PKG)($(STEM).o))) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
$(DEP_DIR)/%.d:%.c
	gcc $(MKDEPFLAGS) $(CPPFLAGS) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.o/$(subst .,\.,$(subst /,\/,$(LIB_PKG)($(STEM).o))) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
$(DEP_DIR)/%.d:%.F
	gcc -traditional -x c $(MKDEPFLAGS) $(CPPFLAGS) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.F\.o/$(subst .,\.,$(subst /,\/,$(LIB_PKG)($(STEM).o))) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
$(DEP_DIR)/%.d:%.g
	gcc -traditional -x c $(MKDEPFLAGS) $(CPPFLAGS) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.g\.o/$(subst .,\.,$(subst /,\/,$(LIB_PKG)($(STEM).o))) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
$(DEP_DIR)/%.d:%.cdf
	cd $(SRC_DIR); \
        echo "$(notdir $(STEM)).c $(ALL_TAGS): $(ALL_DEPS)" > $(ALL_TAGS) ;
        echo "$(STEM).o: $(STEM).c" >> $(ALL_TAGS)
#-----test variables------------------------
endif
endif
endif
#-----cleaning------------------------------
clean: clean_obj clean_lib clean_dep
clean_share:
	rm -rf $(GEN_DIR) $(FILES_ALL_TAB)
clean_obj:
	rm -rf $(OBJ_DIR) 
clean_dep:
	rm -rf $(DEP_DIR) 
clean_lib:
	rm -rf $(SL_PKG) $(LIB_PKG)
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
	@echo FEXTEND= $(FEXTEND)
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
	@echo CWD       = $(CWD)  
	@echo OUT_DIR   = $(OUT_DIR)
	@echo SYS_DIR   = $(SYS_DIR)
	@echo LIB_DIR   = $(LIB_DIR)
	@echo OBJ_DIR   = $(OBJ_DIR)
	@echo DEP_DIR   = $(DEP_DIR)
	@echo DIR_GEN   = $(DIR_GEN)
	@echo GEN_DIR   = $(GEN_DIR)
	@echo GEN_TAB   = $(GEN_TAB)
	@echo GEN_TMP   = $(GEN_TMP)
	@echo DOMAIN    = $(DOMAIN)
	@echo PGK       = $(PKG) 
	@echo SL_PGK    = $(SL_PKG)
	@echo LIB_PKG   = $(LIB_PKG)
	@echo INP_DIR   = $(INP_DIR)
	@echo DOM_DIRS  = $(DOM_DIRS)
	@echo SRC_DIR   = $(SRC_DIR)
	@echo IDL_DIRS  = $(IDL_DIRS)
	@echo INC_DIRS  = $(INC_DIRS)
	@echo SRC_DIRS  = $(SRC_DIRS)
	@echo INP_DIR   = $(INP_DIR)
	@echo sources   = $(sources)
	@echo SRC_DIRS  = $(SRC_DIRS)
	@echo FILES_init= $(FILES_init)
