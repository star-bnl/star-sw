# $Id: MakeFun.mk,v 1.4 1999/09/22 15:26:52 fisyak Exp $
# $Log: MakeFun.mk,v $
# Revision 1.4  1999/09/22 15:26:52  fisyak
# Add definition of STAR_MAKE_HOME
#
# Revision 1.3  1999/01/20 02:16:50  fisyak
# Active STAR_HOST_SYS for egcs
#
# Revision 1.2  1999/01/14 23:28:25  fisyak
# Add includes
#
# Revision 1.1  1999/01/14 13:56:40  fisyak
# Add Victors MakeFun.mk, Add StMagF
#
#  
#  To run this makefile : gmake -f MakeFun.mk fun=NAME

#Where your source code is NAME.cxx and header is NAME.h
#And file NAME.h must contain:

#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

#pragma link C++ class MyClass1;
#pragma link C++ class MyClass2;


#pragma link C++ class MyClass99;
#endif




ifndef STAR_MAKE_HOME
  STAR_MAKE_HOME := $(STAR)/mgr
endif

include $(STAR)/mgr/MakeEnv.mk
include $(STAR)/mgr/MakeArch.mk

ObjSuf        = o
SrcSuf        = cxx
ExeSuf        =
OutPutOpt     = -o
DllSuf        = so

FName      := $(fun)
ifndef SO_LIB
SO_LIB       = ./$(FName).$(DllSuf)
endif
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
# 	Define internal and external includes dirs
INC_NAMES := $(addprefix StRoot/,base StChain xdf2root) StRoot .share .share/tables pams inc 
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


#------------------------------------------------------------------------------
FNameCint     = $(FName)Cint
FNameO        = $(FName).$(ObjSuf) \
                $(FNameCint).$(ObjSuf)

FNameS        = $(FName).$(SrcSuf) \
                $(FNameCint).$(SrcSuf)


OBJS          = $(FNameO) 


$(MY_SO):     $(FNameO) 
	$(SO) $(SOFLAGS) $(LDFLAGS) $(FNameO) $(OutPutOpt) $(SL_NEW); \
        $(RM) $(MY_SO); $(LN) $(SL_NEW) $(MY_SO);  
	@echo "$(SO_LIB) done"


.SUFFIXES: .$(SrcSuf)

###

$(FName).$(ObjSuf): $(FName).h $(FName).$(SrcSuf) 
$(FNameCint).$(ObjSuf): $(FNameCint).$(SrcSuf) 

$(FName).$(ObjSuf):$(FName).$(SrcSuf)
	$(CXX)  $(CPPFLAGS) $(CXXFLAGS) $(INCLUDES) -c $(1ST_DEPS) -o $(ALL_TAGS)
$(FName)LinkDef.h : $(FName).h
	if [ -f $(FName)LinkDef.h ]; then   rm $(FName)LinkDef.h ; fi
	cat $(FName).h | sed -e '/\#ifdef __CINT__/,/\#endif/!'d > $(FName)LinkDef.h

$(FNameCint).$(SrcSuf): $(FName).h $(FName)LinkDef.h
	@echo "Generating dictionary ..."
	@rootcint -f $(FNameCint).$(SrcSuf) -c $(FName).h $(FName)LinkDef.h

clean:
		@rm -f $(OBJS) $(FNameCint).$(SrcSuf) $(FNameCint).h core $(FName)LinkDef.h $(MY_SO)*
show:
	@echo $(UNAME).$(UNAMER) CXX=$(CXX) KASE=$(KASE)
	@echo INCLUDES = $(INCLUDES)
	@echo CPPFLAGS = $(CPPFLAGS)
	@echo CXXFLAGS = $(CXXFLAGS)
