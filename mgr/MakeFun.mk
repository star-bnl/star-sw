# $Id: MakeFun.mk,v 1.1 1999/01/14 13:56:40 fisyak Exp $
# $Log: MakeFun.mk,v $
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

$(FName).o: $(FName).h $(FName).$(SrcSuf)

$(FName)LinkDef.h : $(FName).h
	if [ -f $(FName)LinkDef.h ]; then   rm $(FName)LinkDef.h ; fi
	cat $(FName).h | sed -e '/\#ifdef __CINT__/,/\#endif/!'d > $(FName)LinkDef.h

$(FNameCint).$(SrcSuf): $(FName).h $(FName)LinkDef.h
	@echo "Generating dictionary ..."
	@rootcint -f $(FNameCint).$(SrcSuf) -c $(FName).h $(FName)LinkDef.h

.$(SrcSuf).$(ObjSuf):
	$(CXX) $(CXXFLAGS) -I$(ROOTSYS)/include -c $<


clean:
		@rm -f $(OBJS) $(FNameCint).$(SrcSuf) $(FNameCint).h core

show:
	@echo $(UNAME).$(UNAMER) CXX=$(CXX) KASE=$(KASE)
