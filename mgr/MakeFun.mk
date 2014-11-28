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
FNameSO       = $(FName).$(DllSuf)

FUN_INCS := $(STAR)/include

ifdef FUN_INC
 FUN_INCS := $(FUN_INC)
endif
ifdef FUN_INCS
  ADD_INCS  := $(addprefix -I,$(FUN_INCS))
endif

#------------------------------------------------------------------------------
FNameCint     = $(FName)Cint
FNameO        = $(FName).$(ObjSuf) \
                $(FNameCint).$(ObjSuf)

FNameS        = $(FName).$(SrcSuf) \
                $(FNameCint).$(SrcSuf)


OBJS          = $(FNameO) 


$(FNameSO):     $(FNameO) 
		$(SO) $(SOFLAGS) $(LDFLAGS) $(FNameO) $(OutPutOpt) $(FNameSO)
		@echo "$(FNameSO) done"


.SUFFIXES: .$(SrcSuf)

###

$(FName).o: $(FName).h $(FName).$(SrcSuf)

$(FName)LinkDef.h : $(FName).h
	if [ -f $(FName)LinkDef.h ]; then   rm $(FName)LinkDef.h ; fi
	cat $(FName).h | sed -e '/\#ifdef __CINT__/,/\#endif/!'d > $(FName)LinkDef.h

$(FNameCint).$(SrcSuf): $(FName).h $(FName)LinkDef.h
	@echo "Generating dictionary ..."
	rootcint -f $(FNameCint).$(SrcSuf) -c $(ADD_INCS) -I$(ROOTSYS)/include $(FName).h  $(FName)LinkDef.h 

.$(SrcSuf).$(ObjSuf):
	$(CXX) $(CXXFLAGS)  -I$(ROOTSYS)/include $(ADD_INCS) -c $<


clean:
		@rm -f $(OBJS) $(FNameCint).$(SrcSuf) $(FNameCint).h core

show:
	@echo $(UNAME).$(UNAMER) CXX=$(CXX) KASE=$(KASE)
