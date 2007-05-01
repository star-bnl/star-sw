
-include $(ROOTSYS)/test/Makefile.arch
HSuf = h

ifeq (1$(OUTPUT_DIR),1)
OUTPUT_DIR = $(shell pwd)
endif
ifeq (1$(OUTPUT_NAME),1)
OUTPUT_NAME = StPi0Results
endif
ifeq (1$(INCDIR),1)
INCDIR = ../..
endif

MYS      = \
           TDataProcessorMemberInspector.$(SrcSuf) \
           StPi0ResultsUtil.$(SrcSuf)
MYDict   = $(OUTPUT_NAME)_Dict.$(SrcSuf)
MYO      = $(MYS:.$(SrcSuf)=.$(ObjSuf)) $(MYDict:.$(SrcSuf)=.$(ObjSuf))
MY       = $(OUTPUT_DIR)/$(OUTPUT_NAME).$(DllSuf)

PROGRAMS      = $(MY)
OBJS          = $(MYO)
DICTIONARIES  = $(MYDict) *_Dict.$(SrcSuf)

MYINCLUDES = -I$(INCDIR)
CXXFLAGS += $(MYINCLUDES) -Wall
LDFLAGS += -shared

ifeq ($(ARCH),win32gcc)
# Windows with gcc

ifeq (1$(STPI0COMMON_DIR),1)
STPI0COMMON_DIR = StPi0Common
endif
ifeq (1$(STPI0COMMON_NAME),1)
STPI0COMMON_NAME = StPi0Common
endif
LIBS += $(STPI0COMMON_DIR)/$(STPI0COMMON_NAME).$(DllSuf)

endif

.SUFFIXES: .$(SrcSuf) .$(ObjSuf) .$(DllSuf)
.PHONY : clean distclean 

all: $(PROGRAMS)

$(MY): $(MYO)
	@echo $(LD) $(LDFLAGS) $^ $(LIBS) $(OutPutOpt)$@
	@$(LD) $(LDFLAGS) $^ $(LIBS) $(OutPutOpt)$@
	@echo "$@ done"

clean:
	@-rm -f $(OBJS) *.d $(DICTIONARIES) $(DICTIONARIES:.$(SrcSuf)=.$(HSuf)) $(DICTIONARIES:.$(SrcSuf)=.$(ObjSuf)) *.stackdump

distclean:      clean
	@-rm -f $(PROGRAMS)

.SUFFIXES: .$(SrcSuf)

TDataProcessorMemberInspector.$(SrcSuf): TDataProcessorMemberInspector.$(HSuf) StPi0ResultsVersion.$(HSuf)

StPi0ResultsUtil.$(SrcSuf): StPi0ResultsUtil.$(HSuf) TDataProcessorMemberInspector.$(HSuf) StPi0ResultsVersion.$(HSuf)

#StPi0ResultsVersion.h:

$(MYDict): \
           TDataProcessorMemberInspector.$(HSuf) \
           StPi0ResultsUtil.$(HSuf) \
           StPi0ResultsVersion.$(HSuf) \
           StPi0ResultsLinkDef.$(HSuf)
	@echo rootcint -f $@ -c -p $(MYINCLUDES) $^
	@rootcint -f $@ -c -p $(MYINCLUDES) $^

.$(SrcSuf).$(ObjSuf):
	@echo $(CXX) $(CXXFLAGS) -c $< -o $@
	@$(CXX) $(CXXFLAGS) -c $< -o $@
