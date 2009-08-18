
-include $(ROOTSYS)/test/Makefile.arch
HSuf = h

MODULE_NAME = StPi0Results

ifeq (1$(OUTPUT_DIR),1)
OUTPUT_DIR = "$(shell pwd)"
endif
ifeq (1$(OUTPUT_NAME),1)
OUTPUT_NAME = $(MODULE_NAME)
endif
ifeq (1$(INCDIR),1)
INCDIR = ../..
endif
ifeq (1$(MYCXXFLAGS),1)
MYCXXFLAGS = "-Wall -fPIC -g"
endif

MYS      = \
           TDataProcessorMemberInspector.$(SrcSuf) \
           StPi0ResultsUtil.$(SrcSuf) \
           StPi0ResultsFinal.$(SrcSuf)
MYDict   = $(OUTPUT_NAME)_Dict.$(SrcSuf)
MYO      = $(MYS:.$(SrcSuf)=.$(ObjSuf)) $(MYDict:.$(SrcSuf)=.$(ObjSuf))
MY       = $(OUTPUT_NAME).$(DllSuf)
MYINST = "$(OUTPUT_DIR)/$(MY)"

DICTIONARIES  = $(MYDict) *_Dict.$(SrcSuf)

MYINCLUDES = -I"$(INCDIR)"
CXXFLAGS = $(shell root-config --cflags) $(MYCXXFLAGS) $(MYINCLUDES)
LDFLAGS  = $(shell root-config --ldflags) -shared
LIBS     = $(shell root-config --libs)

ifeq ($(ARCH),win32gcc)

ifeq (1$(STPI0COMMON_NAME),1)
STPI0COMMON_NAME = StPi0Common
endif
ifeq (1$(STPI0COMMON_DIR),1)
STPI0COMMON_DIR = ../$(STPI0COMMON_NAME)
endif
LIBS += "$(STPI0COMMON_DIR)"/$(STPI0COMMON_NAME).$(DllSuf)

ifeq (1$(STPI0ANALYSIS_NAME),1)
STPI0ANALYSIS_NAME = StPi0Analysis
endif
ifeq (1$(STPI0ANALYSIS_DIR),1)
STPI0ANALYSIS_DIR = ../$(STPI0ANALYSIS_NAME)
endif
LIBS += "$(STPI0ANALYSIS_DIR)"/$(STPI0ANALYSIS_NAME).$(DllSuf)

endif

.SUFFIXES: .$(SrcSuf) .$(ObjSuf) .$(DllSuf)
.PHONY : clean distclean install

all: $(MY)

$(MY): $(MYO)
	$(LD) $(LDFLAGS) $^ $(LIBS) $(OutPutOpt)$@
	@echo "$@ done"

install:
	@if ! test -d "$(OUTPUT_DIR)"; then mkdir -p "$(OUTPUT_DIR)"; fi
	@if test $(MY) -nt $(MYINST); then cp -f $(MY) $(MYINST); echo "Installed $(MY) as $(MYINST)"; fi

clean:
	-rm -f $(MYO) *.d $(DICTIONARIES) $(DICTIONARIES:.$(SrcSuf)=.$(HSuf)) $(DICTIONARIES:.$(SrcSuf)=.$(ObjSuf)) *.stackdump

distclean:      clean
	-rm -f $(MY)
	-rm -f "$(OUTPUT_DIR)"/$(MY)

.SUFFIXES: .$(SrcSuf)

TDataProcessorMemberInspector.$(SrcSuf): TDataProcessorMemberInspector.$(HSuf) StPi0ResultsVersion.$(HSuf)

StPi0ResultsUtil.$(SrcSuf): StPi0ResultsUtil.$(HSuf) TDataProcessorMemberInspector.$(HSuf) StPi0ResultsVersion.$(HSuf)

StPi0ResultsFinal.$(SrcSuf): StPi0ResultsUtil.$(HSuf) TDataProcessorMemberInspector.$(HSuf) StPi0ResultsVersion.$(HSuf)

$(MYDict): \
           TDataProcessorMemberInspector.$(HSuf) \
           StPi0ResultsUtil.$(HSuf) \
           StPi0ResultsFinal.$(HSuf) \
           StPi0ResultsVersion.$(HSuf) \
           StPi0ResultsLinkDef.$(HSuf)
	rootcint -f $@ -c -p $(MYINCLUDES) $^

.$(SrcSuf).$(ObjSuf):
	$(CXX) $(CXXFLAGS) -c $< -o $@
