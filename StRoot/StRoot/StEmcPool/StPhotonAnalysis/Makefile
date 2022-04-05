
-include $(ROOTSYS)/test/Makefile.arch
HSuf = h

MODULE_NAME = StPhotonAnalysis

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
           AnaCuts.$(SrcSuf) \
           Efficiency.$(SrcSuf) \
           EventMixer.$(SrcSuf) \
           GainAnalysis.$(SrcSuf) \
           Pi0Analysis.$(SrcSuf) \
           dAuBackground.$(SrcSuf) \
           MyDecay.$(SrcSuf) \
           PhotonAnalysisUtil.$(SrcSuf)
MYDict   = $(OUTPUT_NAME)_Dict.$(SrcSuf)
MYO      = $(MYS:.$(SrcSuf)=.$(ObjSuf)) $(MYDict:.$(SrcSuf)=.$(ObjSuf))
MY       = $(OUTPUT_NAME).$(DllSuf)
MYINST = "$(OUTPUT_DIR)/$(MY)"

DICTIONARIES  = $(MYDict) *_Dict.$(SrcSuf)

MYINCLUDES = -I"$(INCDIR)"
CXXFLAGS = $(shell root-config --cflags) $(MYCXXFLAGS) $(MYINCLUDES)
LDFLAGS  = $(shell root-config --ldflags) -shared
LIBS     = $(shell root-config --libs) -lEG

ifeq ($(ARCH),win32gcc)

ifeq (1$(STPHOTONCOMMON_NAME),1)
STPHOTONCOMMON_NAME = StPhotonCommon
endif
ifeq (1$(STPHOTONCOMMON_DIR),1)
STPHOTONCOMMON_DIR = ../$(STPHOTONCOMMON_NAME)
endif
LIBS += "$(STPHOTONCOMMON_DIR)"/$(STPHOTONCOMMON_NAME).$(DllSuf)

endif

#------------------------------------------------------------------------------

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

distclean: clean
	-rm -f $(MY)
	-rm -f "$(OUTPUT_DIR)"/$(MY)

.SUFFIXES: .$(SrcSuf)

AnaCuts.$(SrcSuf): AnaCuts.$(HSuf)

Efficiency.$(SrcSuf): AnaCuts.$(HSuf) Pi0Analysis.$(HSuf) Efficiency.$(HSuf)

EventMixer.$(SrcSuf): AnaCuts.$(HSuf) EventMixer.$(HSuf)

GainAnalysis.$(SrcSuf): GainAnalysis.$(HSuf)

MyDecay.$(SrcSuf): MyDecay.$(HSuf)

PhotonAnalysisUtil.$(SrcSuf): AnaCuts.$(HSuf) Pi0Analysis.$(HSuf) PhotonAnalysisUtil.$(HSuf)

Pi0Analysis.$(SrcSuf): AnaCuts.$(HSuf) EventMixer.$(HSuf) Pi0Analysis.$(HSuf)

dAuBackground.$(SrcSuf): AnaCuts.$(HSuf) dAuBackground.$(HSuf)

$(MYDict): \
           AnaCuts.$(HSuf) \
           Efficiency.$(HSuf) \
           EventMixer.$(HSuf) \
           GainAnalysis.$(HSuf) \
           Pi0Analysis.$(HSuf) \
           dAuBackground.$(HSuf) \
           MyDecay.$(HSuf) \
           PhotonAnalysisUtil.$(HSuf) \
           StPhotonAnalysisLinkDef.$(HSuf)
	rootcint -f $@ -c -p $(MYINCLUDES) $^

.$(SrcSuf).$(ObjSuf):
	$(CXX) $(CXXFLAGS) -c $< -o $@
