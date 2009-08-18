
-include $(ROOTSYS)/test/Makefile.arch
HSuf = h

MODULE_NAME = StPi0Analysis

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
           TBinParameters.$(SrcSuf) \
           TBinStatistics.$(SrcSuf) \
           TCandidateDataProcessor.$(SrcSuf) \
           TCutParameters.$(SrcSuf) \
           TCuts.$(SrcSuf) \
           TDataProcessor.$(SrcSuf) \
           TDataProcessorPool.$(SrcSuf) \
           TEventDataProcessor.$(SrcSuf) \
           TInvariantMassDistribution.$(SrcSuf) \
           TPointDataProcessor.$(SrcSuf) \
           TClusterDataProcessor.$(SrcSuf) \
           THitDataProcessor.$(SrcSuf) \
           TSimuDataProcessor.$(SrcSuf) \
           TMCGammaDataProcessor.$(SrcSuf) \
           TSMDThresholdDataProcessor.$(SrcSuf) \
           TWeightCalculator.$(SrcSuf) \
           StPi0AnalysisUtil.$(SrcSuf) \
           StPi0AnalysisUtilDB.$(SrcSuf)
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

TBinParameters.$(SrcSuf): TBinParameters.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TBinStatistics.$(SrcSuf): TBinStatistics.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TCandidateDataProcessor.$(SrcSuf): TCandidateDataProcessor.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TCutParameters.$(SrcSuf): TCutParameters.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TCuts.$(SrcSuf): TCuts.$(HSuf) TCutParameters.$(HSuf) CutDefinitions.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TDataProcessor.$(SrcSuf): TDataProcessor.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TDataProcessorPool.$(SrcSuf): TDataProcessorPool.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TEventDataProcessor.$(SrcSuf): TEventDataProcessor.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TInvariantMassDistribution.$(SrcSuf): TInvariantMassDistribution.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TPointDataProcessor.$(SrcSuf): TPointDataProcessor.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TClusterDataProcessor.$(SrcSuf): TClusterDataProcessor.$(HSuf) StPi0AnalysisVersion.$(HSuf)

THitDataProcessor.$(SrcSuf): THitDataProcessor.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TMCGammaDataProcessor.$(SrcSuf): TMCGammaDataProcessor.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TSimuDataProcessor.$(SrcSuf): TSimuDataProcessor.$(HSuf) TMCGammaDataProcessor.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TWeightCalculator.$(SrcSuf): TWeightCalculator.$(HSuf) StPi0AnalysisVersion.$(HSuf)

StPi0AnalysisUtil.$(SrcSuf): StPi0AnalysisUtil.$(HSuf) StPi0AnalysisVersion.$(HSuf)

StPi0AnalysisUtilDB.$(SrcSuf): StPi0AnalysisUtilDB.$(HSuf)

# Header dependencies 

TBinParameters.$(SrcSuf): StPi0AnalysisUtil.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TBinStatistics.$(SrcSuf): StPi0AnalysisUtil.$(HSuf) TBinParameters.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TCandidateDataProcessor.$(SrcSuf): TDataProcessor.$(HSuf) TInvariantMassDistribution.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TDataProcessor.$(SrcSuf): StPi0AnalysisUtil.$(HSuf) TCuts.$(HSuf) TWeightCalculator.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TDataProcessorPool.$(SrcSuf): TDataProcessor.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TEventDataProcessor.$(SrcSuf): TDataProcessor.$(HSuf) THitDataProcessor.$(HSuf) StPi0AnalysisUtil.$(HSuf) TBinStatistics.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TInvariantMassDistribution.$(SrcSuf): StPi0AnalysisUtil.$(HSuf) TBinParameters.$(HSuf) TCuts.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TPointDataProcessor.$(SrcSuf): TDataProcessor.$(HSuf) TClusterDataProcessor.$(HSuf) StPi0AnalysisUtil.$(HSuf) TBinStatistics.$(HSuf) TInvariantMassDistribution.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TClusterDataProcessor.$(SrcSuf): TDataProcessor.$(HSuf) THitDataProcessor.$(HSuf) StPi0AnalysisUtil.$(HSuf) TBinStatistics.$(HSuf) TInvariantMassDistribution.$(HSuf) StPi0AnalysisVersion.$(HSuf)

THitDataProcessor.$(SrcSuf): TDataProcessor.$(HSuf) StPi0AnalysisUtil.$(HSuf) TBinStatistics.$(HSuf) TInvariantMassDistribution.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TMCGammaDataProcessor.$(SrcSuf): TDataProcessor.$(HSuf) StPi0AnalysisUtil.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TSMDThresholdDataProcessor.$(SrcSuf): TDataProcessor.$(HSuf) StPi0AnalysisUtil.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TSimuDataProcessor.$(SrcSuf): TDataProcessor.$(HSuf) StPi0AnalysisUtil.$(HSuf) TBinStatistics.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TWeightCalculator.$(SrcSuf): StPi0AnalysisUtil.$(HSuf) StPi0AnalysisVersion.$(HSuf)

$(MYDict): \
           TBinParameters.$(HSuf) \
           TBinStatistics.$(HSuf) \
           TCandidateDataProcessor.$(HSuf) \
           TCutParameters.$(HSuf) \
           TCuts.$(HSuf) \
           TDataProcessor.$(HSuf) \
           TDataProcessorPool.$(HSuf) \
           TEventDataProcessor.$(HSuf) \
           TInvariantMassDistribution.$(HSuf) \
           TPointDataProcessor.$(HSuf) \
           TClusterDataProcessor.$(HSuf) \
           THitDataProcessor.$(HSuf) \
           TMCGammaDataProcessor.$(HSuf) \
           TSimuDataProcessor.$(HSuf) \
           TSMDThresholdDataProcessor.$(HSuf) \
           TWeightCalculator.$(HSuf) \
           StPi0AnalysisUtil.$(HSuf) \
           StPi0AnalysisUtilDB.$(HSuf) \
           StPi0AnalysisVersion.$(HSuf) \
           StPi0AnalysisLinkDef.$(HSuf)
	rootcint -f $@ -c -p $(MYINCLUDES) $^

.$(SrcSuf).$(ObjSuf):
	$(CXX) $(CXXFLAGS) -c $< -o $@
