
-include $(ROOTSYS)/test/Makefile.arch
HSuf = h

ifeq (1$(OUTPUT_DIR),1)
OUTPUT_DIR = $(shell pwd)
endif
ifeq (1$(OUTPUT_NAME),1)
OUTPUT_NAME = StPi0Analysis
endif
ifeq (1$(INCDIR),1)
INCDIR = ../..
endif

MYS      = \
           TBinParameters.$(SrcSuf) \
           TBinStatistics.$(SrcSuf) \
           TCandidateDataProcessor.$(SrcSuf) \
           TCutParameters.$(SrcSuf) \
           TCuts.$(SrcSuf) \
           TDataProcessor.$(SrcSuf) \
           TDataPreloader.$(SrcSuf) \
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
MY       = $(OUTPUT_DIR)/$(OUTPUT_NAME).$(DllSuf)

PROGRAMS      = $(MY)
OBJS          = $(MYO)
DICTIONARIES  = $(MYDict) *_Dict.$(SrcSuf)

MYINCLUDES = -I$(INCDIR)
CXXFLAGS += $(MYINCLUDES) -Wall
LDFLAGS += -shared

ifeq ($(ARCH),win32gcc)
# Windows with gcc

ifeq (1$(DATASTRUCTURES_DIR),1)
DATASTRUCTURES_DIR = DataStructures
endif
ifeq (1$(DATASTRUCTURES_NAME),1)
DATASTRUCTURES_NAME = DataStructures
endif
LIBS += $(DATASTRUCTURES_DIR)/$(DATASTRUCTURES_NAME).$(DllSuf)

ifeq (1$(STPI0COMMON_DIR),1)
STPI0COMMON_DIR = StPi0Common
endif
ifeq (1$(STPI0COMMON_NAME),1)
STPI0COMMON_NAME = StPi0Common
endif
LIBS += $(STPI0COMMON_DIR)/$(STPI0COMMON_NAME).$(DllSuf)

endif

#------------------------------------------------------------------------------

.SUFFIXES: .$(SrcSuf) .$(ObjSuf) .$(DllSuf)
.PHONY : clean distclean

all: $(PROGRAMS)

$(MY): $(MYO)
	@echo $(LD) $(LDFLAGS) $^ $(LIBS) $(OutPutOpt)$@
	@$(LD) $(LDFLAGS) $^ $(LIBS) $(OutPutOpt)$@
	@echo "$@ done"

clean:
	@-rm -f $(OBJS) *.d $(DICTIONARIES) $(DICTIONARIES:.$(SrcSuf)=.$(HSuf)) $(DICTIONARIES:.$(SrcSuf)=.$(ObjSuf)) *.stackdump

distclean: clean
	@-rm -f $(PROGRAMS)

.SUFFIXES: .$(SrcSuf)

TBinParameters.$(SrcSuf): TBinParameters.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TBinStatistics.$(SrcSuf): TBinStatistics.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TCandidateDataProcessor.$(SrcSuf): TCandidateDataProcessor.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TCutParameters.$(SrcSuf): TCutParameters.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TCuts.$(SrcSuf): TCuts.$(HSuf) TCutParameters.$(HSuf) CutDefinitions.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TDataProcessor.$(SrcSuf): TDataProcessor.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TDataProcessorPool.$(SrcSuf): TDataProcessorPool.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TDataPreloader.$(SrcSuf): TDataPreloader.$(HSuf) StPi0AnalysisVersion.$(HSuf)

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

TBinParameters.$(HSuf): StPi0AnalysisUtil.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TBinStatistics.$(HSuf): StPi0AnalysisUtil.$(HSuf) TBinParameters.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TCandidateDataProcessor.$(HSuf): TDataProcessor.$(HSuf) TInvariantMassDistribution.$(HSuf) StPi0AnalysisVersion.$(HSuf)

#TCutParameters.$(HSuf): 

TDataProcessor.$(HSuf): StPi0AnalysisUtil.$(HSuf) TCuts.$(HSuf) TWeightCalculator.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TDataProcessorPool.$(HSuf): TDataProcessor.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TEventDataProcessor.$(HSuf): TDataProcessor.$(HSuf) THitDataProcessor.$(HSuf) StPi0AnalysisUtil.$(HSuf) TBinStatistics.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TInvariantMassDistribution.$(HSuf): StPi0AnalysisUtil.$(HSuf) TBinParameters.$(HSuf) TCuts.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TPointDataProcessor.$(HSuf): TDataProcessor.$(HSuf) TClusterDataProcessor.$(HSuf) StPi0AnalysisUtil.$(HSuf) TBinStatistics.$(HSuf) TInvariantMassDistribution.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TClusterDataProcessor.$(HSuf): TDataProcessor.$(HSuf) THitDataProcessor.$(HSuf) StPi0AnalysisUtil.$(HSuf) TBinStatistics.$(HSuf) TInvariantMassDistribution.$(HSuf) StPi0AnalysisVersion.$(HSuf)

THitDataProcessor.$(HSuf): TDataProcessor.$(HSuf) StPi0AnalysisUtil.$(HSuf) TBinStatistics.$(HSuf) TInvariantMassDistribution.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TMCGammaDataProcessor.$(HSuf): TDataProcessor.$(HSuf) StPi0AnalysisUtil.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TSMDThresholdDataProcessor.$(HSuf): TDataProcessor.$(HSuf) StPi0AnalysisUtil.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TSimuDataProcessor.$(HSuf): TDataProcessor.$(HSuf) StPi0AnalysisUtil.$(HSuf) TBinStatistics.$(HSuf) StPi0AnalysisVersion.$(HSuf)

TWeightCalculator.$(HSuf): StPi0AnalysisUtil.$(HSuf) StPi0AnalysisVersion.$(HSuf)

#StPi0AnalysisVersion.h:


$(MYDict): \
           TBinParameters.$(HSuf) \
           TBinStatistics.$(HSuf) \
           TCandidateDataProcessor.$(HSuf) \
           TCutParameters.$(HSuf) \
           TCuts.$(HSuf) \
           TDataProcessor.$(HSuf) \
           TDataPreloader.$(HSuf) \
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
	@echo rootcint -f $@ -c -p $(MYINCLUDES) $^
	@rootcint -f $@ -c -p $(MYINCLUDES) $^

.$(SrcSuf).$(ObjSuf):
	@echo $(CXX) $(CXXFLAGS) -c $< -o $@
	@$(CXX) $(CXXFLAGS) -c $< -o $@
