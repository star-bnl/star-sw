#ifndef StPi0Results_Util_H
#define StPi0Results_Util_H

#include <TNamed.h>
#include <TString.h>
#include <TH1F.h>
#include <TLegend.h>
#include <TF1.h>
#include <TF2.h>
#include <TCanvas.h>

#include <StEmcPool/StPi0Analysis/TBinStatistics.h>
#include <StEmcPool/StPi0Analysis/TInvariantMassDistribution.h>

#include <list>
using namespace std;

#include "StPi0ResultsVersion.h"

class TDataProcessorPool;
class TCandidateDataProcessor;
class TEventDataProcessor;
class TSimuDataProcessor;
class TMCGammaDataProcessor;
class TPointDataProcessor;
class TWeightCalculator;
class TBinStatistics;
class TInvariantMassDistribution;
typedef list<TBinStatistics> bin_stat_list_type;
typedef list<TInvariantMassDistribution> distribution_list_type;
typedef TBinStatistics bin_stat_type;
typedef TInvariantMassDistribution distribution_type;

extern Bool_t showPhenixPi0;
extern const Char_t *showPhenixPi0Legend;
extern Bool_t showPhenixPi0PP;
extern const Char_t *showPhenixPi0PPLegend;
extern Bool_t showStarChargedHadrons;
extern const Char_t *showStarChargedHadronsLegend;
extern Bool_t showStarChargedPions;
extern const Char_t *showStarChargedPionsPlusLegend;
extern const Char_t *showStarChargedPionsMinusLegend;
extern Bool_t showMischkePi0;
extern const Char_t *showMischkePi0Legend;
extern Bool_t showStolpovskyPi0;
extern const Char_t *showStolpovskyPi0Legend;
extern Bool_t showStolpovskyPi0PP;
extern const Char_t *showStolpovskyPi0PPLegend;
extern Bool_t showpQCD;
extern const Char_t *showpQCDLegend;
extern Bool_t showStarRcp;
extern const Char_t *showStarRcpLegend1;
extern const Char_t *showStarRcpLegend2;
extern Bool_t showPhenixPi0dA;
extern const Char_t *showPhenixPi0dALegend;
extern Bool_t showMartijnPi0dAu;
extern const Char_t *showMartijnPi0dAuLegend;
extern Bool_t showSimonPi0PPCrossection;
extern const Char_t *showSimonPi0PPCrossectionLegend;

extern const Char_t *fitFuncZero;
extern const Char_t *fitFuncConst;
extern const Char_t *fitFuncLinear;

extern const Char_t *fitFuncTrackDist;
extern const Char_t *fitFuncTrackDistPeak;

extern const Float_t noLimitMagic;

class TFitSettings : public TNamed {
public:
    typedef TNamed inherited;

    Float_t value;
    Float_t valueDrift;
    Bool_t useFit;
    Bool_t wasFit;
    Float_t fitLow;
    Float_t fitHigh;
    TString fitFuncStr;
    Option_t *fitOption;
    TF1 *func;
    Float_t fitUseLow;
    Float_t fitUseHigh;

    Float_t limitLow;
    Float_t limitHigh;

    bin_stat_list_type valuesForFit;
    Float_t fitSigma;
    Float_t fitMeanAbs;

    TFitSettings(Float_t _value = 0, Float_t _valueDrift = 0
		, Float_t _limitLow = noLimitMagic, Float_t _limitHigh = noLimitMagic
		, Bool_t _useFit = false, Float_t _fitLow = 0, Float_t _fitHigh = 0, Float_t _fitUseLow = 0, Float_t _fitUseHigh = 0
		, const TString &_fitFuncStr = "[0]", Option_t *_fitOption = "RQN", TF1 *_func = 0
	)
	: inherited()
	, value(_value)
	, valueDrift(_valueDrift)
	, useFit(_useFit)
	, wasFit(false)
	, fitLow(_fitLow)
	, fitHigh(_fitHigh)
	, fitFuncStr(_fitFuncStr)
	, fitOption(_fitOption)
	, func(_func)
	, fitUseLow(_fitUseLow)
	, fitUseHigh(_fitUseHigh)
	, limitLow(_limitLow)
	, limitHigh(_limitHigh)
	, fitSigma(0)
	, fitMeanAbs(0)
    {}

    ClassDef(TFitSettings, STPI0RESULTS_VERSION);
};

class TAnalysisSettingsTrigger : public TNamed {
public:
    typedef TNamed inherited;

    Float_t lowPt;
    Float_t highPt;
    Float_t lowPtBinSize;
    Float_t switchBinSizePt;
    Float_t highPtBinSize;

    Float_t rebinNumBinsWeight;
    Float_t rebinMeanErrorWeight;

    Bool_t setErrorOnZeroBins;
    Bool_t setErrorOnZeroBinsBg;
    Bool_t setErrorOnZeroBinsBgRandom;
    Bool_t setErrorOnZeroBinsBgLowMass;
    Bool_t setErrorOnZeroBinsPeakShape;
    Bool_t setErrorOnZeroBinsPeakShapeEta;

    TFitSettings fitLeft;
    TFitSettings fitRight;

    TFitSettings peakLeft;
    TFitSettings peakRight;
    TFitSettings peakPosition;
    TFitSettings peakWidth;

    TFitSettings peakLeftEta;
    TFitSettings peakRightEta;
    TFitSettings peakPositionEta;
    TFitSettings peakWidthEta;

    Float_t peakPositionTrue;
    Float_t peakPositionTrueEta;

    TFitSettings fitParameter6;
    TFitSettings fitParameter7;
    TFitSettings fitParameters;

    TString fitFunctionStr;
    Option_t *fitOption;

    TFitSettings areaFractionBg;

    Bool_t usePeakRangeYield;
    TFitSettings peakLeftYield;
    TFitSettings peakRightYield;
    Bool_t usePeakRangeSigmaYield;
    TFitSettings peakLeftSigmaYield;
    TFitSettings peakRightSigmaYield;

    Bool_t usePeakRangeYieldEta;
    TFitSettings peakLeftYieldEta;
    TFitSettings peakRightYieldEta;
    Bool_t usePeakRangeSigmaYieldEta;
    TFitSettings peakLeftSigmaYieldEta;
    TFitSettings peakRightSigmaYieldEta;

    Bool_t usePeakRangeAreaShare;

    TFitSettings signalToBackgroundRatio;
    TFitSettings signalToBackgroundRatioEta;

    Bool_t correctCpv;
    Bool_t correctCpvEta;
    Float_t rebinNumBinsWeightCpv;
    Float_t rebinMeanErrorWeightCpv;
    TFitSettings fitLeftCpv;
    TFitSettings fitRightCpv;
    TFitSettings peakLeftCpv;
    TFitSettings peakRightCpv;
    TFitSettings peakPositionCpv;
    TFitSettings peakWidthCpv;
    TFitSettings fitParameter6Cpv;
    TFitSettings fitParameter7Cpv;
    TFitSettings fitParametersCpv;
    TString fitFunctionStrCpv;
    Option_t *fitOptionCpv;
    Float_t binRangeLeftCpv;
    Float_t binRangeRightCpv;
    TFitSettings cpvCorrection;

    TFitSettings jetBgFraction;
    Bool_t fitJetBgFractionFromCandidates;

    Int_t smoothBg;
    Float_t mixNormFixed;
    Bool_t mixNormMassRange;
    Float_t mixNormLeft;
    Float_t mixNormRight;
    Bool_t mixNormEntries;

    Int_t smoothBgRandom;
    Float_t mixNormFixedRandom;
    Bool_t mixNormMassRangeRandom;
    Float_t mixNormLeftRandom;
    Float_t mixNormRightRandom;
    Bool_t mixNormEntriesRandom;

    Bool_t subtractBg;
    Bool_t subtractBgRandom;
    Bool_t subtractBgLowMass;
    Bool_t subtractBgLowMassPeakShape;
    Bool_t subtractBgLowMassPeakShapeEta;
    Bool_t subtractBgFitDistribution;
    Bool_t useEventMixingBgRandom;
    Bool_t useEventMixingNotmatchedBgRandom;
    Bool_t useEventJetMixingBg;
    Bool_t useEventJetNotmatchedMixingBg;
    Bool_t useEventJetBackMixingBg;
    Bool_t useEventShufflingBg;
    Bool_t fitBgDistribution;
    Bool_t fitBgRandomDistribution;
    Bool_t fitBgLowMassDistribution;
    Bool_t fitPeakShapeDistribution;
    Bool_t fitPeakShapeEtaDistribution;
    Bool_t fitPeakShapeEtabgDistribution;
    Float_t fitDistributionLeft;
    Float_t fitDistributionRight;
    
    Float_t lowNormFixed;
    Bool_t lowNormMassRange;
    Float_t lowNormMassRangeLeft;
    Float_t lowNormMassRangeRight;
    Bool_t lowNormPoints;
    Float_t lowmassbgPointsLowPt;
    Float_t lowmassbgPointsHighPt;
    Bool_t lowNormPointsPt;

    Float_t fixedPrescale;
    Bool_t calculatePrescaleFromDB;
    Bool_t calculatePrescaleFromSim;
    Bool_t calculatePrescaleFromPoints;
    Bool_t calculatePrescaleFromPointsIntegral;
    Float_t prescalePointsLowPt;
    Float_t prescalePointsHighPt;
    Float_t prescalePointsLowPtIntegral;
    Float_t prescalePointsHighPtIntegral;

    Bool_t useProportionalBgErrors;
    Bool_t useIndependentSigBgErrors;
    Bool_t normalizeSigBgErrors;
    Bool_t normalizeSigBgSquareErrors;
    Bool_t useAnticorrelatedSigBgErrors;

    Bool_t correctVertexFindingEff;
    Float_t vertexFindingEff;
    Bool_t correctVertexFindingEffMB;
        
    Bool_t normPeakShapeYieldShow;
    Bool_t normPeakShapeEtaYieldShow;

    Bool_t showBins;
    Bool_t showSigBg;
    Bool_t showSig;
    Bool_t showPeakShape;
    Bool_t showPeakShapeEta;
    Bool_t showPeakShapeEtabg;
    Bool_t showBg;
    Bool_t showBgRandom;
    Bool_t showBgLowMass;
    Bool_t showBgFitDistribution;
    Bool_t showFuncPeak;
    Bool_t showFuncPeakEta;
    Bool_t showFuncBg;
    Bool_t showResidual;
    Bool_t showResidualBg;
    Bool_t showResidualPeakShapeBg;
    Bool_t showText;
    Bool_t showTruePeakPos;

    Int_t color;
    Int_t colorSigBg;
    Int_t colorBg;
    Int_t colorBgRandom;
    Int_t colorBgLowMass;
    Int_t colorBgFitDistribution;
    Int_t peakLinesColor;
    Int_t fitLinesColor;
    Int_t bgColor;
    Int_t fillColor;
    Int_t bgFillStyle;
    Float_t binRangeLeft;
    Float_t binRangeRight;
    TString name;
    TString title;

    Option_t *drawOption;
    Option_t *legendOption;
    TString legendTitle;
    Bool_t useXerr;
    Float_t markerSize;
    Int_t markerStyle;
    Int_t lineSize;
    Int_t lineStyle;

    Float_t triggerThreshold;
    Bool_t useFittedPeakPos;

    ClassDef(TAnalysisSettingsTrigger, STPI0RESULTS_VERSION);
};

struct TAnalysisSettings : public TNamed {
public:
    TAnalysisSettingsTrigger settingsMB;
    TAnalysisSettingsTrigger settingsHT1;
    TAnalysisSettingsTrigger settingsHT2;

    TAnalysisSettingsTrigger settingsSimMB;
    TAnalysisSettingsTrigger settingsSimHT1;
    TAnalysisSettingsTrigger settingsSimHT2;

    TAnalysisSettingsTrigger settingsSimEtaMB;
    TAnalysisSettingsTrigger settingsSimEtaHT1;
    TAnalysisSettingsTrigger settingsSimEtaHT2;

    Bool_t smoothEffMB;
    Bool_t smoothEffHT1;
    Bool_t smoothEffHT2;
    Bool_t dontCorrectEff;
    Bool_t correctEffMatrix;

    Bool_t smoothEffMBEta;
    Bool_t smoothEffHT1Eta;
    Bool_t smoothEffHT2Eta;
    Bool_t dontCorrectEffEta;
    Bool_t correctEffMatrixEta;

    TFitSettings cpvCorrection;
    TFitSettings cpvCorrectionSim;
    TFitSettings cpvCorrectionSimEta;

    TString shiftFuncStr;

    Bool_t showReal;
    Bool_t showSim;
    Bool_t print;
    Int_t multiplicityDistributions;
    Bool_t pointMultiplicityDistributions;
    Bool_t showTriggersSeparately;
    Bool_t showWeightNextIteration;
    Bool_t showpQCDWeight;
    Bool_t showWeightUsed;
    Bool_t correctForPtShift;
    Bool_t correctForPtShiftEff;
    Bool_t correctForPreciseBinPt;
    Bool_t saveWeightNextIteration;
    TString tpcVertexEffFunc;
    Bool_t showTPCVertexEff;
    Bool_t saveTPCVertexEff;
    TString bbcVertexEffFunc;
    Bool_t showBBCVertexEff;
    Bool_t saveBBCVertexEff;
    TString weightFile;

    Bool_t correctPrescaleTriggerBias;
    Float_t prescaleTriggerBiasMBHT1;
    Float_t prescaleTriggerBiasMBHT2;
    Float_t prescaleTriggerBiasHT1HT2;

    Bool_t correctMeanAcceptanceRatioMB;
    Bool_t correctMeanAcceptanceRatioHT1;
    Bool_t correctMeanAcceptanceRatioHT2;
    Bool_t correctMeanAcceptanceRatioEtaMB;
    Bool_t correctMeanAcceptanceRatioEtaHT1;
    Bool_t correctMeanAcceptanceRatioEtaHT2;

    TString name;
    TString title;

    Bool_t savePtSlicesTrig;
    TString PtSlicesTrigFilename;
    Bool_t savePtSlicesTrigSim;
    TString PtSlicesTrigSimFilename;
    Bool_t savePtSlicesTrigSimEta;
    TString PtSlicesTrigSimEtaFilename;
    Bool_t savePtSlicesTrigSimEtabg;
    TString PtSlicesTrigSimEtabgFilename;

    Bool_t saveDataArrays;
    TString dataArraysFilenameFormat;

    Bool_t saveRunTimes;
    TString runTimesFilenameFormat;
    Bool_t saveRunTimesSim;
    TString runTimesSimFilenameFormat;
    Bool_t saveRunTimesSimEta;
    TString runTimesSimEtaFilenameFormat;
    Bool_t savePrescales;
    TString prescalesSaveFilename;
    TString dbConnectString;

    Float_t triggerThresholdAdd;
    TString prescalesDBFilename;

    Float_t NBinaryCollisions;
    Float_t NBinaryCollisionsErr;

    Float_t crossectionMB;
    Float_t crossectionMBErr;
    Float_t crossectionMBTotalFraction;
    Float_t crossectionMBTotalFractionErr;

    Bool_t usePPpQCD;
    Bool_t plotCrossection;

    ClassDef(TAnalysisSettings, STPI0RESULTS_VERSION);
};

struct TAllSettings : public TNamed {
public:
    TAnalysisSettings settingsDAuNoCentral;
    TAnalysisSettings settingsDAuAllCentral;
    TAnalysisSettings settingsDAuMostCentral;
    TAnalysisSettings settingsDAuMidCentral;
    TAnalysisSettings settingsDAuMostPeripheral;
    TAnalysisSettings settingsPP;

    Bool_t browseAnalysis;
    Bool_t browseResults;

    TString dAuCentralityName;
    TString dAuCentralityTitle;
    Bool_t dAuCentralityShow;
    Bool_t dAuCentralityPrint;
    Bool_t showDAuCentralityTriggersSeparately;

    Bool_t showSpectrumDAu;
    Bool_t showRcp;
    Bool_t showAllCentral;
    Bool_t showMidCentral;
    Bool_t useDAuEff;
    Bool_t useDAuEmbedding;
    Bool_t useDAuEffCentralities;
    Bool_t useDAu1gamma;
    Bool_t useDAu1gammaSim;
    Bool_t useDAu1gammaSimEta;
    Bool_t useDAu1gammaCentralities;
    Bool_t useDAuEffWeight;

    Bool_t showSpectrumDAuEta;
    Bool_t showRcpEta;
    Bool_t useDAuEffEta;
    Bool_t useDAuEffEtabg;
    Bool_t useDAuEmbeddingEta;
    Bool_t useDAuEffCentralitiesEta;
    Bool_t useDAuEffWeightEta;

    Bool_t showSpectrumPP;
    Bool_t usePPEff;
    Bool_t usePP1gamma;
    Bool_t usePP1gammaSim;
    Bool_t usePP1gammaSimEta;
    Bool_t usePPEffWeight;

    Bool_t showSpectrumPPEta;
    Bool_t usePPEffEta;
    Bool_t usePPEffEtabg;
    Bool_t usePPEffWeightEta;

    Bool_t showRDA;
    Bool_t showRDAEta;
    TString RDAName;
    TString RDATitle;
    Bool_t RDAShow;
    Bool_t showRDATriggersSeparately;

    ClassDef(TAllSettings, STPI0RESULTS_VERSION);
};

struct TAnalysisRawResultsTrigger : public TNamed {
public:
    distribution_list_type invlist;
    distribution_list_type invBglist;
    distribution_list_type invBgRandomlist;
    distribution_list_type inv1gammalist;
    distribution_list_type invNeutralMultlist;
    distribution_list_type invPtshiftlist;
    distribution_list_type trackdistlist;
    distribution_list_type trackdistBglist;
    distribution_list_type trackdistBgRandomlist;
    distribution_list_type simulatedPtlist;

    bin_stat_list_type fitLeft;
    bin_stat_list_type fitRight;

    bin_stat_list_type peakLeft;
    bin_stat_list_type peakRight;
    bin_stat_list_type peakPositionForFit;
    bin_stat_list_type peakWidthForFit;
    TFitSettings peakPositionParamFit;
    TFitSettings peakWidthParamFit;
    bin_stat_list_type peakLeftYield;
    bin_stat_list_type peakRightYield;

    bin_stat_list_type peakLeftEta;
    bin_stat_list_type peakRightEta;
    bin_stat_list_type peakPositionForFitEta;
    bin_stat_list_type peakWidthForFitEta;
    TFitSettings peakPositionEtaParamFit;
    TFitSettings peakWidthEtaParamFit;
    bin_stat_list_type peakLeftYieldEta;
    bin_stat_list_type peakRightYieldEta;

    bin_stat_list_type nrebin;

    bin_stat_list_type scaleBg;
    bin_stat_list_type scaleBgRandom;
    bin_stat_list_type scaleBgLowMass;
    bin_stat_list_type scalePeakShape;
    bin_stat_list_type scalePeakShapeEta;

    bin_stat_list_type scalePeakShapeLowMass;
    bin_stat_list_type scalePeakShapeEtaLowMass;
    bin_stat_list_type scalePeakShapeEtabgLowMass;

    bin_stat_list_type jetBgFractionForFit;
    TFitSettings jetBgFractionParamFit;
    TFitSettings jetBgFractionCandidatesFit;

    bin_stat_list_type fitParameter6;
    bin_stat_list_type fitParameter7;

    bin_stat_list_type peakAreaShareBorder;

    bin_stat_list_type rawYield;
    bin_stat_list_type peakPosition;
    bin_stat_list_type peakWidth;
    bin_stat_list_type peakPositionData;
    bin_stat_list_type peakWidthData;
    bin_stat_list_type peakBackground;
    bin_stat_list_type peakSigBackground;
    TFitSettings peakSigBackgroundParamFit;

    bin_stat_list_type rawYieldEta;
    bin_stat_list_type peakPositionEta;
    bin_stat_list_type peakWidthEta;
    bin_stat_list_type peakPositionDataEta;
    bin_stat_list_type peakWidthDataEta;
    bin_stat_list_type peakBackgroundEta;
    bin_stat_list_type peakSigBackgroundEta;
    TFitSettings peakSigBackgroundEtaParamFit;

    bin_stat_list_type bgPar6;
    bin_stat_list_type bgPar7;
    TFitSettings bgPar6ParamFit;
    TFitSettings bgPar7ParamFit;
    bin_stat_list_type fitChi2Ndf;
    bin_stat_list_type bgJetToTotal;
    bin_stat_list_type bgJetToTotalFirst;

    bin_stat_list_type areaFractionBg;
    TFitSettings areaFractionBgParamFit;

    bin_stat_list_type cpvCorrection;
    TFitSettings cpvCorrectionParamFit;
    //bin_stat_list_type cpvCorrectionSim;
    //TFitSettings cpvCorrectionParamSimFit;
    //bin_stat_list_type cpvCorrectionSimEta;
    //TFitSettings cpvCorrectionParamSimEtaFit;
    TFitSettings cpvCorrectionFit;
    bin_stat_list_type cpvCorrectionParam6;
    bin_stat_list_type cpvCorrectionParam7;

    Float_t prescaleUsed;
    Float_t lowNormPoints;
    Float_t lowNormPointsPt;
    bin_stat_list_type lowNormPointsPtCorr;
    Float_t evNum;
    Float_t evNumErr;
    Int_t evNumSeenByMaker;
    Int_t evNumPassedAllCuts;
    Float_t totalRawYield;
    Float_t totalRawYieldEta;
    Float_t scalePeakShapeEtabgRel;
    Float_t deltaEta;
    Float_t vertexFindingEff;
    Float_t meanAcceptanceBTOW;
    Float_t meanAcceptanceBPRS;
    Float_t meanAcceptanceBSMDE;
    Float_t meanAcceptanceBSMDP;

    ClassDef(TAnalysisRawResultsTrigger, STPI0RESULTS_VERSION);
};

struct TAnalysisResults : public TNamed {
public:
    TAnalysisRawResultsTrigger resultsMB;
    TAnalysisRawResultsTrigger resultsHT1;
    TAnalysisRawResultsTrigger resultsHT2;
    TAnalysisRawResultsTrigger resultsSimMB;
    TAnalysisRawResultsTrigger resultsSimHT1;
    TAnalysisRawResultsTrigger resultsSimHT2;
    TAnalysisRawResultsTrigger resultsSimEtaMB;
    TAnalysisRawResultsTrigger resultsSimEtaHT1;
    TAnalysisRawResultsTrigger resultsSimEtaHT2;
    TAnalysisRawResultsTrigger resultsSimEtabgMB;
    TAnalysisRawResultsTrigger resultsSimEtabgHT1;
    TAnalysisRawResultsTrigger resultsSimEtabgHT2;

    Int_t year;

    bin_stat_list_type areaSimuMB;
    bin_stat_list_type areaSimuHT1;
    bin_stat_list_type areaSimuHT2;
    Float_t totSimuYield;
    Float_t totSimuYieldMB;
    Float_t totSimuYieldHT1;
    Float_t totSimuYieldHT2;
    bin_stat_list_type areaSimuEtaMB;
    bin_stat_list_type areaSimuEtaHT1;
    bin_stat_list_type areaSimuEtaHT2;
    Float_t totSimuYieldEta;
    Float_t totSimuYieldEtaMB;
    Float_t totSimuYieldEtaHT1;
    Float_t totSimuYieldEtaHT2;
    bin_stat_list_type areaSimuEtabgMB;
    bin_stat_list_type areaSimuEtabgHT1;
    bin_stat_list_type areaSimuEtabgHT2;
    Float_t totSimuYieldEtabg;
    Float_t totSimuYieldEtabgMB;
    Float_t totSimuYieldEtabgHT1;
    Float_t totSimuYieldEtabgHT2;

    bin_stat_list_type cpvCorrection;
    bin_stat_list_type cpvCorrectionSim;
    bin_stat_list_type cpvCorrectionSimEta;
    TFitSettings cpvCorrectionFit;
    TFitSettings cpvCorrectionFitSim;
    TFitSettings cpvCorrectionFitSimEta;

    Float_t meanAcceptanceRatioMB;
    Float_t meanAcceptanceRatioHT1;
    Float_t meanAcceptanceRatioHT2;
    Float_t meanAcceptanceRatioEtaMB;
    Float_t meanAcceptanceRatioEtaHT1;
    Float_t meanAcceptanceRatioEtaHT2;

    Float_t prescaleTriggerBiasMBHT1;
    Float_t prescaleTriggerBiasMBHT2;
    Float_t prescaleTriggerBiasHT1HT2;

    bin_stat_list_type effMB;
    bin_stat_list_type effHT1;
    bin_stat_list_type effHT2;
    bin_stat_list_type effMBSmooth;
    bin_stat_list_type effHT1Smooth;
    bin_stat_list_type effHT2Smooth;
    bin_stat_list_type effMBSmoothPoint;
    bin_stat_list_type effHT1SmoothPoint;
    bin_stat_list_type effHT2SmoothPoint;
    bin_stat_list_type spectrumMBembed;
    bin_stat_list_type spectrumHT1embed;
    bin_stat_list_type spectrumHT2embed;
    bin_stat_list_type spectrumembed;
    bin_stat_list_type correctedAreaMB;
    bin_stat_list_type correctedAreaHT1;
    bin_stat_list_type correctedAreaHT2;
    bin_stat_list_type correctedMB;
    bin_stat_list_type correctedHT1;
    bin_stat_list_type correctedHT2;
    bin_stat_list_type spectrumMB;
    bin_stat_list_type spectrumHT1;
    bin_stat_list_type spectrumHT2;
    bin_stat_list_type spectrum;
    bin_stat_list_type spectrumMBDiv;
    bin_stat_list_type spectrumHT1Div;
    bin_stat_list_type spectrumHT2Div;
    bin_stat_list_type spectrumDiv;
    bin_stat_list_type crossectionMB;
    bin_stat_list_type crossectionHT1;
    bin_stat_list_type crossectionHT2;
    bin_stat_list_type crossection;
    bin_stat_list_type crossectionMBDiv;
    bin_stat_list_type crossectionHT1Div;
    bin_stat_list_type crossectionHT2Div;
    bin_stat_list_type crossectionDiv;

    bin_stat_list_type effMBEta;
    bin_stat_list_type effHT1Eta;
    bin_stat_list_type effHT2Eta;
    bin_stat_list_type effMBSmoothEta;
    bin_stat_list_type effHT1SmoothEta;
    bin_stat_list_type effHT2SmoothEta;
    bin_stat_list_type effMBSmoothPointEta;
    bin_stat_list_type effHT1SmoothPointEta;
    bin_stat_list_type effHT2SmoothPointEta;
    bin_stat_list_type spectrumMBembedEta;
    bin_stat_list_type spectrumHT1embedEta;
    bin_stat_list_type spectrumHT2embedEta;
    bin_stat_list_type spectrumembedEta;
    bin_stat_list_type correctedAreaMBEta;
    bin_stat_list_type correctedAreaHT1Eta;
    bin_stat_list_type correctedAreaHT2Eta;
    bin_stat_list_type correctedMBEta;
    bin_stat_list_type correctedHT1Eta;
    bin_stat_list_type correctedHT2Eta;
    bin_stat_list_type spectrumMBEta;
    bin_stat_list_type spectrumHT1Eta;
    bin_stat_list_type spectrumHT2Eta;
    bin_stat_list_type spectrumEta;
    bin_stat_list_type spectrumMBDivEta;
    bin_stat_list_type spectrumHT1DivEta;
    bin_stat_list_type spectrumHT2DivEta;
    bin_stat_list_type spectrumDivEta;
    bin_stat_list_type crossectionMBEta;
    bin_stat_list_type crossectionHT1Eta;
    bin_stat_list_type crossectionHT2Eta;
    bin_stat_list_type crossectionEta;
    bin_stat_list_type crossectionMBDivEta;
    bin_stat_list_type crossectionHT1DivEta;
    bin_stat_list_type crossectionHT2DivEta;
    bin_stat_list_type crossectionDivEta;

    bin_stat_list_type etaToPi0Ratio;
    bin_stat_list_type etaToPi0RatioMB;
    bin_stat_list_type etaToPi0RatioHT1;
    bin_stat_list_type etaToPi0RatioHT2;

    bin_stat_list_type ptCenterShiftMB;
    bin_stat_list_type ptCenterShiftHT1;
    bin_stat_list_type ptCenterShiftHT2;
    bin_stat_list_type ptShiftCorrMB;
    bin_stat_list_type ptShiftCorrHT1;
    bin_stat_list_type ptShiftCorrHT2;

    Float_t NBinaryCollisions;
    Float_t NBinaryCollisionsErr;
    Float_t PercentCentralityLow;
    Float_t PercentCentralityHigh;

    bin_stat_list_type refMultEast;
    bin_stat_list_type refMultWest;

    ClassDef(TAnalysisResults, STPI0RESULTS_VERSION);
};

struct TAllResults : public TNamed {
public:
    TAnalysisResults resultsDAuNoCentral;
    TAnalysisResults resultsDAuAllCentral;
    TAnalysisResults resultsDAuMostCentral;
    TAnalysisResults resultsDAuMidCentral;
    TAnalysisResults resultsDAuMostPeripheral;
    TAnalysisResults resultsPP;

    bin_stat_list_type dAuRcpMB;
    bin_stat_list_type dAuRcpHT1;
    bin_stat_list_type dAuRcpHT2;
    bin_stat_list_type dAuRcp;
    bin_stat_list_type dAuRcpMBEta;
    bin_stat_list_type dAuRcpHT1Eta;
    bin_stat_list_type dAuRcpHT2Eta;
    bin_stat_list_type dAuRcpEta;

    bin_stat_list_type RDAMB;
    bin_stat_list_type RDAHT1;
    bin_stat_list_type RDAHT2;
    bin_stat_list_type RDA;
    bin_stat_list_type RDAMBEta;
    bin_stat_list_type RDAHT1Eta;
    bin_stat_list_type RDAHT2Eta;
    bin_stat_list_type RDAEta;

    ClassDef(TAllResults, STPI0RESULTS_VERSION);
};

TCanvas *findPeak(const distribution_type *distribution
		, const distribution_type *distributionBg
		, const distribution_type *distributionBgRandom
		, const distribution_type *distributionBgLowMass
		, const distribution_type *distributionNeutralMult
		, const distribution_type *distributionPeakShape
		, const distribution_type *distributionPeakShapeLowMass
		, const distribution_type *distributionPeakShapeEta
		, const distribution_type *distributionPeakShapeEtaLowMass
		, const distribution_type *distributionPeakShapeEtabg
		, const distribution_type *distributionPeakShapeEtabgLowMass
                , Int_t numBinsToShow, Int_t binToShow, TCanvas *canvas
		, const TAnalysisSettingsTrigger &analysisSettings
		, TAnalysisRawResultsTrigger &analysisResults
);

TCanvas *findPeaks(const TAnalysisSettingsTrigger &analysisSettings, TAnalysisRawResultsTrigger &analysisResults, const TAnalysisRawResultsTrigger *analysisResultsSim, const TAnalysisRawResultsTrigger *analysisResultsSimEta, const TAnalysisRawResultsTrigger *analysisResultsSimEtabg);

TCanvas *fitSpectraBins(const TAnalysisSettingsTrigger &analysisSettings, TAnalysisRawResultsTrigger &analysisResults, const TAnalysisRawResultsTrigger *analysisResultsSim, const TAnalysisRawResultsTrigger *analysisResultsSimEta, const TAnalysisRawResultsTrigger *analysisResultsSimEtabg);

Float_t getScaleFactorFromPt(TString name, TString title, const TPointDataProcessor *proc1, const TPointDataProcessor *proc2, Float_t lowPt, Float_t highPt, const distribution_list_type &invlist, bin_stat_list_type &lowNormPointsPtCorr, Bool_t show, Bool_t print);

void getCentralityBin(Int_t RefMultLow, Int_t RefMultHigh, Float_t &percentLow, Float_t &percentHigh, Float_t &Nbin, Float_t &NbinErr);

void calculateBinPurityMatrix(const distribution_list_type &simulatedPtDistributions
			, const bin_stat_list_type &areaIn
			, const bin_stat_list_type &areaInSim
			, const bin_stat_list_type &areaInSimReco
			, bin_stat_list_type &areaOut
			, bin_stat_list_type &areaOutR
);

void calculateBinPurity(const TCandidateDataProcessor *candidate
			, const list<TInvariantMassDistribution> &inv
			, const Char_t *shiftFuncName, const Char_t *shiftFunc
			, TF1* *fPtShift
			, const Char_t *shiftCanvasName, const Char_t *shiftCanvasTitle
			, bool show
);

extern TF2 *ptBinPosFuncFromQCD;
void clearPtBinPosFuncFromQCD();
void createPtBinPosFuncFromQCD(Double_t (* getNLOFunc) (Double_t*, Double_t*));
void calculatePtShiftCorrInv(const list<TInvariantMassDistribution> &inv, const TF1* fPtShift, const TF2* fBinCenter, bin_stat_list_type &corr, bin_stat_list_type &centerShift);
void correctPtShiftInv(list<TInvariantMassDistribution> &inv, const TF1* fPtShift, const TF2* fBinCenter);
void correctPtShiftBins(bin_stat_list_type &bins, const TF1* fPtShift, const TF2* fBinCenter);

void selectInv(const TCandidateDataProcessor *proc, Float_t binSize, Float_t pTLow, Float_t pTHigh, Int_t mult, list<TInvariantMassDistribution> &binlist, const TPointDataProcessor *pointDataProcessor = 0, bool debug = false);
void selectInvSim(const TSimuDataProcessor *proc, Float_t binSize, Float_t pTLow, Float_t pTHigh, bin_stat_list_type &binlist);

void calculatePSFromPoints(const Char_t *name, const Char_t *title
	, const TDataProcessorPool *poolMB, const TDataProcessorPool *poolHT1, const TDataProcessorPool *poolHT2
	, const TDataProcessorPool *pool1gammaMB, const TDataProcessorPool *pool1gammaHT1, const TDataProcessorPool *pool1gammaHT2
	, Bool_t chargedPoints, Bool_t allPoints
	, Float_t psPointsHT1Low, Float_t psPointsHT1High, Float_t psPointsHT2Low, Float_t psPointsHT2High, Float_t binSize
	, Float_t psPointsHT1Lowi, Float_t psPointsHT1Highi, Float_t psPointsHT2Lowi, Float_t psPointsHT2Highi
	, Float_t corrMBHT1, Float_t corrMBHT2, Float_t corrHT1HT2
	, Bool_t show, Bool_t print
	, Float_t &PSHT1, Float_t &PSHT2
	, Float_t &PSHT1Intergal, Float_t &PSHT2Integral
);

void calculatePSFromSim(const Char_t *name, const Char_t *title, const TDataProcessorPool *poolMB, const TDataProcessorPool *poolHT1
	, Float_t corrMBHT1, Float_t corrMBHT2, Float_t corrHT1HT2
	, Bool_t showReal, Bool_t print, Float_t &PSHT1, Float_t &PSHT2);

void calculatePSFromDB(const Int_t runInd, const Int_t *runNums, const Int_t *evNumsMB, const Int_t *evNumsHT1, const Int_t *evNumsHT2, const Int_t *psMB, const Int_t *psHT1, const Int_t *psHT2, Float_t &PS_HT1MB, Float_t &PS_HT2MB, Float_t &PS_HT1MB_alexst, Float_t &PS_HT2MB_alexst, Bool_t print);
extern Int_t *prescalesRuns;
extern Int_t *prescalesMB;
extern Int_t *prescalesHT1;
extern Int_t *prescalesHT2;
extern Int_t prescalesNum;
void clearCachedPrescales();
void calculatePSFromDB(const Char_t *name, const Char_t *title, const Char_t *psFilename
    , const TEventDataProcessor *eventDataProcessorMB, const TEventDataProcessor *eventDataProcessorHT1, const TEventDataProcessor *eventDataProcessorHT2
    , Bool_t show, Bool_t print, Float_t &PSHT1, Float_t &PSHT2, Float_t &PSHT1_alexst, Float_t &PSHT2_alexst
);

void getPrescales(const TEventDataProcessor *eventsMB, const TEventDataProcessor *eventsHT1, const TEventDataProcessor *eventsHT2, const Char_t *prescalesFilename, bool print = true, const Char_t *dbStr = 0, const Int_t *ourMinBiasTriggers = 0, const Int_t *ourHt1Triggers = 0, const Int_t *ourHt2Triggers = 0);
void getRunTimes(const TEventDataProcessor *events, const Char_t *runTimesFilename, bool print = true, const Char_t *dbStr = 0);

Int_t getRunYear(const TEventDataProcessor *events);

TH1F *showList(const bin_stat_list_type *points, const Char_t *name, const Char_t *title, TH1F *oldHist, TLegend* *legend
	, const TAnalysisSettingsTrigger &settings
	, Bool_t show = true
);

TH1F *showLists(const bin_stat_list_type *pointsMB, const bin_stat_list_type *pointsHT1, const bin_stat_list_type *pointsHT2
        , const Char_t *name, const Char_t *title
        , TH1F *oldHist, TLegend* *legend
	, const TAnalysisSettingsTrigger &settingsMB, const TAnalysisSettingsTrigger &settingsHT1, const TAnalysisSettingsTrigger &settingsHT2
        , Bool_t show = true
);

void showResults(
          const TDataProcessorPool *poolMB, const TDataProcessorPool *poolHT1, const TDataProcessorPool *poolHT2
        , const TDataProcessorPool *poolEmbedMB, const TDataProcessorPool *poolEmbedHT1, const TDataProcessorPool *poolEmbedHT2
        , const TDataProcessorPool *poolEmbedEtaMB, const TDataProcessorPool *poolEmbedEtaHT1, const TDataProcessorPool *poolEmbedEtaHT2
        , const TDataProcessorPool *poolEmbedEtabgMB, const TDataProcessorPool *poolEmbedEtabgHT1, const TDataProcessorPool *poolEmbedEtabgHT2
        , const TDataProcessorPool *pool1gamma, const TDataProcessorPool *pool1gammaSim
        , const TDataProcessorPool *poolPtshiftMB, const TDataProcessorPool *poolPtshiftHT1, const TDataProcessorPool *poolPtshiftHT2
	, const TAnalysisSettings &analysisSettings
	, Bool_t deletePools
	, TAnalysisResults &analysisResults
);

void savePtSlices(const Char_t *filenameOut
    , const list<TInvariantMassDistribution> &invlistMB
    , const list<TInvariantMassDistribution> &invlistHT1
    , const list<TInvariantMassDistribution> &invlistHT2
    , Float_t triggerThresholdMB
    , Float_t triggerThresholdHT1
    , Float_t triggerThresholdHT2
    , TCanvas *peakFinderMBcanvas
    , TCanvas *peakFinderHT1canvas
    , TCanvas *peakFinderHT2canvas
);

void showResultsOthers(TH1 *histSpectrum, TLegend *legendSpectrum, const TCandidateDataProcessor *candidateDataProcessorEmbedMB
    , Bool_t showWeightUsed, Bool_t showpQCDWeight, Bool_t usePPpQCD);

void showResultsOthersEtaToPi0(TH1 *hist, TLegend *legend);

void showResultsOthersDiv(TH1 *histDiv, TLegend *legend, TF1 *weightDiv, Bool_t usePPpQCD);

void showResultsOthersDAuRcp(TH1 *histRcp, TLegend *legendRcp
	, const TAnalysisResults &resultsDAuMostCentral
	, const TAnalysisResults &resultsDAuMostPeripheral
);

void showPointsRcp(const Char_t *name, const Char_t *title
	, const TDataProcessorPool *poolMostcentral, const TDataProcessorPool *poolMostperipheral
	, Bool_t chargedPoints, Bool_t allPoints
	, Bool_t showSpectrum, Bool_t show, Bool_t print
	, Float_t binSizeMB, Float_t binSizeHT1, Float_t binSizeHT2
	, Float_t minPtMB, Float_t minPtHT1, Float_t minPtHT2
	, Float_t maxPtMB, Float_t maxPtHT1, Float_t maxPtHT2
);
void showPointsRcpEtaPhiCoord(const Char_t *name, const Char_t *title
	, const TDataProcessorPool *poolMostcentral, const TDataProcessorPool *poolMostperipheral
	, Bool_t chargedPoints, Bool_t allPoints
	, Bool_t show, Bool_t print
	, Float_t binSizeMBPt, Float_t binSizeHT1Pt, Float_t binSizeHT2Pt
	, Float_t binSizeEta, Float_t binSizePhi
	, Float_t minPtMB, Float_t minPtHT1, Float_t minPtHT2
	, Float_t maxPtMB, Float_t maxPtHT1, Float_t maxPtHT2
	, Float_t integralLow, Float_t maxError
);

void showResultsDAuCentrality(const Char_t *dAuCentralityName, const Char_t *dAuCentralityTitle
	, Bool_t showRcp, Bool_t showRcpEta, Bool_t dAuCentralityShow, Bool_t dAuCentralityPrint, Bool_t showDAuCentralityTriggersSeparately
	, const TAnalysisSettings &settingsDAuNoCentral
	, const TAnalysisSettings &settingsDAuAllCentral
	, const TAnalysisSettings &settingsDAuMostCentral
	, const TAnalysisSettings &settingsDAuMidCentral
	, const TAnalysisSettings &settingsDAuMostPeripheral 
	, const TAnalysisResults &resultsDAuNoCentral
	, const TAnalysisResults &resultsDAuMostCentral
	, const TAnalysisResults &resultsDAuMidCentral
	, const TAnalysisResults &resultsDAuMostPeripheral
	, bin_stat_list_type &dAuRcpMB
	, bin_stat_list_type &dAuRcpHT1
	, bin_stat_list_type &dAuRcpHT2
	, bin_stat_list_type &dAuRcp
	, bin_stat_list_type &dAuRcpMBEta
	, bin_stat_list_type &dAuRcpHT1Eta
	, bin_stat_list_type &dAuRcpHT2Eta
	, bin_stat_list_type &dAuRcpEta
);

void showResultsRDA(const Char_t *RDAName, const Char_t *RDATitle
	, Bool_t showRDA, Bool_t showRDAEta, Bool_t RDAShow, Bool_t showRDATriggersSeparately
	, const TAnalysisSettings &settingsDAuNoCentral
	, const TAnalysisSettings &settingsPP
	, const TAnalysisResults &resultsDAuNoCentral
	, const TAnalysisResults &resultsPP
	, bin_stat_list_type &RDAMB
	, bin_stat_list_type &RDAHT1
	, bin_stat_list_type &RDAHT2
	, bin_stat_list_type &RDA
	, bin_stat_list_type &RDAMBEta
	, bin_stat_list_type &RDAHT1Eta
	, bin_stat_list_type &RDAHT2Eta
	, bin_stat_list_type &RDAEta
);

void calculateBunchCrossingId7bitOffset(const TEventDataProcessor *eventDataProcessorAllValid
	, bool show, bool print, const Char_t *outputFilename
	, const Char_t *outputHighbgFilename, const Char_t *highBgArrayname
	, Float_t bgFractionCut, Bool_t checkAbortGaps, Bool_t checkEmptyBunches);

void outputCArrays(ostream &ostr, const bin_stat_list_type &values, const Char_t *title, const Char_t *suffix);

void showAnalysis(const Char_t *DATA_DIR, const TAllSettings &allSettings, TAllResults &allResults);

void calculateVertexFindingEff(const Char_t *title, Bool_t calculate, const TEventDataProcessor *event, Float_t &eff);

void calculateMeanAcceptance(const TEventDataProcessor *eventData, Float_t &acceptanceBTOW, Float_t &acceptanceBPRS, Float_t &acceptanceBSMDE, Float_t &acceptanceBSMDP);

void calculatePrescaleTriggerBias(const TDataProcessorPool *pool, const TAnalysisSettings &settings, TAnalysisResults &results, Int_t step);

class StPi0ResultsUtil {public: Int_t i;}; // To make RootCint happy

#endif
