#ifndef STPI0RESULTSFINAL
#define STPI0RESULTSFINAL

#include "StPi0ResultsUtil.h"
#include "StPi0ResultsVersion.h"

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
    Bool_t saveRcpDataArrays;
    TString dataArraysRcpFilenameFormat;
    Bool_t showAllCentral;
    Bool_t showMidCentral;
    Bool_t useDAuEff;
    Bool_t useDAuEmbedding;
    Bool_t useDAuEffCentralities;
    Bool_t useDAu1gamma;
    Bool_t useDAu1gammaSim;
    Bool_t useDAu1gammaSimEta;
    Bool_t useDAu1gammaCentralities;
    Bool_t useDAunbar;
    Bool_t useDAunbarSim;
    Bool_t useDAunbarSimEta;
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
    Bool_t usePPEffPythia;
    Bool_t usePPnbar;
    Bool_t usePPnbarSim;
    Bool_t usePPnbarSimEta;

    Bool_t showSpectrumPPEta;
    Bool_t usePPEffEta;
    Bool_t usePPEffEtabg;
    Bool_t usePPEffWeightEta;

    data_points_list dataPointsR;

    Bool_t showRDA;
    Bool_t showRDAEta;
    Bool_t showRDAGamma;
    TString RDAName;
    TString RDATitle;
    Bool_t RDAShow;
    Bool_t showRDATriggersSeparately;
    Bool_t saveRdADataArrays;
    TString dataArraysRdAFilenameFormat;

    ClassDef(TAllSettings, STPI0RESULTS_VERSION);
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
    bin_stat_list_type RDAMBGamma;
    bin_stat_list_type RDAHT1Gamma;
    bin_stat_list_type RDAHT2Gamma;
    bin_stat_list_type RDAGamma;

    ClassDef(TAllResults, STPI0RESULTS_VERSION);
};

void showAnalysis(const Char_t *DATA_DIR, const TAllSettings &allSettings, TAllResults &allResults);

void show_analysis_final(const Char_t *DATA_DIR);

#endif
