#ifndef StPi0Common_Util_H
#define StPi0Common_Util_H

#include <RVersion.h>
#include <TObject.h>
#include <TNamed.h>
#include <TString.h>
#include <TPave.h>

extern const Float_t EMCRadius;
extern const Float_t truePionMass;
extern const Float_t truePionBranchingRatio;
extern const Float_t trueEtaMass;
extern const Float_t trueEtaBranchingRatio;
extern const Float_t trueOmegaMass;

struct TMyDataAnalysisSettings : public TObject {
public:
    Int_t StPi0Common_Version_data;
    Int_t StPi0Common_Version_analysis;
    Int_t StPi0Common_Version_results;
    Int_t StPi0DataStructures_Version_data;
    Int_t StPi0DataStructures_Version_analysis;
    Int_t StPi0DataMaker_Version_data;
    Int_t StPi0DataSaveMaker_Version_data;
    Int_t StTimeRandomizerMaker_Version_data;
    Int_t StPi0Analysis_Version_analysis;
    Int_t StPi0Analysis_Version_results;
    Int_t StPi0Results_Version_results;

    Bool_t isSimulation;
    Bool_t isEmbedding;
    Bool_t isPythia;
    Bool_t saveHits;
    Bool_t saveHitsPlain;
    Bool_t saveClusters;
    Bool_t saveClustersPlain;
    Bool_t savePoints;
    Bool_t savePointsPlain;
    Bool_t useLocalDB;
    TString localDBpath;
    TString timestampsFilename;
    Float_t timeRandomNormEventsTotal;
    Bool_t useSimCalibFlavour;
    Bool_t saveCandidates;
    Bool_t saveCandidatesPlain;
    Bool_t saveCandidatesMixed;
    Bool_t saveCandidatesMixedPlain;
    Bool_t saveCandidatesSubmixed;
    Bool_t saveCandidatesSubmixedPlain;
    Bool_t saveCandidatesWithoutSMD;
    Bool_t saveCandidatesWithoutSMDBoth;
    Bool_t saveMCGammas;
    Bool_t saveMCGammasPlain;
    Bool_t saveFirstMCGammaOnly;
    Bool_t saveMCPions;
    Bool_t saveMCPionsPlain;
    Bool_t saveFirstMCPionOnly;
    Bool_t saveMCEtas;
    Bool_t saveMCEtasPlain;
    Bool_t saveFirstMCEtaOnly;
    Bool_t saveMCNbars;
    Bool_t saveMCNbarsPlain;
    Bool_t saveFirstMCNbarOnly;
    Bool_t saveEvents;
    Bool_t saveEventsPlain;
    Bool_t saveSMDThreshold;
    Bool_t saveSMDThresholdPlain;
    Float_t smd1Threshold;
    Float_t smd2Threshold;
    Float_t smd3Threshold;
    Bool_t smdThresholdEnergy;
    Bool_t smdThresholdEt;
    Int_t saveInterval;
    Int_t clonesArraySize;
    Int_t basketSize;
    Int_t splitLevel;
    Int_t compressionLevel;
    Int_t HT1Threshold;
    Int_t HT2Threshold;
    Int_t TriggerAdc;
    Bool_t useTriggerSimulatorOriginal;
    Bool_t useTriggerSimulatorEmbed;
    Int_t MixedEventsNumber;
    Bool_t MixingClassZ;
    Float_t MixingClassZSize;
    Bool_t MixingClassBemcMult;
    Float_t MixingClassBemcMultSize;
    Bool_t MixingClassTrigger;
    Bool_t MixingClassJetEta;
    Float_t MixingClassJetEtaSize;
    Bool_t MixingClassJetPhi;
    Float_t MixingClassJetPhiSize;
    Bool_t MixingClassJetET;
    Float_t MixingClassJetETSize;
    Int_t SubmixedEventsNumber;
    Bool_t ShuffleSubmixEnergy;
    Bool_t ShuffleSubmixEta;
    Bool_t ShuffleSubmixPhi;
    Int_t ClusterSizeMaxTower;
    Float_t ClusterEnSeedTower;
    Float_t ClusterEnAddTower;
    Float_t ClusterEnTotTower;
    Bool_t ClusterCheckTower;
    Int_t ClusterSizeMaxPreshower;
    Float_t ClusterEnSeedPreshower;
    Float_t ClusterEnAddPreshower;
    Float_t ClusterEnTotPreshower;
    Bool_t ClusterCheckPreshower;
    Int_t ClusterSizeMaxSMDE;
    Float_t ClusterEnSeedSMDE;
    Float_t ClusterEnAddSMDE;
    Float_t ClusterEnTotSMDE;
    Bool_t ClusterCheckSMDE;
    Int_t ClusterSizeMaxSMDP;
    Float_t ClusterEnSeedSMDP;
    Float_t ClusterEnAddSMDP;
    Float_t ClusterEnTotSMDP;
    Bool_t ClusterCheckSMDP;
    Float_t CalibSpreadTower;
    Float_t CalibOffsetTower;
    Float_t CalibSpreadPreshower;
    Float_t CalibOffsetPreshower;
    Float_t CalibSpreadSMDE;
    Float_t CalibOffsetSMDE;
    Float_t CalibSpreadSMDP;
    Float_t CalibOffsetSMDP;
    Float_t CrosstalkTower;
    Float_t CrosstalkPreshower;
    Float_t CrosstalkSMDE;
    Float_t CrosstalkSMDP;
    Float_t jetConeRadius;
    UInt_t triggers[32];
    ULong_t triggersSim;
    ULong_t triggersMB;
    ULong_t triggersHT1;
    ULong_t triggersHT2;
    TString adcToEMakerName;
    Bool_t useFullJetMaker;
    Bool_t useSpinDbMaker;
    TString jetMakerName;
    TString triggerFullSimulatorName;
    TString triggerFullSimulatorNameEmbed;
    TString triggerFullSimulatorNameFinal;
    Bool_t jetFullMakerDoTowerSwapFix;
    Bool_t jetFullMakerUse2003Cuts;
    Bool_t jetFullMakerUse2005Cuts;
    Bool_t jetFullMakerUse2006Cuts;
    TString jetFullMakerBranchName;
    TString spinDbMakerName;
    TString dataMakerName;
    TString datasetNameStEvent;
    TString datasetNameStMcEvent;
    Bool_t doTowerSwapFix;

    ClassDef(TMyDataAnalysisSettings, 20);
};

TString findFile(const Char_t *filename);

Int_t floatCompare(const Float_t &f1, const Float_t &f2, Float_t relPrec = 1e-5);

Float_t getPseudorapidity(Float_t m, Float_t pT, Float_t y);
Float_t getRapidity(Float_t m, Float_t pT, Float_t eta);

class TH1;
class TLegend;
class TLatex;
class TVirtualPad;
void setHistFontSize(TH1 *hist, Float_t padWidthPt = 356 /*B5 text width*/, Float_t fontSizePt = 10 /*10pt*/, const TVirtualPad *pad = 0 /*gPad*/);
void setLegendFontSize(TLegend *legend, Float_t padWidthPt = 356 /*B5 text width*/, Float_t fontSizePt = 10 /*10pt*/, const TVirtualPad *pad = 0 /*gPad*/);
void setLatexFontSize(TLatex *latex, Float_t padWidthPt = 356 /*B5 text width*/, Float_t fontSizePt = 10 /*10pt*/, const TVirtualPad *pad = 0 /*gPad*/);

void SetShadowColor(TPave *pave, Int_t color);

class StPi0CommonUtil {public: Int_t i;}; // To make RootCint happy

#endif
