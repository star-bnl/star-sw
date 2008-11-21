#ifndef StPi0DataMaker_StPi0DataSaveMaker_H
#define StPi0DataMaker_StPi0DataSaveMaker_H

class TTree;
class TH1F;
class TFile;
class TClonesArray;

#include <StMaker.h>

#include <StEmcPool/StPi0Common/StPi0CommonUtil.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

class StPi0DataSaveMaker : public StMaker {
public:
  typedef StMaker inherited;

  StPi0DataSaveMaker(const Char_t *name = "StPi0DataSaveMaker");
  virtual ~StPi0DataSaveMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  
  TString outputFileName;
  TMyDataAnalysisSettings settings;

  void save();

  ClassDef(StPi0DataSaveMaker, 1)

protected:
  TFile *mFile;

  TClonesArray *mMCGammaTreeDataArray;
  Int_t mMCGammaIndex;
  TTree* mMCGammaTree;
  TMyMCParticleTreeData mMCGammaPlainStructure;
  TTree* mMCGammaTreePlain;

  TClonesArray *mMCPionTreeDataArray;
  Int_t mMCPionIndex;
  TTree* mMCPionTree;
  TMyMCDecayTreeData mMCPionPlainStructure;
  TTree* mMCPionTreePlain;

  TClonesArray *mMCEtaTreeDataArray;
  Int_t mMCEtaIndex;
  TTree* mMCEtaTree;
  TMyMCDecayTreeData mMCEtaPlainStructure;
  TTree* mMCEtaTreePlain;

  TClonesArray *mMCNbarTreeDataArray;
  Int_t mMCNbarIndex;
  TTree* mMCNbarTree;
  TMyMCParticleTreeData mMCNbarPlainStructure;
  TTree* mMCNbarTreePlain;

  TClonesArray *mCandidateTreeDataArray;
  Int_t mCandidateIndex;
  TTree* mCandidateTree;
  TMyCandidateTreeData mCandidatePlainStructure;
  TTree* mCandidateTreePlain;

  TClonesArray *mCandidateTreeDataMixArray;
  Int_t mCandidateMixIndex;
  TTree* mCandidateMixTree;
  TMyCandidateTreeData mCandidateMixPlainStructure;
  TTree* mCandidateMixTreePlain;

  TClonesArray *mCandidateTreeDataSubmixArray;
  Int_t mCandidateSubmixIndex;
  TTree* mCandidateSubmixTree;
  TMyCandidateTreeData mCandidateSubmixPlainStructure;
  TTree* mCandidateSubmixTreePlain;

  TClonesArray *mEventTreeDataArray;
  Int_t mEventIndex;
  TTree* mEventTree;
  TMyEventTreeData mEventPlainStructure;
  TTree* mEventTreePlain;

  TClonesArray *mHitTreeDataArray;
  Int_t mHitIndex;
  TTree* mHitTree;
  TMyHitTreeData mHitPlainStructure;
  TTree* mHitTreePlain;

  TClonesArray *mClusterTreeDataArray;
  Int_t mClusterIndex;
  TTree* mClusterTree;
  TMyClusterTreeData mClusterPlainStructure;
  TTree* mClusterTreePlain;

  TClonesArray *mPointTreeDataArray;
  Int_t mPointIndex;
  TTree* mPointTree;
  TMyPointTreeData mPointPlainStructure;
  TTree* mPointTreePlain;

  TClonesArray *mSMDThresholdTreeDataArray;
  Int_t mSMDThresholdIndex;
  TTree* mSMDThresholdTree;
  TMySMDThresholdTreeData mSMDThresholdPlainStructure;
  TTree* mSMDThresholdTreePlain;

  TH1F* mEventSummary;
  TH1F* mTriggerSummary;

  Int_t mEventCounter;
};

#endif
