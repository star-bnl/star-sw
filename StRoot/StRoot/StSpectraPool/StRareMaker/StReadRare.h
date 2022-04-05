//
//
// author: struck
//
// read rare tree and fill histos

#include <Ttypes.h>
#include <TChain.h>
#include <TString.h>
#include <TFile.h>
#include <TH1.h>
#include <TH2.h>

class StReadRare
{
 public:
      StReadRare();
      StReadRare(Int_t nEvents, Char_t* histoFileName="RareEventPlots.root");
      virtual ~StReadRare(){};
      void  Init();
      Int_t Run();
      void  PrintInfo();
      Int_t Finish();


      Int_t    mNEvents;
      TChain*  mChain;
      TString* mHistoFile;

      // L3 trigger cut parameters
      Float_t mCutFraction;
      Int_t   mMinHits;
      Float_t mMaxDCA;
      Float_t mMinP;
      Float_t mMaxDedx;
      // Offline cut parameters
      Int_t   mMinHitsOffline;
      Float_t mDEdxnHitsRatio;


      // histograms
      TH1F* hOffP;
      TH1F* hL3P;

      TH1F* hL3Dca;

      TH2F* hOffdEdx;
      TH2F* hL3dEdx;

      TH1F* hOffZ;
      TH1F* hL3Z;

      TH1F* hCandidateMatch;
      TH2F* hOffCandidatedEdx;
      TH1F* hOffCandidateZ;
      TH2F* hL3CandidatedEdx;
      TH2F* hL3CandidatedEdxMatch;
      TH2F* hL3CandidatedEdxTriggeredMatch;
      TH1F* hL3CandidateDca;
      TH1F* hL3CandidateZ;

      ClassDef(StReadRare, 1)
};

