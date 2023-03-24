/**
 * \class StCentralityAnalyzer
 * \brief Process and build distributions for centrality determination
 *
 * The StPiStCentralityAnalyzer processes
 * Most of the members are copied from StMuEvent.
 * \author Grigory Nigmatkulov
 */

#ifndef StCentralityAnalyzer_h
#define StCentralityAnalyzer_h

// STAR classes
#include "StMaker.h"

// PicoDst headers
#include "StPicoEvent/StPicoDstReader.h"
#include "StPicoEvent/StPicoEvent.h"
#include "StPicoEvent/StPicoTrack.h"
#include "StPicoEvent/StPicoBTofPidTraits.h"
#include "StPicoEvent/StPicoDst.h"

// ROOT headers
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TRandom3.h"

// C++ headers
#include <vector>

//________________
class StCentralityAnalyzer : public StMaker {
 public:
  // Constructor
  StCentralityAnalyzer(StPicoDstReader *reader, const Char_t* oFileName);
  // Destructor
  virtual ~StCentralityAnalyzer();

  // Standard StMaker inherited methods
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  void Clear(Option_t *option="");

  // Set cuts, limits and triggers
  void addTriggerId(const unsigned int& id);
  void setDebugStatus(bool status)                                   { mDebug = status; }
  void setUsePileUp(bool pileup)                                     { mUsePileUp = pileup; }
  void setUseLumCorr(bool lumcorr)                                   { mUseLumCorr = lumcorr; }
  void setUseVzCorr(bool vzcorr)                                     { mUseVzCorr = vzcorr; }
  void setRunIdParameters(unsigned int& bins, double& lo, double hi) { mRunIdBins=bins; mRunIdRange[0]=lo; mRunIdRange[1]=hi; }
  void setVtxZCut(const float& lo, const float& hi)                  { mVtxZ[0]=lo; mVtxZ[1]=hi; }
  void setVtxRCut(const float& lo, const float& hi)                  { mVtxR[0]=lo; mVtxR[1]=hi; }
  void setVtxPositionShift(const float& x, const float& y)           { mVtxXShift=x; mVtxYShift=y; }
  void setVtxVpdVzCut(const float& lo, const float& hi)              { mVpdVzDiff[0]=lo; mVpdVzDiff[1]=hi; }

  void setPileUpParameters(double a0,double a1,double a2,double a3,double a4,
			   double b0,double b1,double b2,double b3,double b4,
			   double c0,double c1,double c2,double c3,double c4)
  { m_a0=a0; m_a1=a1; m_a2=a2; m_a3=a3; m_a4=a4;
    m_b0=b0; m_b1=b1; m_b2=b2; m_b3=b3; m_b4=b4;
    m_c0=c0; m_c1=c1; m_c2=c2; m_c3=c3; m_c4=c4;}

  void setLumCorrParameters(double lumcorr_a, double lumcorr_b, double lumcorr_bprime) 
	{ m_LumCorr_a = lumcorr_a; m_LumCorr_b = lumcorr_b; m_LumCorr_bprime = lumcorr_bprime; }

  void setVzCorrParameters(double vzCorPar0, double vzCorPar1, double vzCorPar2, double vzCorPar3, double vzCorPar4, double vzCorPar5, double vzCorPar6)
  { 
    m_vzCorr0 = vzCorPar0;
    m_vzCorr1 = vzCorPar1;
    m_vzCorr2 = vzCorPar2;
    m_vzCorr3 = vzCorPar3;
    m_vzCorr4 = vzCorPar4;
    m_vzCorr5 = vzCorPar5;
    m_vzCorr6 = vzCorPar6;
  }
  
  //set shape correction index, 0: Ru, 1: Zr
  void setShapeIndex(Int_t index) {mShapeIndex = index;}

  Double_t getShapeWeight(Double_t Vz, Double_t RefMult); 

 private:
  // Histogram creation
  void createHistograms();
  void createEventHistograms();
  void createPrimaryTrackHistograms();
  void createGlobalTrackHistograms();

  // Methods to check whether
  Bool_t isGoodEvent(StPicoEvent* event);
  Bool_t isGoodVertex(Float_t x, Float_t y, Float_t z, Float_t vpdVz);
  Bool_t isGoodTrigger(std::vector<unsigned int> ids);
  Bool_t isNotPileUp(UShort_t refMult, UShort_t btofMatched);
  Bool_t isGoodTrack(StPicoTrack* track);
  Bool_t isInBadRunList(unsigned int runId);

  // Method for luminosity correction
  Double_t calculateLumCorr(Double_t ZDCx);

  //Function to correct refmult Vz dependence
  Double_t getRefMultCorrVz(Double_t RefMult, Double_t Vz);

  const Char_t *mOutFileName;
  TFile *mOutFile;
  StPicoDstReader *mReader;
  StPicoDst* mDst;
  std::vector<unsigned int> mTriggerIdCollection;

  UInt_t mEventsPassed;

  Bool_t mDebug;
  Bool_t mUsePileUp;
  Bool_t mUseLumCorr;
  Bool_t mUseVzCorr;

  // Pileup Parameters
  Double_t m_a0, m_a1, m_a2, m_a3, m_a4;
  Double_t m_b0, m_b1, m_b2, m_b3, m_b4;
  Double_t m_c0, m_c1, m_c2, m_c3, m_c4;

  // Luminosity Correction Parameters
  Double_t m_LumCorr_a;
  Double_t m_LumCorr_b;
  Double_t m_LumCorr_bprime;

  //a backup tree to confirm vz correciton 
  TTree *mTree;
  Double_t mTree_Vz;
  Double_t mTree_ZDCx;
  Double_t mTree_refMult;
  Double_t mTree_refMultCor;
  Double_t mTree_nBTOFMatched;


  // Vz Correction Parameters
  Double_t m_vzCorr0, m_vzCorr1, m_vzCorr2, m_vzCorr3, m_vzCorr4, m_vzCorr5, m_vzCorr6;
  //Shape correction index, 0: Ru, 1: Zr
  Int_t mShapeIndex;

  // Bad run list for Ru+Ru at 200 GeV
  static const std::vector<unsigned int> bad_run_list_ruru_200gev;
  // Bad run list for Zr+Zr at 200 GeV
  static const std::vector<unsigned int> bad_run_list_zrzr_200gev;

  // Event cuts
  Float_t mVtxZ[2];
  Float_t mVtxR[2];
  Float_t mVtxXShift, mVtxYShift;
  Float_t mVpdVzDiff[2];
  Float_t mRefMult[2];
  UInt_t mRunIdBins;
  Double_t mRunIdRange[2];

  // Track cuts
  Float_t mMom[2];
  Float_t mEta[2];
  Float_t mNHits[2];
  Float_t mNHitsRatio[2];

  //////////////////////
  //     Histograms   //
  //////////////////////
  TH1F* hRefMult;
  TH1F* hRefMultVtxZ[73];
  TH1F* hGRefMult;
  TH1F* hPrimVertNum;
  TH1F* hPrimVertZ;
  TH1F* hZdcAdcEast;
  TH1F* hZdcAdcWest;
  TH1F* hZdcAdcSum;
  TH1F* hZdcCoincidenceRate;
  TH1F* hEpdAdcEast;
  TH1F* hEpdAdcWest;
  TH1F* hEpdAdcSum;
  TH2F* hPrimVertXvsY;
  TH1F* hPrimVertVpdVzDiff;
  TH2F* hRefMultVsTofTrayMult;
  TH2F* hRefMultVsTofMatched;
  TH2F* hGRefMultVsTofTrayMult;
  TH2F* hGRefMultVsTofMatched;
  TH2F* hRefMultVsTofMatch;
  TH2F* hRefMultVsBemcMatch;
  TH2F* hRefMultVsZdcCoincidenceRate;
  TProfile* hRefMultVsRunNumber;
  TProfile* hGRefMultVsRunNumber;
  TProfile* hTofTrayMultVsRunNumber;
  TProfile* hTofMatchedVsRunNumber;
  TProfile* hZdcAdcSumVsRunNumber;
  TProfile* hZdcCoincidenceRateVsRunNumber;
  TProfile* hPrimTrackPtVsRunNumber;
  TProfile* hPrimTrackNHitsVsRunNumber;
  TProfile* hPrimTrackDedxVsRunNumber;
  TProfile* hGlobTrackPtVsRunNumber;
  TProfile* hGlobTrackNHitsVsRunNumber;
  TProfile* hGlobTrackDedxVsRunNumber;
  TProfile* hAvgRefMultVsZdcCoincidenceRate;
  TProfile* hAvgRefMultVsZdcCoincidenceRateForTrig[4];

  ClassDef(StCentralityAnalyzer,0)
};

#endif // StCentralityAnalyzer_h
