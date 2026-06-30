/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  To generate a ROOT file of histograms related to the FCS and event data in the produced MuDsts from RHIC Run 22 that can be used to do quality assurance (QA) on the data.

  DESCRIPTION
  This class inherits from StMaker and contains many histograms to be used for Quality Assurance (QA) of MuDst files that contain Forward Calorimeter System (FCS) data. It uses #LoadHists() together with the functions #AddH1F(), #AddH2F(), #AddH1FArr(), #AddH2FArr() to ease histogram creation and management. These functions should be used exclusively in this and inherited classes to automate histogram management. #FillEventInfo() is used (and can be re-implemented) to fill QA histograms related to event information. #FillFcsInfo() is used (and can be re-implemented) to fill QA histograms related to FCS information. The idea is that inherited classes don't need to implement the #Init() function only the #LoadHists() function. Also can be used to do some EPD Qa with the #FillEpdInfo() function.

  LOG
  @[August 14, 2024] > Copied from StMuFcsRun22QaMaker.h into its own class
  @[August 29, 2024] > Added some histograms to check the correlation between various vertex methods. Added a variable #mEpdVertex to store the found vertex so it can be retrieved outside the maker if needed. Added a draw function for the vertex histograms. Added multiplicity histograms with cuts on nMIP. Some cleanup.
  @[August 30, 2024] > Changed default vertex for all detectors to -999. Modified some drawing options. Small fixes
  @[September 9, 2024] > Clean up extraneous code and added #getFileName() for mHists
  @[September 18, 2024] > Made the variables related to computing the vertex members of the class and added get functions for them
  @[January 8, 2025] > Fixed comments to be ROOT friendly. Fixed how histograms are loaded so it can work with multiple files. Added a graph for EPD vertex QA over many runs and methods for processing, filling, and plotting the EPD vertex over time.
  @[February 1, 2025] > Small fix for including StEnumerations.h
  @[March 21, 2025] > Added a new plotting function #DrawVertexNoZdc() since ZDC was not used in this analysis.
  @[September 10, 2025] > Implemented an adjacency map for EPD tiles

  @[January 22, 2026] > Copied from #StMuEpdRun22QaMaker and modified for this analysis
  
  Do DEP calib of EPD chs, bunch xing analysis for spin. Change some plots so they use logz and move/remove the stats box for some of hte 2d histograms when plotting. Show on the fly EPD MIP peak locations and valleys
 */

#ifndef STMUFCSANAEPDQAANDVERT_HH
#define STMUFCSANAEPDQAANDVERT_HH

//C/C++ Headers
// #include <iostream>

//ROOT Headers
// #include "TRandom3.h"
// #include "TCanvas.h"
// #include "TObjArray.h"
// #include "TString.h"
// #include "TFile.h"
// #include "TTree.h"
// #include "TH1F.h"
// #include "TH2F.h"

//STAR Headers
// #include "StEnumerations.h"
// #include "StMaker.h"
// #include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"
// #include "StMuDSTMaker/COMMON/StMuDstMaker.h"
// #include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
// #include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
// #include "StEvent/StTriggerData.h"
// #include "StEvent/StTriggerId.h"
// #include "StMessMgr.h"
// #include "StMuDSTMaker/COMMON/StMuEvent.h"
// #include "StMuDSTMaker/COMMON/StMuTypes.hh"
// #include "Stypes.h"
// #include "StFcsDbMaker/StFcsDbMaker.h"
// #include "StFcsDbMaker/StFcsDb.h"
// #include "StMuDSTMaker/COMMON/StMuFcsCollection.h"
// #include "StMuDSTMaker/COMMON/StMuFcsHit.h"
// #include "StMuDSTMaker/COMMON/StMuFcsCluster.h"
// #include "StMuDSTMaker/COMMON/StMuFcsPoint.h"
// #include "StEpdDbMaker/StEpdDbMaker.h"
// #include "StEpdHitMaker/StEpdHitMaker.h"

//Custom headers in this folder
#include "StMuFcsVirtualAna.h"
//#include "StMuFcsRun22QaMaker.h"   //For MakeGraph


class StMuFcsAnaEpdQaAndVert : public StMuFcsVirtualAna
{
 public:
  StMuFcsAnaEpdQaAndVert();
  ~StMuFcsAnaEpdQaAndVert();

  virtual UInt_t LoadHists(TFile* file, HistManager* histman, StMuFcsAnaData* anadata);
  virtual Int_t DoMake(StMuFcsAnaData* mufcsdata);
  
  void setEpdTacAdcOn(bool value=true) { mEpdTacAdcOn = value; }

  Int_t epdTacEarlyE(){ return mCutEarliestTacE; }
  Int_t epdTacEarlyW(){ return mCutEarliestTacW; }
  Double_t epdTacAvgE()  { return mCutAvgTacE; }
  Double_t epdTacAvgW()  { return mCutAvgTacW; }
  Double_t epdVertex()   { return mEpdVertex; }

  
  void DrawEpdAllQa(TCanvas* canv, const char* savename);      //!< Call all the draw functions in same order as below, should be some kind of pdf as it will encompass many pages
  //void DrawVertex(TCanvas* canv, const char* savename);        //!< Draw 2D vertex correlation histograms, single page
  void DrawEpdHitQa(TCanvas* canv, const char* savename);      //!< Draw Multiplicty and nMIP histograms, single page
  void DrawEpdTacQa(TCanvas* canv, const char* savename);      //!< Draw the histograms related to the TAC difference, single page
  void DrawEpdTacCutQa(TCanvas* canv, const char* savename);   //!< Draw the histograms related to the TAC difference with cuts, single page
  void DrawEpdTacAdcQa(TCanvas* canv, const char* savename);   //!< Draw the TAC vs. ADC histograms for all channels, savename should be some kind of pdf as it will encompass many pages
  //void DrawVertexNoZdc(TCanvas* canv, const char* savename);   //!< Draw 2D vertex correlation histograms, single page no ZDC
  /*
  Int_t LoadGraphsFromFile(TFile* file, TObjArray* graphs );
  void FillGraphs(Int_t irun);

  void DrawGraphVertex(TCanvas* canv, const char* savename="testGraphEpdVert.png");
  */
  
protected:
  StMuDstMaker* mMuDstMkr = 0;
  StMuDst* mMuDst = 0;
  StMuEvent* mMuEvent = 0;
  const StTriggerData* mTrigData = 0;
  StRunInfo* mRunInfo = 0;
  
  //StEpdGeom* mEpdGeo=0;
  TClonesArray* mMuEpdHits = 0;
  StEpdHitMaker* mEpdHitMkr = 0;
  StEpdCollection* mEpdColl = 0;

  TH1* mH1F_Epd_NHits = 0;              ///< Number of hits from EPD collection
  TH1* mH1F_Epd_NHits_Cut = 0;          ///< Number of hits in EPD with nMIP>0.7
  TH1* mH1F_Epd_NHitsWest = 0;          ///< Number of hits from EPD collection only west side
  TH1* mH1F_Epd_NHitsWest_Cut = 0;      ///< Number of hits from EPD collection only west side with nMIP>0.7

  TH1* mH2F_HitEpd_nmipVchkey[2];        ///< Special for checking nmip of a given channel in the EPD. The "chkey" is (supersector-1)*31+(tileid-1)
  TObjArray* mH2F_HitEpd_tacVadcmip[2];  ///< Special for checking EPD TAC vs. ADC/ADC_1mip histograms which may help with slew corrections in the EPD

  TH1* mH2F_Epd_earlywVearlye = 0;      ///< EPD hit with Earliest West TAC vs. Earliest East TAC (Since using common stop early means largest TAC value)
  TH1* mH2F_Epd_avgwVavge = 0;          ///< EPD Averaged West TAC vs. Averaged East TAC
  TH1* mH2F_EpdTacDiff_avgVearly = 0;       //EPD tac difference between West and East tiles (in that order) computed from average tac vs. differences computed using earliest TAC
  TH1* mH2F_EpdCut_earlywVearlye = 0;   ///< EPD hit with Earliest West TAC vs. Earliest East TAC with channel 1<nMIP<15 (Since using common stop early means largest TAC value)
  TH1* mH2F_EpdCut_avgwVavge = 0;       ///< EPD Averaged West TAC vs. Averaged East TAC with channel 1<nMIP<15
  TH1* mH2F_EpdCutTacDiff_avgVearly = 0;  ///< EPD tac difference between West and East tiles (in that order)  computed from average tac vs. differences computed using earliest TAC; computed with cut 1<adcnmip<15 && TAC>50

  bool mEpdTacAdcOn = true;            ///< For turning on/off TAC vs. ADC histograms for all EPD channels

  TGraphErrors* mGE_VertexEpd = 0;     ///< Graph for Mean EPD vertex and Err as RMS vs. Run Index

private:
  Double_t mEpdVertex = -999;         ///< Saved vertex from event
  Int_t mCutEarliestTacE  = 0;        ///< Stores the largest found TAC value in EPD East with cuts
  Int_t mCutEarliestTacW  = 0;        ///< Stores the largest found TAC value in EPD West with cuts
  Double_t mCutAvgTacE = 0;           ///< Stores the average TAC value in EPD East with cuts
  Double_t mCutAvgTacW = 0;           ///< Stores the average TAC value in EPD West with cuts

  double mEpdScale = 15.6;             ///< picoSecond/TAC for EPD

  ClassDef(StMuFcsAnaEpdQaAndVert,1)

};

#endif
