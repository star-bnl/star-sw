/**********************************************************************
 *
 * $Id: StEStruct2ptCorrelations.h,v 1.1 2003/10/15 18:20:46 porter Exp $
 *
 * Author: Jeff Porter adaptation of Aya's 2pt-analysis
 *
 **********************************************************************
 *
 * Description:  Analysis code for 2pt-analysis. 
 *    The analysis runs as follows:
 *       1D and 2D arrays (in yt,eta,phi) are setup
 *       and filled for each of the 6 pair types:
 *       Sibling (++,+- & -+, --)
 *       Mixed   (++,+- & -+, --)
 *       The 2D versions are additionally divided into yt1,yt2 slices
 *       and (via the StEStructBuffer) z-vertex
 *       After arrays are filled (looped over all events/job), Histograms are 
 *       created, filled, and written out to the data file for further
 *       processing.
 *
 *
 ***********************************************************************/
#ifndef __STEBYE2PTCORRELATIONS__H
#define __STEBYE2PTCORRELATIONS__H


#include "TROOT.h"
#include "StEStructPool/AnalysisMaker/StEStructAnalysis.h"
#include "StEStructPairCuts.h"
#include "StEStructBinning.h"
#include "StEStructBuffer.h"

class TFile;
class TH1F;
class TH2F;
class StEStructEvent;
class StTimer;


class StEStruct2ptCorrelations: public StEStructAnalysis {

 protected:

  int  manalysisMode; //! simple enumeration of analyses ...
  bool mskipPairCuts; //!
  bool mdoPairCutHistograms; //!

  StEStructEvent*        mCurrentEvent;  //!  pointer to EStruct2pt data 
  StEStructPairCuts      mPair; //! for pairs (1 at a time) and all pair cuts

  char*     moutFileName; //!
  StTimer*  mtimer;       //!

  StEStructEvent*     mMixingEvent;  //! dummy      //  Previous Event Stored 
  StEStructBuffer      mbuffer[10];  // 10 slices in z-vertex
  int             mbuffCounter[10];

  //-> (pre) histograms & histograms for analysis.
  // All are arrays of 6 for 6 charged sign and combinatoric types;
  // 0,1,2 for Sibling (++,+-,--)
  // 3,4,5, for Mixed  (++,+-,--)

  int numPairs[6];
  int numPairsProcessed[6];
  int mpossiblePairs[6];
  deltaMtBins mDeltaMt[6]; //!
  TH1F * mHDeltaMt[6];//!
 
  // -> 1D hists....
  dmtBins mdmts[6];
  smtBins msmts[6];
  detaBins mdetas[6];
  setaBins msetas[6];
  dphiBins mdphis[6];
  sphiBins msphis[6];

  TH1F* mHdmts[6];
  TH1F* mHsmts[6];
  TH1F* mHdetas[6];
  TH1F* mHsetas[6];
  TH1F* mHdphis[6];
  TH1F* mHsphis[6];

  //-> X vs X 
  mtBins **mMtMt[6]; //!
  etaBins **mEtaEta[6]; //!
  phiBins **mPhiPhi[6]; //!

  TH2F ** mHMtMt[6]; //!
  TH2F ** mHEtaEta[6]; //!
  TH2F ** mHPhiPhi[6]; //!

  // Delta Y vs Delta X
  dphiBins **mJtDMtDPhi[6]; //!
  dmtBins  **mJtDEtaDMt[6]; //!
  dphiBins **mJtDEtaDPhi[6]; //!

  TH2F ** mHJtDMtDPhi[6];
  TH2F ** mHJtDEtaDMt[6];
  TH2F ** mHJtDEtaDPhi[6];

  // Sum Y vs Delta X
  dmtBins  **mAtSMtDMt[6];     //! smt array of dmt bins
  detaBins **mAtSEtaDEta[6];  //! seta array of deta bins
  dphiBins **mAtSPhiDPhi[6];  //! sphi array of dphi bins

  TH2F ** mHAtSMtDMt[6];
  TH2F ** mHAtSEtaDEta[6];
  TH2F ** mHAtSPhiDPhi[6];

  // Sum Y vs Sum X
  sphiBins **mJtSMtSPhi[6]; //!
  smtBins  **mJtSEtaSMt[6]; //!
  sphiBins **mJtSEtaSPhi[6];//! 

  TH2F ** mHJtSMtSPhi[6];//!
  TH2F ** mHJtSEtaSMt[6];//!
  TH2F ** mHJtSEtaSPhi[6];//!

  void  initArraysAndHistograms();
  void  deleteArraysAndHistograms();
  void  moveEvents();

 public:

  StEStruct2ptCorrelations(int mode=0);
  StEStruct2ptCorrelations(const char* cutFileName, int mode=0);
  virtual ~StEStruct2ptCorrelations();

  StEStructPairCuts& getPairCuts();
  void  setAnalysisMode(int mode);
  void  setCutFile(const char* cutFileName);  

  //---> support of interface  
  virtual void  setOutputFileName(const char* outFileName);
  bool  doEvent(StEStructEvent* p);
  void  init();
  void  cleanUp();
  void  finish();
  void  fillHistograms();
  void  writeHistograms(TFile* tf);

  // analysis specific functions 
  bool  makeSiblingAndMixedPairs();
  void  makePairs(StEStructEvent* e1, StEStructEvent* e2, int j);

  int   getNumPairs(int i){ return numPairs[i]; };
  int   getNumPairsProcessed(int i){ return numPairsProcessed[i]; };
  int   getPossiblePairs(int i){ return mpossiblePairs[i]; };

    ClassDef(StEStruct2ptCorrelations,1)
};   

inline void StEStruct2ptCorrelations::setAnalysisMode(int mode){ manalysisMode=mode;};

inline void StEStruct2ptCorrelations::setCutFile(const char* cutFileName){
  mPair.setCutFile(cutFileName);
  mPair.loadCuts();
}

inline void StEStruct2ptCorrelations::setOutputFileName(const char* fName){
  if(!fName) return;
  moutFileName=new char[strlen(fName)+1];
  strcpy(moutFileName,fName);
}

inline StEStructPairCuts& StEStruct2ptCorrelations::getPairCuts() {
  return mPair;
}

#endif


/***********************************************************************
 *
 * $Log: StEStruct2ptCorrelations.h,v $
 * Revision 1.1  2003/10/15 18:20:46  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/





