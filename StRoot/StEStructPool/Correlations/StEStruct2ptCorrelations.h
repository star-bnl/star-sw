/**********************************************************************
 *
 * $Id: StEStruct2ptCorrelations.h,v 1.6 2005/09/14 17:14:20 msd Exp $
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
#ifndef __STESTRUCT2PTCORRELATIONS__H
#define __STESTRUCT2PTCORRELATIONS__H


#include "TROOT.h"
#include "StEStructPool/AnalysisMaker/StEStructAnalysis.h"
#include "StEStructPool/EventMaker/StEStructCentrality.h"
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

  // Event-level hists
  TH1F*  mHNEvents[2];
  TH1F** mHpt;
  TH2F*  mHmix;

  // HBT parameters
  qBins *mQinv[6]; //!  1D
  TH1F ** mHQinv[6];//!  1D hist

  //-> X vs X 
  ytBins **mYtYt[6]; //!
  xtBins **mXtXt[6]; //! Xt, legacy quantity
  ptBins **mPtPt[6]; //!
  etaBins **mEtaEta[6]; //!
  phiBins **mPhiPhi[6]; //!

  etaBins **mPrEtaEta[6]; //! weight = pt1*pt2
  phiBins **mPrPhiPhi[6]; //!  "

  etaBins **mSuEtaEta[6]; //! weight = pt1+pt2
  phiBins **mSuPhiPhi[6]; //!  "


  TH2F ** mHYtYt[6]; //!
  TH2F ** mHXtXt[6]; //!
  TH2F ** mHPtPt[6]; //!
  TH2F ** mHEtaEta[6]; //!
  TH2F ** mHPhiPhi[6]; //!

  TH2F ** mHPrEtaEta[6]; //!
  TH2F ** mHPrPhiPhi[6]; //!
  TH2F ** mHSuEtaEta[6]; //!
  TH2F ** mHSuPhiPhi[6]; //!

  // Delta Y vs Delta X
  dphiBins **mJtDYtDPhi[6]; //!
  detaBins **mJtDYtDEta[6]; //!
  dphiBins **mJtDEtaDPhi[6]; //!
  dphiBins **mPrJtDEtaDPhi[6]; //!
  dphiBins **mSuJtDEtaDPhi[6]; //!

  TH2F ** mHJtDYtDPhi[6];
  TH2F ** mHJtDYtDEta[6];
  TH2F ** mHJtDEtaDPhi[6];
  TH2F ** mHPrJtDEtaDPhi[6];
  TH2F ** mHSuJtDEtaDPhi[6];

  // Sum Y vs Delta X
  dytBins  **mAtSYtDYt[6];     //! smt array of dmt bins
  dptBins  **mAtSPtDPt[6];     //! smt array of dmt bins
  dphiBins **mJtSEtaDPhi[6];//! 
  dphiBins **mPrJtSEtaDPhi[6];//! 
  dphiBins **mSuJtSEtaDPhi[6];//! 

  TH2F ** mHAtSYtDYt[6];
  TH2F ** mHAtSPtDPt[6];
  TH2F ** mHJtSEtaDPhi[6];//!
  TH2F ** mHPrJtSEtaDPhi[6];//!
  TH2F ** mHSuJtSEtaDPhi[6];//!

  // TPC Separation
  TPCSepBins *mTPCAvgTSep[6];  //1D
  TPCSepBins *mTPCAvgZSep[6];  
  TPCSepBins *mTPCEntTSep[6];  
  TPCSepBins *mTPCEntZSep[6];  
  TPCSepBins *mTPCMidTSep[6];  
  TPCSepBins *mTPCMidZSep[6];  
  TPCSepBins *mTPCExitTSep[6];  
  TPCSepBins *mTPCExitZSep[6];  

  TPCSepBins *mTPCMidTdptP[6]; //! needed to differentiate by sign of deltaPt   
  TPCSepBins *mTPCMidTdptN[6]; //! to evaluate pair crossing cut   
  TPCSepBins *mTPCMidZdptP[6];  
  TPCSepBins *mTPCMidZdptN[6];  

  TH1F **  mHTPCAvgTSep[6]; 
  TH1F **  mHTPCAvgZSep[6];  
  TH1F **  mHTPCEntTSep[6]; 
  TH1F **  mHTPCEntZSep[6];  
  TH1F **  mHTPCMidTSep[6]; 
  TH1F **  mHTPCMidZSep[6];  
  TH1F **  mHTPCExitTSep[6]; 
  TH1F **  mHTPCExitZSep[6];  

  TH1F **  mHTPCMidTdptP[6];  
  TH1F **  mHTPCMidTdptN[6];  
  TH1F **  mHTPCMidZdptP[6];  
  TH1F **  mHTPCMidZdptN[6];

  TPCSepBins **mTPCAvgTZ[6];  //2D
  TPCSepBins **mTPCEntTZ[6];  
  TPCSepBins **mTPCMidTZ[6];  
  TPCSepBins **mTPCExitTZ[6];  
  dptBins **mTPCEntTdpt[6];  // T vs delta-Pt; for joint hists, use bin type of y axis
  dptBins **mTPCMidTdpt[6];
  dptBins **mTPCExitTdpt[6];   

  TH2F **  mHTPCAvgTZ[6];  
  TH2F **  mHTPCEntTZ[6];  
  TH2F **  mHTPCMidTZ[6];  
  TH2F **  mHTPCExitTZ[6];  
  TH2F **  mHTPCEntTdpt[6];
  TH2F **  mHTPCMidTdpt[6];
  TH2F **  mHTPCExitTdpt[6];


  // generic histogram create functions to simplify the code
  //

  char* bName[6];
  char* bTitle[6];

  void createHist2D(TH2F*** h, const char* name, int iknd, int icut,int numCuts, int nx, float xmin, float xmax, int ny, float ymin, float ymax);
  void createHist1D(TH1F*** h, const char* name, int iknd, int icut,int numCuts, int nx, float xmin, float xmax);
  void  moveEvents();


 public:

  int  manalysisMode; //! simple enumeration of analyses ...
  bool mskipPairCuts; //!
  bool mdoPairCutHistograms; //!
  bool mdoPairDensityHistograms; 
  bool mInit;  //! found need when overridding this class
  bool mDeleted;//! "     " ...

  StEStructEvent*        mCurrentEvent;  //!  pointer to EStruct2pt data 
  StEStructPairCuts      mPair; //! for pairs (1 at a time) and all pair cuts

  char*     moutFileName; //!
  StTimer*  mtimer;       //!

  StEStructEvent*     mMixingEvent;  //! dummy      //  Previous Event Stored 

  // *** had a problem using constants here (dyn. libs wouldn't load), doing this for now...
  //int kNumBuffers = 20;  // number of z-vertex bins 
  //const static int kBuffRange = 50;   // max |Vz| 
  //const static int kBuffWidth = 2*kBuffRange / kNumBuffers;
  int kNumBuffers;
  int kBuffRange;
  int kBuffWidth;
  //StEStructBuffer      mbuffer[kNumBuffers];  // kNumBuffers slices in z-vertex from -kBuffRange to +kBuffRange
  //int             mbuffCounter[kNumBuffers];
  StEStructBuffer      mbuffer[30];  // kNumBuffers slices in z-vertex from -kBuffRange to +kBuffRange
  int             mbuffCounter[30];

  //-> (pre) histograms & histograms for analysis.
  // All are arrays of 6 for 6 charged sign and combinatoric types;
  // 0,1,2 for Sibling (++,+-,--)
  // 3,4,5, for Mixed  (++,+-,--)
  
  int numPairs[6];
  int numPairsProcessed[6];
  int mpossiblePairs[6];

  StEStruct2ptCorrelations(int mode=0);
  StEStruct2ptCorrelations(const char* cutFileName, int mode=0);
  virtual ~StEStruct2ptCorrelations();

  StEStructPairCuts& getPairCuts();
  void  setAnalysisMode(int mode);
  void  setCutFile(const char* cutFileName, StEStructCentrality *cent);  

  //---> support of interface  
  void  setOutputFileName(const char* outFileName);
  bool  doEvent(StEStructEvent* p);

  void  init();
  void  cleanUp();
  void  finish();

  virtual void  fillHistograms();
  virtual void  writeHistograms();
    void  initArrays();
    void  deleteArrays();
    void  initHistograms();
    void  deleteHistograms();


  // analysis specific functions 
  bool  makeSiblingAndMixedPairs();
  virtual void  makePairs(StEStructEvent* e1, StEStructEvent* e2, int j);

  int   getNumPairs(int i){ return numPairs[i]; };
  int   getNumPairsProcessed(int i){ return numPairsProcessed[i]; };
  int   getPossiblePairs(int i){ return mpossiblePairs[i]; };

  void logStats(ostream& os);

    ClassDef(StEStruct2ptCorrelations,1)
};   

inline void StEStruct2ptCorrelations::setAnalysisMode(int mode){ manalysisMode=mode;};

inline void StEStruct2ptCorrelations::setCutFile(const char* cutFileName,
                                                 StEStructCentrality *cent){
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

inline void StEStruct2ptCorrelations::logStats(ostream& os){
  char* htp[]={"SibPP","SibPM","SibMM","MixPP","MixPM","MixMM"};
  for(int i=0;i<6;i++){
    os<<"<pairType>"<<htp[i]<<" "<<endl;;
   os<<"   <processStat \"possiblePairs\">"<<getPossiblePairs(i);
   os<<"</processStat> "<<endl;;
   os<<"   <processStat \"inputPairs\">"<<getNumPairs(i);
   os<<"</processStat> "<<endl;;
   os<<"   <processStat \"outputPairs\">"<<getNumPairsProcessed(i);
   os<<"</processStat> "<<endl;
   os<<"</pairType>"<<endl;
  }
};



#endif


/***********************************************************************
 *
 * $Log: StEStruct2ptCorrelations.h,v $
 * Revision 1.6  2005/09/14 17:14:20  msd
 * Large update, added new pair-cut system, added pair density plots for new analysis mode (4), added event mixing cuts (rewrote buffer for this)
 *
 * Revision 1.5  2005/09/07 20:21:15  prindle
 *
 *   2ptCorrelations: Rearranged array/histogram initialization/destruction.
 *                    Now histograms are only allocated at end of job,
 *                    just before they are filled then written.
 *
 * Revision 1.4  2005/03/03 01:30:43  porter
 * updated StEStruct2ptCorrelations to include pt-correlations and removed
 * old version of pt-correlations from chunhuih (StEStruct2ptPtNbar)
 *
 * Revision 1.3  2004/09/16 23:37:25  chunhuih
 *
 * changed a number of methods to be virtual, so that its behavior can
 * be dynamically changed.
 *
 * Revision 1.2  2004/06/25 03:11:49  porter
 * New cut-binning implementation and modified pair-cuts for chunhui to review
 *
 * Revision 1.1  2003/10/15 18:20:46  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/





