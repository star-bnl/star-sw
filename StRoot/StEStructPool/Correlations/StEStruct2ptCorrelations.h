 /**********************************************************************
 *
 * $Id: StEStruct2ptCorrelations.h,v 1.8 2006/04/04 22:10:09 porter Exp $
 *
 * Author: Jeff Porter adaptation of Aya's 2pt-analysis
 *
 **********************************************************************
 *
 * Description:  Analysis code for 2pt-analysis. 
 *    The analysis runs as follows:
 *       1D and 2D arrays (in yt,eta,phi) are setup
 *       and filled for each of the 8 pair types:
 *       Sibling (++, +-, -+, --)
 *       Mixed   (++, +-, -+, --)
 *       Particle id is done via dEdx and introduced into the analysis via cut-bins.
 *       Order particles so pi always come before K before p and plus comes before
 *       minus. 2D histograms will not be guaranteed to be symmetric.
 *       The 2D versions are additionally divided into z-vertex (via the StEStructBuffer)
 *       After arrays are filled (looped over all events/job), Histograms are 
 *       created, filled, and written out to the data file for further
 *       processing.
 *
 *       Note that if we find charge symmetry we can form LS, US, CD and CI
 *       combinations using these histograms.
 *
 *
 ***********************************************************************/
#ifndef __STESTRUCT2PTCORRELATIONS__H
#define __STESTRUCT2PTCORRELATIONS__H


#include "TROOT.h"
#include "StEStructPool/AnalysisMaker/StEStructAnalysis.h"
#include "StEStructPool/AnalysisMaker/StEStructQAHists.h"
#include "StEStructPool/EventMaker/StEStructCentrality.h"
#include "StEStructPairCuts.h"
#include "StEStructBinning.h"
#include "StEStructBuffer.h"

class TFile;
class TH1F;
class TH2F;
class StEStructEvent;
class StTimer;
#define _MAX_ZVBINS_ 30

class StEStruct2ptCorrelations: public StEStructAnalysis {

 protected:

  // Event-level hists
  TH1F*  mHNEvents[2];
  TH1F** mHpt;
  TH2F*  mHmix;

  // HBT parameters
  qBins *mQinv[8]; //!  1D
  TH1F ** mHQinv[8];//!  1D hist

  // QAEtaBins mQAEta[2][4];
  // QAPhiBins mQAPhi[2][4];
  // QAPtBins  mQAPt[2][4];
  // PtotBins  *mdEdxPtot[2][4];

  //-> X vs X 
  ytBins **mYtYt[8]; //!
  xtBins **mXtXt[8]; //! Xt, legacy quantity
  ptBins **mPtPt[8]; //!
  etaBins **mEtaEta[8]; //!
  phiBins **mPhiPhi[8]; //!

  etaBins **mPrEtaEta[8]; //! weight = pt1*pt2
  phiBins **mPrPhiPhi[8]; //!  "

  etaBins **mSuEtaEta[8]; //! weight = pt1+pt2
  phiBins **mSuPhiPhi[8]; //!  "


  //  TH1F  * mHQAEta[2][4]; //!
  //  TH1F  * mHQAPhi[2][4]; //!
  //  TH1F  * mHQAPt[2][4]; //!
  //  TH2F  * mHdEdxPtot[2][4]; //!
  TH2F ** mHYtYt[8]; //!
  TH2F ** mHXtXt[8]; //!
  TH2F ** mHPtPt[8]; //!
  TH2F ** mHEtaEta[8]; //!
  TH2F ** mHPhiPhi[8]; //!

  TH2F ** mHPrEtaEta[8]; //!
  TH2F ** mHPrPhiPhi[8]; //!
  TH2F ** mHSuEtaEta[8]; //!
  TH2F ** mHSuPhiPhi[8]; //!

  // Delta Y vs Delta X
  dphiBins **mJtDYtDPhi[8]; //!
  detaBins **mJtDYtDEta[8]; //!
  dphiBins **mJtDEtaDPhi[8]; //!
  dphiBins **mPrJtDEtaDPhi[8]; //!
  dphiBins **mSuJtDEtaDPhi[8]; //!

  TH2F ** mHJtDYtDPhi[8];
  TH2F ** mHJtDYtDEta[8];
  TH2F ** mHJtDEtaDPhi[8];
  TH2F ** mHPrJtDEtaDPhi[8];
  TH2F ** mHSuJtDEtaDPhi[8];

  // Sum Y vs Delta X
  dytBins  **mAtSYtDYt[8];     //! smt array of dmt bins
  dptBins  **mAtSPtDPt[8];     //! smt array of dmt bins
  dphiBins **mJtSEtaDPhi[8];//! 
  dphiBins **mPrJtSEtaDPhi[8];//! 
  dphiBins **mSuJtSEtaDPhi[8];//! 

  TH2F ** mHAtSYtDYt[8];
  TH2F ** mHAtSPtDPt[8];
  TH2F ** mHJtSEtaDPhi[8];//!
  TH2F ** mHPrJtSEtaDPhi[8];//!
  TH2F ** mHSuJtSEtaDPhi[8];//!

  // TPC Separation
  TPCSepBins *mTPCAvgTSep[8];  //1D
  TPCSepBins *mTPCAvgZSep[8];  
  TPCSepBins *mTPCEntTSep[8];  
  TPCSepBins *mTPCEntZSep[8];  
  TPCSepBins *mTPCMidTSep[8];  
  TPCSepBins *mTPCMidZSep[8];  
  TPCSepBins *mTPCExitTSep[8];  
  TPCSepBins *mTPCExitZSep[8];  

  TPCSepBins *mTPCMidTdptP[8]; //! needed to differentiate by sign of deltaPt   
  TPCSepBins *mTPCMidTdptN[8]; //! to evaluate pair crossing cut   
  TPCSepBins *mTPCMidZdptP[8];  
  TPCSepBins *mTPCMidZdptN[8];  

  TH1F **  mHTPCAvgTSep[8]; 
  TH1F **  mHTPCAvgZSep[8];  
  TH1F **  mHTPCEntTSep[8]; 
  TH1F **  mHTPCEntZSep[8];  
  TH1F **  mHTPCMidTSep[8]; 
  TH1F **  mHTPCMidZSep[8];  
  TH1F **  mHTPCExitTSep[8]; 
  TH1F **  mHTPCExitZSep[8];  

  TH1F **  mHTPCMidTdptP[8];  
  TH1F **  mHTPCMidTdptN[8];  
  TH1F **  mHTPCMidZdptP[8];  
  TH1F **  mHTPCMidZdptN[8];

  TPCSepBins **mTPCAvgTZ[8];  //2D
  TPCSepBins **mTPCEntTZ[8];  
  TPCSepBins **mTPCMidTZ[8];  
  TPCSepBins **mTPCExitTZ[8];  
  dptBins **mTPCEntTdpt[8];  // T vs delta-Pt; for joint hists, use bin type of y axis
  dptBins **mTPCMidTdpt[8];
  dptBins **mTPCExitTdpt[8];   

  TH2F **  mHTPCAvgTZ[8];  
  TH2F **  mHTPCEntTZ[8];  
  TH2F **  mHTPCMidTZ[8];  
  TH2F **  mHTPCExitTZ[8];  
  TH2F **  mHTPCEntTdpt[8];
  TH2F **  mHTPCMidTdpt[8];
  TH2F **  mHTPCExitTdpt[8];

  char* bName[8];
  char* bTitle[8];

  // generic histogram create functions to simplify the code
  //

  void createHist2D(TH2F*** h, const char* name, int iknd, int icut,int numCuts, int nx, float xmin, float xmax, int ny, float ymin, float ymax);
  void createHist1D(TH1F*** h, const char* name, int iknd, int icut,int numCuts, int nx, float xmin, float xmax);
  void  moveEvents();
  void  initInternalData();
  int   bufferIndex();


 public:

  int  manalysisMode; //! simple enumeration of analyses ...
  bool mskipPairCuts; //!
  bool mdoPairCutHistograms; //!
  bool mdoPairDensityHistograms; 
  bool mskipEtaDeltaWeight; //!
  bool mInit;  //! found need when overridding this class
  bool mDeleted;//! "     " ...
  bool mHistosWritten;//! "     " ...

  StEStructEvent*        mCurrentEvent;  //! pointer to EStruct2pt data 
  StEStructPairCuts*     mPairCuts;      //! for pairs kine + all paircuts
  StEStructQAHists*      mQAHists;       //! for QA histogramming
  bool                   mlocalQAHists;  //! toggle needed for who writes output
  char*     moutFileName;   //!
  char*     mqaoutFileName; //!
  StTimer*  mtimer;         //!
  StEStructEvent*     mMixingEvent;  //! dummy      //  Previous Event Stored 

  // *** had a problem using constants here (dyn. libs wouldn't load), doing this for now...
  int kNumBuffers;
  float kZBuffMin, kZBuffMax; // Read from Cuts file. Default +/- 75cm if not found.
  float kBuffWidth;           // Set to 5 cm in Initr().
  StEStructBuffer mbuffer[_MAX_ZVBINS_];  // kNumBuffers slices in z-vertex from kZBuffMin to +kZBuffMax
  //  int             mbuffCounter[_MAX_ZVBINS_];

  //-> (pre) histograms & histograms for analysis.
  // All are arrays of 8 for 8 charged sign and combinatoric types;
  // 0,1,2,3 for Sibling (++,+-,-+,--)
  // 4,5,6,7 for Mixed   (++,+-,-+,--)
  
  int numPairs[8];
  int numPairsProcessed[8];
  int mpossiblePairs[8];

  StEStruct2ptCorrelations(int mode=0);
  StEStruct2ptCorrelations(StEStructPairCuts* pcuts, int mode=0);
  virtual ~StEStruct2ptCorrelations();

  StEStructPairCuts* getPairCuts();
  void  setAnalysisMode(int mode);
  void  setCutFile(const char* cutFileName);  
  void  setZBuffLimits(StEStructCuts* cuts);// const char* cutFileName );
  void  setQAHists(StEStructQAHists* qaHists);
  void  setPairCuts(StEStructPairCuts* cuts);

  //---> support of interface  
  void  setOutputFileName(const char* outFileName);
  void  setQAOutputFileName(const char* qaoutFileName);
  bool  doEvent(StEStructEvent* p);

  void  init();
  void  cleanUp();
  void  finish();

  virtual void  fillHistograms();
  virtual void  writeHistograms();
    void   writeQAHists(TFile * tf);
    void   writeDiagnostics();

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

inline void StEStruct2ptCorrelations::setCutFile(const char* cutFileName){
  if(!mPairCuts) mPairCuts=new StEStructPairCuts;
  mPairCuts->setCutFile(cutFileName);
  mPairCuts->loadCuts();
}

inline void StEStruct2ptCorrelations::setOutputFileName(const char* fName){
  if(!fName) return;
  moutFileName=new char[strlen(fName)+1];
  strcpy(moutFileName,fName);
}

inline void StEStruct2ptCorrelations::setQAHists(StEStructQAHists* qahists){
  mQAHists = qahists;
}

inline void StEStruct2ptCorrelations::setPairCuts(StEStructPairCuts* pcuts){
  mPairCuts=pcuts;
}

inline void StEStruct2ptCorrelations::setQAOutputFileName(const char* fName){
  if(!fName) return;
  mqaoutFileName=new char[strlen(fName)+1];
  strcpy(mqaoutFileName,fName);
}

inline StEStructPairCuts* StEStruct2ptCorrelations::getPairCuts() {
  return mPairCuts;
}


inline void StEStruct2ptCorrelations::logStats(ostream& os){
  char* htp[]={"SibPP","SibPM","SibMP","SibMM","MixPP","MixPM","MixMP","MixMM"};
  for(int i=0;i<8;i++){
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
 * Revision 1.8  2006/04/04 22:10:09  porter
 * a handful of changes (specific to correlations)
 *  - added StEStructQAHists so that if NOT input frm Maker, each analysis has its own
 *  - used ability to get any max,min val from the cut class - or z-vertex binning
 *  - put z-vertex binning into 1 place
 *  - switched back 1st line of pair cut method to keep pair if good, not to reject if bad.
 *  - Pair cut object is now pointer in correlations
 *  - some diagnostic printouts available from macro
 *  - Duncan's delta-phi binning change
 *
 * Revision 1.7  2006/02/22 22:05:13  prindle
 * Removed all references to multRef (?)
 * Added cut mode 5 for particle identified correlations.
 * Other cut modes should be same as before
 *
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





