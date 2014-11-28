/**********************************************************************
 *
 * $Id: StEStructPhiWeight.h,v 1.1 2006/04/26 18:58:15 dkettler Exp $
 *
 * Author: David Kettler
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
#ifndef __STESTRUCTPHIWEIGHT__H
#define __STESTRUCTPHIWEIGHT__H


#include "TROOT.h"
#include "StEStructPool/AnalysisMaker/StEStructAnalysis.h"
#include "StEStructPool/EventMaker/StEStructCentrality.h"
//#include "StEStructPairCuts.h"
//#include "StEStructBinning.h"
//#include "StEStructBuffer.h"

class TFile;
class TH1F;
//class TH2F;
class StEStructEvent;
class StTimer;
class StEStructQAHists;

class StEStructPhiWeight: public StEStructAnalysis {
 private:
  char*     moutFileName;
  char*     mqaoutFileName;
  int nPhiBins;
  float phiMin;
  float phiMax;
  TH1F* mPhiHist;
  StEStructQAHists*      mQAHists;       //! for QA histogramming
  
 public:
	StEStructPhiWeight();
	~StEStructPhiWeight();

  StEStructEvent* mCurrentEvent;

  void setOutputFileName(const char* outFileName);
  void setQAOutputFileName(const char* qaoutFileName);
  void  setQAHists( StEStructQAHists* qaHists );
  void  setCutFile(const char* cutFileName, StEStructCentrality *cent);  // no-op, see below
  bool doEvent(StEStructEvent* event);
  void finish();


  // new method; should be pure-virtual but make it 'no-opt' so older codes
  // won't be required to implement it
  void logStats(ostream& os);

  ClassDef(StEStructPhiWeight,1)
};

inline void StEStructPhiWeight::setOutputFileName(const char* fName){
  if(!fName) return;
  moutFileName=new char[strlen(fName)+1];
  strcpy(moutFileName,fName);
}
inline void StEStructPhiWeight::setQAOutputFileName(const char* fName){
  if(!fName) return;
  mqaoutFileName=new char[strlen(fName)+1];
  strcpy(mqaoutFileName,fName);
}
inline void StEStructPhiWeight::setQAHists(StEStructQAHists* qahists){
  mQAHists = qahists;
}

inline void StEStructPhiWeight::setCutFile(const char* cutFileName, StEStructCentrality *cent)
  // doesn't do anything, just here to have consistant interface with the 2pt correlation analysis
{ }

inline void StEStructPhiWeight::logStats(ostream& os){
  os << "<stats></stats>";
};

#endif


/***********************************************************************
 *
 * $Log: StEStructPhiWeight.h,v $
 * Revision 1.1  2006/04/26 18:58:15  dkettler
 *
 * Simple version of weight calculation analysis
 *
 *
 *********************************************************************/





