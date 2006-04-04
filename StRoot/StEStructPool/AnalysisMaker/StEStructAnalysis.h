/**********************************************************************
 *
 * $Id: StEStructAnalysis.h,v 1.3 2006/04/04 22:05:03 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Abstract analysis class
 *
 ***********************************************************************/
#ifndef _STEBYEANALYSIS_H
#define _STEBYEANALYSIS_H

#include "TROOT.h"
#include "Stiostream.h"

class StEStructEvent;
class TFile;

class StEStructAnalysis {

 protected:

  int manalysisIndex; //! may be needed for naming of many hists in same file 

 public:

  StEStructAnalysis();
  virtual ~StEStructAnalysis() {};

  virtual void setOutputFileName(const char* outFileName) = 0;
  virtual bool doEvent(StEStructEvent* event) = 0;
  virtual void finish()  = 0;

  // new methods; should be pure-virtual but make it 'no-opt' so older codes
  // won't be required to implement it 
  virtual void logStats(ostream& os){ /* no opt */ }; 
  virtual void writeQAHists(TFile * tf){ /* no opt */ };
  virtual void setAnalysisIndex(int i);
  virtual int  analysisIndex();

  // called by the maker when one wants to do some periodic printouts
  virtual void writeDiagnostics(){/* no opt*/ }; 

  ClassDef(StEStructAnalysis,1)
};


inline void StEStructAnalysis::setAnalysisIndex(int i){ manalysisIndex=i; };
inline int StEStructAnalysis::analysisIndex(){ return manalysisIndex; };

#endif

/***********************************************************************
 *
 * $Log: StEStructAnalysis.h,v $
 * Revision 1.3  2006/04/04 22:05:03  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 * Revision 1.2  2004/06/25 03:10:22  porter
 * added a new common statistics output and added electron cut with momentum slices
 *
 * Revision 1.1  2003/10/15 18:20:31  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/

