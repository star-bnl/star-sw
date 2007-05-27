/**********************************************************************
 *
 * $Id: StEStructEmptyAnalysis.h,v 1.8 2007/05/27 22:43:35 msd Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Empty analysis code for testing
 *               Can replace StEStruct2ptCorrelations for an analysis that
 *               reads events, applies all event and track (NOT pair) cuts,
 *               and outputs a few histograms.  
 *
 **********************************************************************/
#ifndef _STESTRUCT_EMPTY_HH
#define _STESTRUCT_EMPTY_HH


#include "TROOT.h"
#include "StEStructAnalysis.h"
#include "StEStructPool/EventMaker/StEStructCentrality.h"

class TH1F;

class StEStructEmptyAnalysis : public StEStructAnalysis {

  char* moutFileName;
  
  // Make some plots for determining centrality bins
  TH1F* hNEvent;  // dNevent/dNch using Centrality
  TH1F* hnt;      // dNevent/dNch using Ntrack
  TH1F* hvar;     //variable bins;  I'd like to use a TGraph instead of TH1F, but hadd doesn't support graphs...
  TH1F* href;     // refMult
  TH1F* hnumPrim; // from StEventSummary::numberOfGoodPrimaryTracks()
  TH1D* hctb;     // ctb multiplicity

  TH1F* hnev14;   // centrality^1/4
  TH1F* hnt14;    // ntrack^1/4 
  TH1F* href14;     // refMult^1/4            
  TH1F* hnumPrim14; // numPrim^1/4
  TH1D* hctb14;     // ctb^1/4

  // Track Level plots
  TH1F* hmeanpt;   // event <pt>
  TH1F* hmeanpt14; // <pt>^1/4

 public:

  StEStructEmptyAnalysis();
  ~StEStructEmptyAnalysis(){};

  virtual void setOutputFileName(const char* outFileName);
  void  setCutFile(const char* cutFileName, StEStructCentrality *cent);  // no-op, see below
  virtual bool doEvent(StEStructEvent* event); 
  virtual void finish();

  ClassDef(StEStructEmptyAnalysis,1)
};

inline void StEStructEmptyAnalysis::setOutputFileName(const char* fName){
  if(!fName) return;
  moutFileName=new char[strlen(fName)+1];
  strcpy(moutFileName,fName);
}

inline void StEStructEmptyAnalysis::setCutFile(const char* cutFileName, StEStructCentrality *cent)
  // doesn't do anything, just here to have consistant interface with the 2pt correlation analysis 
{ }

#endif
/**********************************************************************
 *
 * $Log: StEStructEmptyAnalysis.h,v $
 * Revision 1.8  2007/05/27 22:43:35  msd
 * Added new centrality plots to Empty analysis
 *
 * Revision 1.7  2007/01/26 17:09:28  msd
 * Minor bug fix in AnalysisMaker, cleaned up EmptyAnalysis
 *
 * Revision 1.6  2006/04/04 22:05:05  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 * Revision 1.5  2005/10/10 16:22:31  msd
 * stability fixes, yet another tweak of output hists
 *
 * Revision 1.4  2005/10/04 16:06:19  msd
 * Finalized centrality plots
 *
 * Revision 1.3  2005/09/29 17:40:31  msd
 * Changed empty analysis to create plots for determining centrality bins
 *
 * Revision 1.2  2004/06/25 03:10:29  porter
 * added a new common statistics output and added electron cut with momentum slices
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/

