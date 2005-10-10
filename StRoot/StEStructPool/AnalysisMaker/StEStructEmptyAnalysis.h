/**********************************************************************
 *
 * $Id: StEStructEmptyAnalysis.h,v 1.5 2005/10/10 16:22:31 msd Exp $
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
  
  int mhm[14];        // Jeff had these in place for p-p analysis, probably don't need them anymore
  TH1F** etaMean[3];
  TH1F** phiMean[3];
  TH1F** ytMean[3];
  
  // Make some plots for determining centrality bins
  TH1F* hNEvent;  // dNevent/dNch
  TH1F* hvar;     //variable bins;  I'd like to use a TGraph instead of TH1F, but hadd doesn't support graphs...

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

