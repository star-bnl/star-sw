#ifndef StDiagnosticTracks_HH
#define StDiagnosticTracks_HH

///////////////////////////////////////////////////////////////////////////////
//
// StDiagnosticTracks
//
// Description: 
//
// Environment:
//  Software developed for the STAR Detector at 
//  Brookhaven National Laboratory
//
// Author List: 
//  Craig Ogilvie, MIT
//  Matt Horsley,  YALE
// History:
//
///////////////////////////////////////////////////////////////////////////////

#include "StTrackForPool.h"
#include "StDiagnosticTool.h"
#include <TH1.h>

class StDiagnosticTracks: public StDiagnosticTool {
public:
                   StDiagnosticTracks();
                  ~StDiagnosticTracks();
  void        Fill(StTrackForPool* t);
//VP warnOff
  void        Fill(StEvent& ev){StDiagnosticTool::Fill(ev);}
  void        Fill(StTrackForPool* t1, StTrackForPool* t2){StDiagnosticTool::Fill(t1,t2);}
  
  void        Write();
  TString GetName();
private:
  TH1D* tracksNTPC;
  TH1D* tracksMom;
  TH1D* tracksCh;
};

#endif
