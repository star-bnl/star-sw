#ifndef StDiagnosticTrack2_HH
#define StDiagnosticTrack2_HH

///////////////////////////////////////////////////////////////////////////////
//
// StDiagnosticTrack2
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

class StDiagnosticTrack2: public StDiagnosticTool {
public:
                   StDiagnosticTrack2();
                  ~StDiagnosticTrack2();
  void        Fill(StTrackForPool* t);
// avoid warning
  void        Fill(StEvent& ev){StDiagnosticTool::Fill(ev);}
  void        Fill(StTrackForPool* t1, StTrackForPool* t2){StDiagnosticTool::Fill(t1,t2);}

  void        Write();
  TString GetName();
private:
  TH1D* track2NTPC;
  TH1D* track2Mom;
  TH1D* track2Ch;
};

#endif
