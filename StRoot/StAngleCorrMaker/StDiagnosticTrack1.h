#ifndef StDiagnosticTrack1_HH
#define StDiagnosticTrack1_HH

///////////////////////////////////////////////////////////////////////////////
//
// StDiagnosticTrack1
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

class StDiagnosticTrack1: public StDiagnosticTool {
public:
                   StDiagnosticTrack1();
                  ~StDiagnosticTrack1();
  void        Fill(StTrackForPool* t);
//   avoid hidden warn
  void        Fill(StEvent& ev){StDiagnosticTool::Fill(ev);}
  void        Fill(StTrackForPool* t1, StTrackForPool* t2){StDiagnosticTool::Fill(t1,t2);}

  void        Write();
  TString GetName();
private:
  TH1D* track1NTPC;
  TH1D* track1Mom;
  TH1D* track1Ch;
};

#endif
