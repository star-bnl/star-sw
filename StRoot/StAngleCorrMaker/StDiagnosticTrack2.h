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
  void        Write();
  TString GetName();
private:
  TH1D* track2NTPC;
  TH1D* track2Mom;
  TH1D* track2Ch;
};

#endif
