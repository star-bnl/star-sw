#ifndef StDiagnosticSignal_HH
#define StDiagnosticSignal_HH

///////////////////////////////////////////////////////////////////////////////
//
// StDiagnosticSignal
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

class StDiagnosticSignal: public StDiagnosticTool {
public:
                   StDiagnosticSignal();
                  ~StDiagnosticSignal();
  void        Fill(StTrackForPool* t1, StTrackForPool* t2);
  void        Write();
  TString GetName();
private:
  TH1D* signalCut;
};

#endif
