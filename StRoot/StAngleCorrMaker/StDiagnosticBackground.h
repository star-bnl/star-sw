#ifndef StDiagnosticBackground_HH
#define StDiagnosticBackground_HH

///////////////////////////////////////////////////////////////////////////////
//
// StDiagnosticBackground
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


class StDiagnosticBackground: public StDiagnosticTool {
public:
                   StDiagnosticBackground();
                  ~StDiagnosticBackground();
  void        Fill(StTrackForPool* t1, StTrackForPool* t2);
  void        Write();
  TString GetName();
private:
  TH1D* backgroundCut;
};

#endif
