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
#include <TH3.h>
#include "StAngleCorrFunction.h"

class StDiagnosticBackground: public StDiagnosticTool {
public:
                   StDiagnosticBackground();
                  ~StDiagnosticBackground();
  void        Fill(StTrackForPool* t1, StTrackForPool* t2);
  void        Write();
  TString   GetName();
  void        SetCorrelationFunction(StAngleCorrFunction* func);

 private:
  StAngleCorrFunction* corrFunc;

  TH3D* backgroundCut1;
  TH3D* backgroundCut2;
  TH3D* backgroundCut3;
  TH3D* backgroundCut4;
  TH3D* backgroundCut5;

};

#endif
