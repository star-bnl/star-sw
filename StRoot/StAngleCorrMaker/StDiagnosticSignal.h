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
#include <TH3.h>
#include "StAngleCorrFunction.h"

class StDiagnosticSignal: public StDiagnosticTool {
public:
                   StDiagnosticSignal();
                  ~StDiagnosticSignal();
  void        Fill(StTrackForPool* t1, StTrackForPool* t2);
  void        Write();
  void        SetCorrelationFunction(StAngleCorrFunction* func);
  TString GetName();

private:
  StAngleCorrFunction* corrFunc;
  TH3D* signalCut1;
  TH3D* signalCut2;
  TH3D* signalCut3;
  TH3D* signalCut4;
  TH3D* signalCut5;

};

#endif
