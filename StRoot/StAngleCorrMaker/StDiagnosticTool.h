#ifndef StDiagnosticTool_HH
#define StDiagnosticTool_HH

///////////////////////////////////////////////////////////////////////////////
//
// StDiagnosticTool
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

#include "StEvent.h"
#include <TNtuple.h>
#include "StTrackForPool.h"
#include "StAngleCorrFunction.h"

class StDiagnosticTool {
public:
                                 StDiagnosticTool();
  virtual                  ~StDiagnosticTool();
  virtual  void        Fill(StEvent& ev);
  virtual  void        Fill(StTrackForPool* t);
  virtual  void        Fill(StTrackForPool* t1, StTrackForPool* t2);
  virtual  void        Write();
  virtual  void        SetCorrelationFunction(StAngleCorrFunction* func);
  virtual  TString   GetName();

private:
  StAngleCorrFunction* corrFunc;
};

#endif
