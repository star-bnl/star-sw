#ifndef StDiagnosticTool_HH
#define StDiagnosticTool_HH

#include <TNtuple.h>
#include "StTrackForPool.h"
#include "StAngleCorrFunction.h"

class StEvent;

class StDiagnosticTool {
public:
                       StDiagnosticTool();
  virtual              ~StDiagnosticTool();
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
