#ifndef StDiagnosticEventCuts_HH
#define StDiagnosticEventCuts_HH

///////////////////////////////////////////////////////////////////////////////
//
// StDiagnosticEventCuts
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


class StDiagnosticEventCuts: public StDiagnosticTool {
public:
                   StDiagnosticEventCuts();
                  ~StDiagnosticEventCuts();
  void        Fill(StEvent& ev);
// avoid hidden warning
  void        Fill(StTrackForPool* t)                     {StDiagnosticTool::Fill(t);}
  void        Fill(StTrackForPool* t1, StTrackForPool* t2){StDiagnosticTool::Fill(t1,t2);}

  void        Write();
  TString GetName();
private:
  TH1D* evcutMULT;
};

#endif
