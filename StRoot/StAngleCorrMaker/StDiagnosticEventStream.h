#ifndef StDiagnosticEventStream_HH
#define StDiagnosticEventStream_HH

///////////////////////////////////////////////////////////////////////////////
//
// StDiagnosticEventStream
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

class StDiagnosticEventStream: public StDiagnosticTool {
public:
                  StDiagnosticEventStream();
                  ~StDiagnosticEventStream();
  void        Fill(StEvent& ev);
// warnoff
  void        Fill(StTrackForPool* t){StDiagnosticTool::Fill(t);}
  void        Fill(StTrackForPool* t1, StTrackForPool* t2){StDiagnosticTool::Fill(t1,t2);}

    void        Write();
  TString GetName();
private:
  TH1D*  evstreamMULT;
};

#endif
