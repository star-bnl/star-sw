#ifndef StDiagnosticFastestTrack_HH
#define StDiagnosticFastestTrack_HH

///////////////////////////////////////////////////////////////////////////////
//
// StDiagnosticFastestTrack
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

class StDiagnosticFastestTrack: public StDiagnosticTool {
public:
                   StDiagnosticFastestTrack();
                  ~StDiagnosticFastestTrack();
  void        Fill(StTrackForPool* t);
  void        Write();
  TString GetName();
private:
  TH1D* fasttrackNTPC;
  TH1D* fasttrackMom;
  TH1D* fasttrackCh;
};

#endif
