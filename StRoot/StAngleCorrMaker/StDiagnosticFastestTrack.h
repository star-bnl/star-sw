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
//VP
  virtual  void        Fill(StEvent& ev){StDiagnosticTool::Fill(ev);}
  virtual  void        Fill(StTrackForPool* t1, StTrackForPool* t2){StDiagnosticTool::Fill(t1,t2);}
  
  void        Write();
  TString GetName();
private:
  TH1D* fasttrackNTPC;
  TH1D* fasttrackMom;
  TH1D* fasttrackCh;
};

#endif
