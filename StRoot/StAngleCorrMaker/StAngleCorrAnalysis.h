#ifndef StAngleCorrAnalysis_HH
#define StAngleCorrAnalysis_HH

///////////////////////////////////////////////////////////////////////////////
//
// StAngleCorrAnalysis
//
// Description: 
//  Calculates high-pt angular correlations from StEvent
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Craig Ogilvie, MIT
//  Matt Horsley,  YALE
// History: 
//
///////////////////////////////////////////////////////////////////////////////


#include <TH1.h>
#include <TCanvas.h>
#include <TFile.h>
#include "StTrackForPool.h"
#include "StEvent.h"
#include <vector>
#include "TString.h"
#include "StAngleCorrFunction.h"
#include "StTrackCuts.h"
#include "StDiagnosticTool.h"


class StAngleCorrAnalysis {
private:
  TH1D*   signal;
  TH1D*   background;

  StAngleCorrFunction*  correlationFunction;

  StTrackCuts*   track1Cuts;
  StTrackCuts*   track2Cuts;

  Double_t  trackMom,fastestMom;

  int   mNumberOfEventsInPool,mNumberOfTracks1InPool,mNumberOfTracks2InPool;
  int   mNumberOfBackgroundTracks1,mNumberOfBackgroundTracks2,mNumberOfBackgroundEvents;
  int   minimumNumberOfBackgroundEvents,minimumNumberOfBackgroundTracks;
  
  // for storing tracks
  StGlobalTrack* track1;
  StGlobalTrack* track2;
  vector<StTrackForPool*> mCollectionOfTracks1;
  vector<StTrackForPool*> mCollectionOfTracks2;
  vector<StTrackForPool*> mCollectionOfBackgroundTracks1;
  vector<StTrackForPool*> mCollectionOfBackgroundTracks2;
  
  // background
  vector< vector<StTrackForPool*> > mBackgroundTracks1;
  vector< vector<StTrackForPool*> > mBackgroundTracks2;
 
  // functions, diagnostics
  vector<StAngleCorrFunction* > functionLibrary;
  vector<StDiagnosticTool* > diagnosticsLibrary;
  TString name;
  TString DiagnoseEventStream,DiagnoseEventCuts;
  TString DiagnoseTracks,DiagnoseTrack1,DiagnoseTrack2;
  TString DiagnoseFastestTrack,DiagnoseSignal,DiagnoseBackground;
  int  diagnostics;

  // private member functions,variables
  int    fastestTrackAnalysis,ON,OFF;
  int    EventWithinCuts(StEvent& ev);
  int    TracksWithinCuts(StTrackForPool* t1, StTrackForPool* t2);
  int    Track1WithinCuts(StTrackForPool* t1);
  int    Track2WithinCuts(StTrackForPool* t2);
  void   RelativeAngle(StTrackForPool* t1, StTrackForPool* t2, TH1D* hist); 
  void   SetTrackForPool(StGlobalTrack* track, StTrackForPool* trackForPool);
  void   SetTrackCuts(StTrackCuts* t1Cuts, StTrackCuts* t2Cuts);
  StDiagnosticTool* Diagnose(TString diagName); 


public:
       StAngleCorrAnalysis();
      StAngleCorrAnalysis(TString analysisName);
     ~StAngleCorrAnalysis();
    TString GetName();
    void       SetCorrelationFunction(TString functionName);
    void       SetSignalHist(TH1D* sHist);
    void       SetBackgroundHist(TH1D* bHist);
    void       SetTrackForPool(StGlobalTrack& globalTrack, StTrackForPool* trackForPool);
    void       ProcessEvent(StEvent& ev);
    void       AnalyseRealPairs();
    void       AnalyseBackgroundPairs();
    void       SetFastestTrackAnalysis(int fastAnalysis);
    void       SetDiagnosticsON();
    void       SetNBackgroundEvents(int number);
    void       SetNBackgroundTracks(int number);
    StTrackCuts*     GetTrackCuts(TString whichTrack);

};

#endif
