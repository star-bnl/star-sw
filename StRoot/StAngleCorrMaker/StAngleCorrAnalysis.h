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
#include "StTrackForPool.h"
#include "StEvent.h"
#include <vector>
#include "StAngleCorrFunction.h"
#include "StDiagnosticTool.h"

// cut classes
#include "StTrackCuts.h"
#include "StEventCuts.h"

// ROOT classes
#include <TH1.h>
#include <TCanvas.h>
#include <TFile.h>
#include "TString.h"

class StAngleCorrAnalysis {
private:
  TH1D*   signal;
  TH1D*   background;
  
  int FALSE,TRUE;

  StAngleCorrFunction*  correlationFunction;

  // cuts
  StTrackCuts*  track1Cuts;
  StTrackCuts*  track2Cuts;
  StEventCuts*  eventCuts;

  Double_t  trackMom,fastestMom;

  UInt_t   mNumberOfEventsInPool,mNumberOfTracks1InPool,mNumberOfTracks2InPool;
  UInt_t   mNumberOfBackgroundTracks1,mNumberOfBackgroundTracks2,mNumberOfBackgroundEvents;
  UInt_t   minimumNumberOfBackgroundEvents,minimumNumberOfBackgroundPairs;
  
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
  vector<StDiagnosticTool* >       diagnosticsLibrary;
  TString name;
  TString DiagnoseEventStream,DiagnoseEventCuts;
  TString DiagnoseTracks,DiagnoseTrack1,DiagnoseTrack2;
  TString DiagnoseFastestTrack,DiagnoseSignal,DiagnoseBackground;
  int  diagnostics;
  TFile* mOutput;

  // private member functions,variables
  int fastestTrackAnalysis,ON,OFF;
  Double_t fractionToConsider;

  void   RelativeAngle(StTrackForPool* t1, StTrackForPool* t2, TH1D* hist); 
  void   SetTrackForPool(StGlobalTrack* track, StTrackForPool* trackForPool); 

  // diagnostics
  StDiagnosticTool* Diagnose(TString diagName);
  
  // cuts
  int     EventWithinCuts(StEvent& ev);
  int     IdenticalTrackCheck(StTrackForPool* t1, StTrackForPool* t2);
  int     Track1WithinCuts(StTrackForPool* t1);
  int     Track2WithinCuts(StTrackForPool* t2);
  
public:
                            StAngleCorrAnalysis();
                            StAngleCorrAnalysis(TString analysisName);
                            ~StAngleCorrAnalysis();
  TString              GetName();
  void                   SetCorrelationFunction(TString functionName);
  void                   SetSignalHist(TH1D* sHist);
  void                   SetBackgroundHist(TH1D* bHist);
  void                   SetTrackForPool(StGlobalTrack& globalTrack, StTrackForPool* trackForPool);
  void                   ProcessEvent(StEvent& ev);
  void                   AnalyseRealPairs();
  void                   AnalyseBackgroundPairs();
  void                   SetFastestTrackAnalysis(int fastAnalysis);
  void                   SetNBackgroundEvents(int number);
  void                   SetNBackgroundPairs(int number, Double_t fraction);

  // diagnostics
  void                   SetDiagnosticsON();
  void                   WriteDiagnostic();
  
  // cuts
  StTrackCuts*    GetTrackCuts(TString whichTrack);
  StEventCuts*    GetEventCuts();
  void                   SetTrackCuts(StTrackCuts* t1Cuts, StTrackCuts* t2Cuts);
  void                   SetEventCuts(StEventCuts* evCuts);


};

#endif
