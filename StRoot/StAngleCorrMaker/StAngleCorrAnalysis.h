#ifndef StAngleCorrAnalysis_HH
#define StAngleCorrAnalysis_HH

#include "StTrackForPool.h"
#include "StTrackForPoolCollection.h"
#include "StBackgroundTrackCollection.h"
#include <vector>
#include "StAngleCorrFunction.h"
#include "StDiagnosticTool.h"

#include "StTrackCuts.h"
#include "StEventCuts.h"
#include <TH1.h>
#include <TCanvas.h>
#include <TFile.h>
#include "TString.h"

class StGlobalTrack;
class StTrack;
class StEvent;

class StAngleCorrAnalysis {
private:
  TH1D* signal;
  TH1D* background;
  int FALSE,TRUE;
  StAngleCorrFunction*  correlationFunction;

  /* cuts */
  StTrackCuts*  track1Cuts;
  StTrackCuts*  track2Cuts;
  StEventCuts*  eventCuts;

  Double_t  trackMom,fastestMom;

  UInt_t   mNumberOfEventsInPool,mNumberOfTracks1InPool,mNumberOfTracks2InPool;
  UInt_t   mNumberOfBackgroundTracks1,mNumberOfBackgroundTracks2,mNumberOfBackgroundEvents;
  UInt_t   minimumNumberOfBackgroundEvents,minimumNumberOfBackgroundPairs;
  
  /* for storing tracks */
  StTrackForPoolCollection mCollectionOfTracks1;
  StTrackForPoolCollection mCollectionOfTracks2;
  StTrackForPoolCollection mCollectionOfBackgroundTracks1;
  StTrackForPoolCollection mCollectionOfBackgroundTracks2;
  
  /* background */
  StBackgroundTrackCollection mBackgroundTracks1;
  StBackgroundTrackCollection mBackgroundTracks2;
 
  /* functions, diagnostics */
  vector<StAngleCorrFunction* > functionLibrary;
  vector<StDiagnosticTool* >  diagnosticsLibrary;
  TString name;
  TString DiagnoseEventStream,DiagnoseEventCuts;
  TString DiagnoseTracks,DiagnoseTrack1,DiagnoseTrack2;
  TString DiagnoseFastestTrack,DiagnoseSignal,DiagnoseBackground;
  int  diagnostics;
  TFile* mOutput;

  /* private member functions,variables */
  int fastestTrackAnalysis,ON,OFF;
  
  void   RelativeAngle(StTrackForPool* t1, StTrackForPool* t2, TH1D* hist); 
  void   SetTrackForPool(StTrack* track, StTrackForPool* trackForPool); 

  /* diagnostics */
  StDiagnosticTool* Diagnose(TString diagName);
  
  /* cuts */
  int     EventWithinCuts(StEvent& ev);
  int     IdenticalTrackCheck(StTrackForPool* t1, StTrackForPool* t2);
  int     Track1WithinCuts(StTrackForPool* t1);
  int     Track2WithinCuts(StTrackForPool* t2);
  
public:
            StAngleCorrAnalysis(TString analysisName);
            ~StAngleCorrAnalysis();
  TString   GetName();
  void      SetCorrelationFunction(TString functionName);
  void      SetSignalHist(TH1D* sHist);
  void      SetBackgroundHist(TH1D* bHist);
  void      SetTrackForPool(StGlobalTrack& globalTrack, StTrackForPool* trackForPool);
  void      ProcessEvent(StEvent& ev);
  void      AnalyseRealPairs();
  void      AnalyseBackgroundPairs();
  void      SetFastestTrackAnalysis(int fastAnalysis);
  void      SetNBackgroundEvents(int number);
  void      SetNBackgroundPairs(int number);

  /* diagnostics */
  void      SetDiagnosticsON();
  void      WriteDiagnostic();
  
  /* cuts */
  StTrackCuts*    GetTrackCuts(TString whichTrack);
  StEventCuts*    GetEventCuts();
  void            SetTrackCuts(StTrackCuts* t1Cuts, StTrackCuts* t2Cuts);
  void            SetEventCuts(StEventCuts* evCuts);
};

#endif
