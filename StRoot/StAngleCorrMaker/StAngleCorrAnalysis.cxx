#include "StAngleCorrAnalysis.h"
#include "StAngleCorrFunction.h"
#include "StEvent.h"
#include "StTrackForPool.h"
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
#include "SystemOfUnits.h"
#include <TH1.h>
#include <vector>
#include "TString.h"
#include "StTrackCuts.h"

// correlation functions
#include "StOpeningAngle.h"
#include "StAzimuthalAngle.h"
#include "StMassFunction.h"

// diagnostic functions
#include "StDiagnosticTool.h"

#include <time.h>
#include "TRandom.h"

StAngleCorrAnalysis::StAngleCorrAnalysis() 
{
  ON=1;
  OFF=0;
  
  track1Cuts = new StTrackCuts();
  track2Cuts = new StTrackCuts();

  fastestTrackAnalysis = OFF;
  diagnostics = OFF;
  
    // initialize system variables
  mNumberOfEventsInPool=0;
  mNumberOfTracks1InPool=0;
  mNumberOfTracks2InPool=0;
  mNumberOfBackgroundEvents=0;
  mNumberOfBackgroundTracks1=0;
  mNumberOfBackgroundTracks2=0;
  fractionToConsider=0.1;
  minimumNumberOfBackgroundEvents=10;
  minimumNumberOfBackgroundPairs=1000;

  // initialize TStrings
  name                                    = "default";
  DiagnoseEventStream   = "DiagnoseEventStream";
  DiagnoseEventCuts        = "DiagnoseEventCuts";
  DiagnoseTracks                = "DiagnoseTracks";
  DiagnoseTrack1                = "DiagnoseTrack1";
  DiagnoseTrack2                = "DiagnoseTrack2";
  DiagnoseFastestTrack    = "DiagnoseFastestTrack";

  // default function is base function class
  correlationFunction = new StAngleCorrFunction();

  // add functions to correlations library here  
  functionLibrary.push_back(new StOpeningAngle());
  functionLibrary.push_back(new StAzimuthalAngle());
  functionLibrary.push_back(new StMassFunction());
  
  // add functions to diagnostics library here
  diagnosticsLibrary.push_back(new StDiagnosticTool());
}

StAngleCorrAnalysis::StAngleCorrAnalysis(TString analysisName) 
{
  ON=1;
  OFF=0;
  
  track1Cuts = new StTrackCuts();
  track2Cuts = new StTrackCuts();

  fastestTrackAnalysis=OFF;
  diagnostics=OFF;

   // initialize system variables
  mNumberOfEventsInPool=0;
  mNumberOfTracks1InPool=0;
  mNumberOfTracks2InPool=0;
  mNumberOfBackgroundEvents=0;
  mNumberOfBackgroundTracks1=0;
  mNumberOfBackgroundTracks2=0;
  fractionToConsider=0.1;
  minimumNumberOfBackgroundEvents=10;
  minimumNumberOfBackgroundPairs=1000;

   // initialize TStrings used in Diagnostic checks
  name                                    = "default";
  DiagnoseEventStream   = "DiagnoseEventStream";
  DiagnoseEventCuts        = "DiagnoseEventCuts";
  DiagnoseTracks                = "DiagnoseTracks";
  DiagnoseTrack1                = "DiagnoseTrack1";
  DiagnoseTrack2                = "DiagnoseTrack2";
  DiagnoseFastestTrack    = "DiagnoseFastestTrack";

  name=analysisName;
  correlationFunction = new StAngleCorrFunction();

 // add functions to correlations library here  
  functionLibrary.push_back(new StOpeningAngle());
  functionLibrary.push_back(new StAzimuthalAngle());
  functionLibrary.push_back(new StMassFunction());
  
  // add functions to diagnostics library here
  diagnosticsLibrary.push_back(new StDiagnosticTool());

  // initialize vectors 
  if (mCollectionOfTracks1.size()!=0) mCollectionOfTracks1.clear();
  if (mCollectionOfTracks2.size()!=0) mCollectionOfTracks2.clear();
  if (mBackgroundTracks1.size()!=0)   mBackgroundTracks1.clear();
  if (mBackgroundTracks2.size()!=0)   mBackgroundTracks2.clear();
}

void
StAngleCorrAnalysis::SetNBackgroundEvents(int number) 
{
  minimumNumberOfBackgroundEvents=number;
}

void
StAngleCorrAnalysis::SetNBackgroundPairs(int number, Double_t fraction) 
{
  minimumNumberOfBackgroundPairs=number;
  fractionToConsider=fraction;
}

void 
StAngleCorrAnalysis::SetSignalHist(TH1D* sHist) 
{
  signal = sHist;
}

void 
StAngleCorrAnalysis::SetBackgroundHist(TH1D* bHist) 
{
  background = bHist;
}

void
StAngleCorrAnalysis::SetCorrelationFunction(TString functionName)
{
  uint index=0;
  for (index=0;index<functionLibrary.size();index++) 
    {
      if (functionName == functionLibrary[index]->GetName() ) correlationFunction=functionLibrary[index];
    }
}


StAngleCorrAnalysis::~StAngleCorrAnalysis()
{
  if (signal != NULL)                            delete signal;
  if (background != NULL)                 delete background;
  if (correlationFunction != NULL)  delete correlationFunction;
  if (track1Cuts != NULL)                  delete track1Cuts;
  if (track2Cuts != NULL)                  delete track2Cuts;
}


void
StAngleCorrAnalysis::SetFastestTrackAnalysis(int fastAnalysis)
{
  if (fastAnalysis)  
    {
      fastestTrackAnalysis=ON;
      return; 
    }

  if (!fastAnalysis) 
    {
      fastestTrackAnalysis=OFF;
      return;
    }

  cout << "ERROR: Fast Analysis is now turned OFF" << endl;
  fastestTrackAnalysis=OFF;
  exit(1);
  return;
}

int
StAngleCorrAnalysis::TracksWithinCuts(StTrackForPool* t1, StTrackForPool* t2)
{
  // here i just want to check if the two tracks are not the same track!
  Double_t px1,py1,pz1,px2,py2,pz2;
  t1->GetMomentum(px1,py1,pz1);
  t2->GetMomentum(px2,py2,pz2);
  if (px1 == px2 && py1 == py2 && pz1 == pz2) {return 0;}
  return 1;
}


int
StAngleCorrAnalysis::Track1WithinCuts(StTrackForPool* t1)
{
  return track1Cuts->TrackSatisfiesCuts(t1);
}

int
StAngleCorrAnalysis::Track2WithinCuts(StTrackForPool* t2)
{
  return track2Cuts->TrackSatisfiesCuts(t2);
}

int 
StAngleCorrAnalysis::EventWithinCuts(StEvent& ev)
{
  return 1;
}

TString 
StAngleCorrAnalysis::GetName()
{
  return name;
}

StTrackCuts*
StAngleCorrAnalysis::GetTrackCuts(TString whichTrack)
{
  TString track1 = "track1";
  TString track2 = "track2";
  if (whichTrack==track1)    {return track1Cuts;}
  if (whichTrack==track2)    {return track2Cuts;}
  return NULL;
}

void
StAngleCorrAnalysis::SetTrackCuts(StTrackCuts* t1Cuts, StTrackCuts* t2Cuts)
{
  track1Cuts = t1Cuts;
  track2Cuts = t2Cuts;
}

void
StAngleCorrAnalysis::SetTrackForPool(StGlobalTrack* globalTrack, StTrackForPool* trackForPool)
{
  StPhysicalHelixD& helix = globalTrack->helix();
  StThreeVectorD mom       = helix.momentum(0.5*tesla);

  // get track characteristics
  Double_t chiSquareXY,chiSquareZ,numberDegreeOfFreedom,
                    reducedChiSquareXY,reducedChiSquareZ;
 
  numberDegreeOfFreedom = globalTrack->fitTraits().degreesOfFreedom();
  chiSquareXY                          = globalTrack->fitTraits().chiSquaredInXY();
  chiSquareZ                             = globalTrack->fitTraits().chiSquaredInPlaneZ();
  reducedChiSquareXY         = chiSquareXY/numberDegreeOfFreedom;
  reducedChiSquareZ            = chiSquareZ/numberDegreeOfFreedom;
  
  trackForPool->SetMomentum(mom.x(),mom.y(),mom.z());
  trackForPool->SetCharge(helix.h());
  trackForPool->SetRChiSquaredXY(reducedChiSquareXY);
  trackForPool->SetRChiSquaredZ(reducedChiSquareZ);
  trackForPool->SetNTPCPoints(globalTrack->numberOfTpcHits());
}

void
StAngleCorrAnalysis::ProcessEvent(StEvent& ev) 
{  
  StTrackIterator        iter;
  StTrackCollection*  tracks = ev.trackCollection();
  StGlobalTrack*        track;
  StTrackForPool*      fastestTrack;

  if (mCollectionOfTracks1.size()!=0) mCollectionOfTracks1.clear();
  if (mCollectionOfTracks2.size()!=0) mCollectionOfTracks2.clear();
    
  if (diagnostics) {if (Diagnose(DiagnoseEventStream) != NULL) Diagnose(DiagnoseEventStream)->Fill(ev);}
  if (EventWithinCuts(ev)) 
    {
      if (diagnostics) {if (Diagnose(DiagnoseEventCuts) != NULL) Diagnose(DiagnoseEventCuts)->Fill(ev);}
      mNumberOfEventsInPool++;
      mNumberOfBackgroundEvents++;
      tracks=ev.trackCollection();

      iter=tracks->begin();
      for (iter=tracks->begin();iter!=tracks->end();++iter) 
 	{	  
 	  track = *iter;
 	  StTrackForPool* trackForPool                = new StTrackForPool();
	  StTrackForPool* trackForBackground = new StTrackForPool();
	
                      SetTrackForPool(track,trackForPool);
	  SetTrackForPool(track,trackForBackground);
	  //if (diagnostics) {if (Diagnose(DiagnoseTracks) != NULL) Diagnose(DiagnoseTracks)->Fill(trackForPool);}
	  
 	  if (Track1WithinCuts(trackForPool)) 
 	    {
	      //if (diagnostics) {if (Diagnose(DiagnoseTrack1) != NULL) Diagnose(DiagnoseTrack1)->Fill(trackForPool);}
 	      mCollectionOfTracks1.push_back(trackForPool);
	      mCollectionOfBackgroundTracks1.push_back(trackForBackground);
 	      mNumberOfTracks1InPool++;
 	      mNumberOfBackgroundTracks1++;
                          trackForPool->GetMomentum(trackMom);
                          fastestTrack->GetMomentum(fastestMom);
                          if (trackMom>=fastestMom) 
		{
		  fastestTrack=trackForPool;
		}
 	    }

	  if (Track2WithinCuts(trackForPool)) 
 	    {
	      //if (diagnostics) {if (Diagnose(DiagnoseTrack2) != NULL) Diagnose(DiagnoseTrack2)->Fill(trackForPool);}
 	      mCollectionOfTracks2.push_back(trackForPool);
	      mCollectionOfBackgroundTracks2.push_back(trackForBackground);
 	      mNumberOfTracks2InPool++;
 	      mNumberOfBackgroundTracks2++;
 	    }
 	}

      if (fastestTrackAnalysis==ON) 
	{
	  mCollectionOfTracks1.clear();
	  mCollectionOfTracks1.push_back(fastestTrack);
	  // if (diagnostics) {if (Diagnose(DiagnoseFastestTrack) != NULL) Diagnose(DiagnoseFastestTrack)->Fill(fastestTrack);}
	}

      mBackgroundTracks1.push_back(mCollectionOfBackgroundTracks1);
      mBackgroundTracks2.push_back(mCollectionOfBackgroundTracks2);
    }
  
  cout << "StAngleCorrAnalysis:  Event Summary: " << 
    " Type " << ev.type()    << " Run " << ev.runNumber()     << endl;
  cout << " N vertex "       << ev.vertexCollection()->size()  << endl;
  cout << " N track "         << ev.trackCollection()->size()    << endl;
  cout << " N TPC hit "    << ev.tpcHitCollection()->size()  << endl;
  cout << " N FTPC hit " << ev.ftpcHitCollection()->size() << endl;
  cout << " N SVT hit "    << ev.svtHitCollection()->size()  << endl;

  return;
 }


void
StAngleCorrAnalysis::AnalyseRealPairs() 
{
  int numberOfTracks1=mCollectionOfTracks1.size();
  int numberOfTracks2=mCollectionOfTracks2.size();
  int counter1=0;
  int counter2=0;
  StTrackForPool* tr1;
  StTrackForPool* tr2;

  counter1=0;
  while (counter1<numberOfTracks1) 
    {
      tr1=mCollectionOfTracks1[counter1];
      counter2=0;
      while (counter2<numberOfTracks2) 
	{
	  tr2=mCollectionOfTracks2[counter2];
	  RelativeAngle(tr1,tr2,signal);
	  // if (diagnostics) {if (Diagnose(DiagnoseSignal) != NULL) Diagnose(DiagnoseSignal)->Fill(tr1,tr2);}
	  counter2++;
	}
      counter1++;
    }
  if (mCollectionOfTracks1.size()!=0) mCollectionOfTracks1.clear();
  if (mCollectionOfTracks2.size()!=0) mCollectionOfTracks2.clear();
  return;
}


void
StAngleCorrAnalysis::AnalyseBackgroundPairs() 
{
  time_t t1 = time(0);   // to be used as a seed  
  TRandom *ran = new TRandom();
  ran->SetSeed(t1);  

 // reduce total number of pairs by 100 to avoid any superstatistical correlations
  int mNumberOfBackgroundPairs=mNumberOfBackgroundTracks1*
                                                                    mNumberOfBackgroundTracks2/100;
 
  if (mNumberOfBackgroundEvents>minimumNumberOfBackgroundEvents &&
       mNumberOfBackgroundPairs>minimumNumberOfBackgroundPairs)
    {
      StTrackForPool* tr1;
      StTrackForPool* tr2;

      // loop over the events randomly and make random track pairs
      int trackPairs=0;
      Double_t evCounter1,evCounter2,trCounter1,trCounter2;
      Double_t totalNumberOfEvents=mBackgroundTracks1.size();
      Double_t totalTrackPairs=(fractionToConsider)*mNumberOfBackgroundPairs;
      cout << "total num of back tracks under consideration = " << totalTrackPairs << endl;

      Int_t TooManyIterations = 10000;
      while(trackPairs<totalTrackPairs) 
	{ 
	  Int_t eventLoopCounter=0;
	  evCounter1 =  ran->Rndm()*totalNumberOfEvents;
	  evCounter2 =  evCounter1;
	  while (evCounter2==evCounter1) 
	    {
	      evCounter2 =  ran->Rndm()*totalNumberOfEvents;
	      eventLoopCounter++;
	      if (eventLoopCounter>TooManyIterations) break;
	    }

	  if (eventLoopCounter >= TooManyIterations) 
	    {
	      cout << "not enough events in event pool " << 
                                         "  to form track pairs... will try next event loop" << endl;
	      return;
	    } 

	  trCounter1 = ran->Rndm()*mBackgroundTracks1[evCounter1].size();
	  trCounter2 = ran->Rndm()*mBackgroundTracks2[evCounter2].size();
	  tr1 = mBackgroundTracks1[evCounter1][trCounter1];
	  tr2 = mBackgroundTracks2[evCounter2][trCounter2];
                      // if (diagnostics) {if (Diagnose(DiagnoseBackground) != NULL) Diagnose(DiagnoseBackground)->Fill(tr1,tr2);}
	  RelativeAngle(tr1,tr2,background);
	  trackPairs++;
	}
      
      mNumberOfBackgroundTracks1=0;
      mNumberOfBackgroundTracks2=0;
      mNumberOfBackgroundEvents=0;

      if (!mBackgroundTracks1.empty()) { mBackgroundTracks1.clear();}
      if (!mBackgroundTracks2.empty()) { mBackgroundTracks2.clear();}
    }
  return;
}

void
StAngleCorrAnalysis::RelativeAngle(StTrackForPool* t1,StTrackForPool* t2, TH1D* hist) 
{ 
  correlationFunction->Fill(t1,t2,hist);
}

StDiagnosticTool*
StAngleCorrAnalysis::Diagnose(TString diagName) 
{ 
  uint index=0;
  for (index=0;index<functionLibrary.size();index++) 
    {
      if (diagName == diagnosticsLibrary[index]->GetName() ) return diagnosticsLibrary[index];
    }
  return NULL;
}


void
StAngleCorrAnalysis::SetDiagnosticsON()
{
  diagnostics=ON;
}
