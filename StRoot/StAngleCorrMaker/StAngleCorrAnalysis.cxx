#include "StAngleCorrAnalysis.h"
#include <vector>
#include "StTrackForPool.h"
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
#include "SystemOfUnits.h"
#include <time.h>

// ROOT classes
#include <TH1.h>
#include <TFile.h>
#include "TString.h"
#include "TRandom.h"

// cut classes
#include "StTrackCuts.h"
#include "StEventCuts.h"

// correlation functions
#include "StAngleCorrFunction.h" // Base class
#include "StOpeningAngle.h"
#include "StAzimuthalAngle.h"
#include "StMassFunction.h"

// diagnostic functions
#include "StDiagnosticTool.h" // Base class
#include "StDiagnosticEventCuts.h"
#include "StDiagnosticTrack1.h"
#include "StDiagnosticTrack2.h"
#include "StDiagnosticFastestTrack.h"
#include "StDiagnosticSignal.h"
#include "StDiagnosticBackground.h"

#include "StEventTypes.h"


StAngleCorrAnalysis::~StAngleCorrAnalysis()
{
  if (signal != NULL)                        delete signal;
  if (background != NULL)              delete background;
  if (correlationFunction != NULL)  delete correlationFunction;
  if (track1Cuts != NULL)                delete track1Cuts;
  if (track2Cuts != NULL)                delete track2Cuts;
}

StAngleCorrAnalysis::StAngleCorrAnalysis(TString analysisName) 
{
 
  ON=1;
  OFF=0;
  
  FALSE=0;
  TRUE=1;
  
  track1Cuts = new StTrackCuts();
  track2Cuts = new StTrackCuts();

  fastestTrackAnalysis=OFF;
  diagnostics=OFF;

 
  mNumberOfEventsInPool=0;
  mNumberOfTracks1InPool=0;
  mNumberOfTracks2InPool=0;
  mNumberOfBackgroundEvents=0;
  mNumberOfBackgroundTracks1=0;
  mNumberOfBackgroundTracks2=0;
  minimumNumberOfBackgroundEvents=10;
  minimumNumberOfBackgroundPairs=1000;

 
  DiagnoseEventCuts  = "DiagnoseEventCuts";
  DiagnoseTrack1     = "DiagnoseTrack1";
  DiagnoseTrack2     = "DiagnoseTrack2";
  DiagnoseSignal     = "DiagnoseSignal";
  DiagnoseBackground = "DiagnoseBackground";

  name=analysisName;
  correlationFunction = new StAngleCorrFunction();

  functionLibrary.push_back(new StOpeningAngle());
  functionLibrary.push_back(new StAzimuthalAngle());
  functionLibrary.push_back(new StMassFunction());
 
  TString outputfile = name;
  outputfile.Append("Diagnostic.root");
  mOutput = new TFile(outputfile,"RECREATE");

  // fill the diagnostics library
  diagnosticsLibrary.push_back(new StDiagnosticEventCuts());
  diagnosticsLibrary.push_back(new StDiagnosticTrack1());
  diagnosticsLibrary.push_back(new StDiagnosticTrack2());
  diagnosticsLibrary.push_back(new StDiagnosticSignal());
  diagnosticsLibrary.push_back(new StDiagnosticBackground());
 
  mCollectionOfTracks1.Clear();
  mCollectionOfTracks2.Clear();
  mBackgroundTracks1.Clear();
  mBackgroundTracks2.Clear();
}

void 
StAngleCorrAnalysis::SetNBackgroundEvents(int number) { minimumNumberOfBackgroundEvents=number;}

void 
StAngleCorrAnalysis::SetNBackgroundPairs(int number) { minimumNumberOfBackgroundPairs=number;}

void 
StAngleCorrAnalysis::SetSignalHist(TH1D* sHist) { signal = sHist;}

void 
StAngleCorrAnalysis::SetBackgroundHist(TH1D* bHist) { background = bHist;}

void 
StAngleCorrAnalysis::SetCorrelationFunction(TString functionName)
{
  uint index=0;
  for (index=0;index<functionLibrary.size();index++) 
    {
      if (functionName == functionLibrary[index]->GetName() ) 
	{
	  correlationFunction=functionLibrary[index];
	  Diagnose(DiagnoseSignal)->SetCorrelationFunction(functionLibrary[index]);
	  Diagnose(DiagnoseBackground)->SetCorrelationFunction(functionLibrary[index]);
	}
    }
}

void 
StAngleCorrAnalysis::WriteDiagnostic()
{
  mOutput->Write();
  mOutput->Close();
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
StAngleCorrAnalysis::IdenticalTrackCheck(StTrackForPool* t1, StTrackForPool* t2)
{
  // here i just want to check if the two tracks are not the same track!
  Double_t px1,py1,pz1,px2,py2,pz2;
  t1->GetMomentum(px1,py1,pz1);
  t2->GetMomentum(px2,py2,pz2);
  if (px1 == px2 && py1 == py2 && pz1 == pz2) {return 1;}
  return 0;
}

int StAngleCorrAnalysis::Track1WithinCuts(StTrackForPool* t1)
{ return track1Cuts->TrackSatisfiesCuts(t1);}

int StAngleCorrAnalysis::Track2WithinCuts(StTrackForPool* t2)
{ return track2Cuts->TrackSatisfiesCuts(t2);}

int StAngleCorrAnalysis::EventWithinCuts(StEvent& ev)
{ return eventCuts->EventSatisfiesCuts(ev);}

TString StAngleCorrAnalysis::GetName()
{ return name;}

StTrackCuts*
StAngleCorrAnalysis::GetTrackCuts(TString whichTrack)
{
  TString track1 = "track1";
  TString track2 = "track2";
  if (whichTrack==track1)    {return track1Cuts;}
  if (whichTrack==track2)    {return track2Cuts;}
  return NULL;
}

StEventCuts*
StAngleCorrAnalysis::GetEventCuts()
{
 return eventCuts;
}

void
StAngleCorrAnalysis::SetTrackCuts(StTrackCuts* t1Cuts, StTrackCuts* t2Cuts)
{
  track1Cuts = t1Cuts;
  track2Cuts = t2Cuts;
}

void
StAngleCorrAnalysis::SetEventCuts(StEventCuts* evCuts)
{
  eventCuts = evCuts;
}

void
StAngleCorrAnalysis::SetTrackForPool(StTrack* track, StTrackForPool* trackForPool)
{
  StPhysicalHelixD helix = track->geometry()->helix();
  StThreeVectorD mom = helix.momentum(0.5*tesla);

  // get track characteristics
  Double_t chiSquared=0.0;
  Int_t    numberOfPossiblePoints=0;
  Int_t    charge = 0;

  chiSquared = track->fitTraits().chi2();
  charge = helix.h();
  numberOfPossiblePoints = track->numberOfPossiblePoints();  

  trackForPool->SetMomentum(mom.x(),mom.y(),mom.z());
  trackForPool->SetCharge(charge);
  trackForPool->SetChiSquared(chiSquared);
  trackForPool->SetNTPCPoints(numberOfPossiblePoints);
}

void
StAngleCorrAnalysis::ProcessEvent(StEvent& ev) 
{ 
  long counterg=0; 
  StSPtrVecTrackNode& theNodes = ev.trackNodes();
  for (unsigned int i=0; i<theNodes.size(); i++) {counterg += theNodes[i]->entries(global);}  
  
  cout << "StAngleCorrMaker,  Reading Event: " <<  "  Type: " << ev.type()  
       << "  Run: " << ev.runId() << endl;
  cout << "mag field = " << ev.summary()->magneticField() << endl
       << "N tracks = " << counterg << endl;

  // collection of global  tracks
  StPtrVecTrack tracks;
  for (unsigned int j=0; j<theNodes.size(); j++)
    {
      long tempCounter = theNodes[j]->entries(global);
      for (int k=0;k<tempCounter;k++) 
	{
	  tracks.push_back(theNodes[j]->track(global,k));
	}
    }

  StTrackForPool* fastestTrack = new StTrackForPool();
  mCollectionOfTracks1.Clear();
  mCollectionOfTracks2.Clear();
    
  if (EventWithinCuts(ev)) 
    {
      if (diagnostics) {if (Diagnose(DiagnoseEventCuts) != NULL) Diagnose(DiagnoseEventCuts)->Fill(ev);}
      mNumberOfEventsInPool++;
      mNumberOfBackgroundEvents++;

      for (int g=0;g<counterg;++g) 
 	{	  
 	  StTrackForPool* trackForPool       = new StTrackForPool();
	  StTrackForPool* trackForBackground = new StTrackForPool();
	
	  SetTrackForPool(tracks[g],trackForPool);
	  SetTrackForPool(tracks[g],trackForBackground);
	  
 	  if (Track1WithinCuts(trackForPool)) 
 	    {
	      if (diagnostics) {if (Diagnose(DiagnoseTrack1) != NULL) Diagnose(DiagnoseTrack1)->Fill(trackForPool);}
	      mCollectionOfTracks1.AddTrack(trackForPool);
	      mCollectionOfBackgroundTracks1.AddTrack(trackForBackground);
 	      mNumberOfTracks1InPool++;
 	      mNumberOfBackgroundTracks1++;
	      trackForPool->GetMomentum(trackMom);
	      fastestTrack->GetMomentum(fastestMom);
	      if (trackMom>fastestMom)  { fastestTrack=trackForPool;}
 	    }

       
	  if (Track2WithinCuts(trackForPool)) 
 	    {
	      if (diagnostics) {if (Diagnose(DiagnoseTrack2) != NULL) Diagnose(DiagnoseTrack2)->Fill(trackForPool);}
	      mCollectionOfTracks2.AddTrack(trackForPool);
	      mCollectionOfBackgroundTracks2.AddTrack(trackForBackground);
 	      mNumberOfTracks2InPool++;
 	      mNumberOfBackgroundTracks2++;
 	    }
 	}

      if (fastestTrackAnalysis==ON) 
	{
	  mCollectionOfTracks1.Clear();
	  mCollectionOfTracks1.AddTrack(fastestTrack);
	  if (diagnostics) {if (Diagnose(DiagnoseFastestTrack) != NULL) Diagnose(DiagnoseFastestTrack)->Fill(fastestTrack);}
	}

      mBackgroundTracks1.AddTrackCollection(mCollectionOfBackgroundTracks1);
      mBackgroundTracks2.AddTrackCollection(mCollectionOfBackgroundTracks2);
    }
  
  return;
 }


void
StAngleCorrAnalysis::AnalyseRealPairs() 
{
  int numberOfTracks1=mCollectionOfTracks1.Size();
  int numberOfTracks2=mCollectionOfTracks2.Size();
  int counter1=0;
  int counter2=0;
  StTrackForPool* tr1;
  StTrackForPool* tr2;

  cout << "numberOfTracks1 = " << numberOfTracks1 << endl;
  cout << "numberOfTracks2 = " << numberOfTracks2 << endl;
  
  counter1=0;
  while (counter1< numberOfTracks1) 
    {
      tr1=mCollectionOfTracks1.GetTrack(counter1);
      counter2=0;
      while (counter2 < numberOfTracks2) 
	{
	  tr2=mCollectionOfTracks2.GetTrack(counter2);
	  if (IdenticalTrackCheck(tr1,tr2)==FALSE) 
	    {
	      RelativeAngle(tr1,tr2,signal);
	      if (diagnostics) {if (Diagnose(DiagnoseSignal) != NULL) Diagnose(DiagnoseSignal)->Fill(tr1,tr2);}
	    }
	  counter2++;
	}
      counter1++;
    }

  mCollectionOfTracks1.Clear();
  mCollectionOfTracks2.Clear();
  return;
}


void
StAngleCorrAnalysis::AnalyseBackgroundPairs() 
{
  time_t t1 = time(0);   // to be used as a seed  
  TRandom *ran = new TRandom();
  ran->SetSeed(t1);  

 // reduce total number of pairs by 100 to avoid any superstatistical correlations
  UInt_t mNumberOfBackgroundPairs=mNumberOfBackgroundTracks1*mNumberOfBackgroundTracks2/100;
  
 
  if (mNumberOfBackgroundEvents > minimumNumberOfBackgroundEvents &&
       mNumberOfBackgroundPairs > minimumNumberOfBackgroundPairs)
    {
      cout << "Background analysis: Number Of Background Pairs = " << mNumberOfBackgroundPairs << endl;
      StTrackForPool* tr1;
      StTrackForPool* tr2;

      // loop over the events randomly and make random track pairs
      int trackPairs=0;
      Double_t evCounter1,evCounter2,trCounter1,trCounter2;
      Double_t totalNumberOfEvents = mBackgroundTracks1.Size();
      Double_t totalTrackPairs     = mNumberOfBackgroundPairs;

      Int_t TooManyIterations = 10000;
      while(trackPairs < totalTrackPairs) 
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

	  if (eventLoopCounter > TooManyIterations) 
	    {
	      cout << "not enough events in event pool " 
		   << "to form track pairs... will try next event loop" << endl;
	      return;
	    } 

	  trackPairs++;
	  if (mBackgroundTracks1.GetTracks(evCounter1).Size() > 0 && mBackgroundTracks2.GetTracks(evCounter2).Size() > 0) 
	    {
	      trCounter1 = ran->Rndm()*mBackgroundTracks1.GetTracks(evCounter1).Size();
	      trCounter2 = ran->Rndm()*mBackgroundTracks2.GetTracks(evCounter2).Size();
	      tr1 = mBackgroundTracks1.GetTrack(evCounter1,trCounter1);
	      tr2 = mBackgroundTracks2.GetTrack(evCounter2,trCounter2);
	      if (IdenticalTrackCheck(tr1,tr2)==FALSE) 
		{
		  RelativeAngle(tr1,tr2,background);	
		  if (diagnostics) {if (Diagnose(DiagnoseBackground) != NULL) Diagnose(DiagnoseBackground)->Fill(tr1,tr2);}
		}
	    }
	}
      
      mNumberOfBackgroundTracks1=0;
      mNumberOfBackgroundTracks2=0;
      mNumberOfBackgroundEvents=0;
      
      mBackgroundTracks1.Clear();
      mBackgroundTracks2.Clear();
    }
  return;
}

void
StAngleCorrAnalysis::RelativeAngle(StTrackForPool* t1,StTrackForPool* t2, TH1D* hist) {correlationFunction->Fill(t1,t2,hist);}

StDiagnosticTool*
StAngleCorrAnalysis::Diagnose(TString diagName) 
{ 
  uint index=0;
  for (index=0;index<diagnosticsLibrary.size();index++) 
    {
      if (diagName == diagnosticsLibrary[index]->GetName() ) return diagnosticsLibrary[index];
    }
  return NULL;
}


void
StAngleCorrAnalysis::SetDiagnosticsON() {diagnostics=ON;}
