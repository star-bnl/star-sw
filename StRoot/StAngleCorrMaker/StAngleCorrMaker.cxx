// $Id: StAngleCorrMaker.cxx,v 1.10 1999/12/29 01:55:45 horsley Exp $
// $Log: StAngleCorrMaker.cxx,v $
// Revision 1.10  1999/12/29 01:55:45  horsley
// *** empty log message ***
//
// Revision 1.9  1999/12/28 19:10:02  horsley
// *** empty log message ***
//
//
///////////////////////////////////////////////////////////////////////////////
//
// StAngleCorrMaker
// 
// 1999/06/27 13:56:40
// Version minimally changed from standard StAnalysisMaker
//
// Description: 
//  Executes StAngleCorr
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Craig Ogilive, MIT
//  Matt Horsley, Yale University
//
// History:
//
/////////////////////////////////////

// include files included with the standard StAnalysisMaker
#include "StChain.h"
#include "StRun.h"
#include "StEvent.h"
#include "StGlobalTrack.h"
#include "StPhysicalHelixD.hh"
#include "SystemOfUnits.h"
#include "StThreeVectorD.hh"
#include "TString.h"
#include "StAngleCorrFunction.h"
#include "StTrackCuts.h"

// StMakers
#include "StAngleCorrMaker.h"
#include "StTrackForPool.h"
#include "StAngleCorrAnalysis.h"
#include "StAngleCorrAnalysisManager.h"

static const char rcsid[] = "$Id: StAngleCorrMaker.cxx,v 1.10 1999/12/29 01:55:45 horsley Exp $";

Int_t
StAngleCorrMaker::Make() 
{
  StEvent* mEvent;
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (! mEvent) return kStOK; // If no event, we're done
  StEvent& ev = *mEvent;
  corrAnalysis.ProcessEvent(ev);
  return kStOK;
}


StAngleCorrMaker::StAngleCorrMaker(const Char_t *name) : StMaker(name) 
{
  track1     = "track1";
  track2     = "track2";
  drawinit = kFALSE;
}

StAngleCorrMaker::~StAngleCorrMaker() {}

Int_t
StAngleCorrMaker::Init()
{
  return StMaker::Init();
}

void
StAngleCorrMaker::AddAnalysis(TString analysisName)
{
  corrAnalysis.AddAnalysis(analysisName);
}

void
StAngleCorrMaker::SetCorrelationFunction(TString analysisName, TString functionName)
{
  corrAnalysis.GetAnalysis(analysisName)->SetCorrelationFunction(functionName);
}

void
StAngleCorrMaker::SetMomentumCutsTrack1(TString analysisName, double lowerCut, double upperCut)
{
  
  corrAnalysis.GetAnalysis(analysisName)->GetTrackCuts(track1)->SetMomentumCuts(lowerCut,upperCut);
}

void
StAngleCorrMaker::SetMomentumCutsTrack2(TString analysisName, double lowerCut, double upperCut)
{
  corrAnalysis.GetAnalysis(analysisName)->GetTrackCuts(track2)->SetMomentumCuts(lowerCut,upperCut);
}

void
StAngleCorrMaker::SetPtCutsTrack1(TString analysisName, double lowerCut, double upperCut)
{
  corrAnalysis.GetAnalysis(analysisName)->GetTrackCuts(track1)->SetPtCuts(lowerCut,upperCut);
}

void
StAngleCorrMaker::SetPtCutsTrack2(TString analysisName, double lowerCut, double upperCut)
{
  corrAnalysis.GetAnalysis(analysisName)->GetTrackCuts(track2)->SetPtCuts(lowerCut,upperCut);
}


void
StAngleCorrMaker::SetChargeTrack1(TString analysisName, Int_t c)
{
  corrAnalysis.GetAnalysis(analysisName)->GetTrackCuts(track1)->SetTrackCharge(c);
}

void
StAngleCorrMaker::SetChargeTrack2(TString analysisName, Int_t c)
{
  corrAnalysis.GetAnalysis(analysisName)->GetTrackCuts(track2)->SetTrackCharge(c);
}


void
StAngleCorrMaker::SetRapidityCutsTrack1(TString analysisName, double lowerCut, double upperCut)
{
  corrAnalysis.GetAnalysis(analysisName)->GetTrackCuts(track1)->SetPseudoRapidityCuts(lowerCut,upperCut);
}

void
StAngleCorrMaker::SetRapidityCutsTrack2(TString analysisName, double lowerCut, double upperCut)
{
  corrAnalysis.GetAnalysis(analysisName)->GetTrackCuts(track2)->SetPseudoRapidityCuts(lowerCut,upperCut);
}


void 
StAngleCorrMaker::SetFastestTrackAnalysis(TString analysisName, int fastAnalysis)
{
  corrAnalysis.GetAnalysis(analysisName)->SetFastestTrackAnalysis(fastAnalysis);
}

void 
StAngleCorrMaker::SetSignalHist(TString analysisName, TH1D* sHist)
{
  corrAnalysis.GetAnalysis(analysisName)->SetSignalHist(sHist);
}

void 
StAngleCorrMaker::SetBackgroundHist(TString analysisName, TH1D* bHist)
{
  corrAnalysis.GetAnalysis(analysisName)->SetBackgroundHist(bHist);
}


void
StAngleCorrMaker::Clear(Option_t *opt)
{
    StMaker::Clear();
}

Int_t
StAngleCorrMaker::Finish() 
{
  return kStOK;
}



ClassImp(StAngleCorrMaker)







