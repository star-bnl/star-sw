#include "StAngleCorrMaker.h"

StAngleCorrMaker::StAngleCorrMaker(const Char_t *name) : StMaker(name) 
{  
  track1   = "track1";
  track2   = "track2";
  
  drawinit = kFALSE; 
}

StAngleCorrMaker::~StAngleCorrMaker() {}

Int_t 
StAngleCorrMaker::Init() {return StMaker::Init();}

Int_t 
StAngleCorrMaker::Make() 
{
  
  StEvent* mEvent;
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (! mEvent) return kStOK; 
  StEvent& ev = *mEvent;

  corrAnalysis.ProcessEvent(ev);   
  
  return kStOK;
}

void 
StAngleCorrMaker::Clear(Option_t *opt) { StMaker::Clear();}

Int_t 
StAngleCorrMaker::Finish() 
{  
  corrAnalysis.WriteDiagnostic();
  return kStOK;
}

ClassImp(StAngleCorrMaker)


void StAngleCorrMaker::AddAnalysis(TString analysisName) {corrAnalysis.AddAnalysis(analysisName);}

void StAngleCorrMaker::SetMomentumCutsTrack1(TString analysisName, double lowerCut, double upperCut) 
{ corrAnalysis.GetAnalysis(analysisName)->GetTrackCuts(track1)->SetMomentumCuts(lowerCut,upperCut);}

void StAngleCorrMaker::SetMomentumCutsTrack2(TString analysisName, double lowerCut, double upperCut)
{corrAnalysis.GetAnalysis(analysisName)->GetTrackCuts(track2)->SetMomentumCuts(lowerCut,upperCut);}

void StAngleCorrMaker::SetPtCutsTrack1(TString analysisName, double lowerCut, double upperCut)
{corrAnalysis.GetAnalysis(analysisName)->GetTrackCuts(track1)->SetPtCuts(lowerCut,upperCut);}

void StAngleCorrMaker::SetPtCutsTrack2(TString analysisName, double lowerCut, double upperCut)
{ corrAnalysis.GetAnalysis(analysisName)->GetTrackCuts(track2)->SetPtCuts(lowerCut,upperCut);}

void StAngleCorrMaker::SetChargeTrack1(TString analysisName, Int_t c) { corrAnalysis.GetAnalysis(analysisName)->GetTrackCuts(track1)->SetTrackCharge(c);}

void StAngleCorrMaker::SetChargeTrack2(TString analysisName, Int_t c)
{ corrAnalysis.GetAnalysis(analysisName)->GetTrackCuts(track2)->SetTrackCharge(c);}

void StAngleCorrMaker::SetRapidityCutsTrack1(TString analysisName, double lowerCut, double upperCut)
{ corrAnalysis.GetAnalysis(analysisName)->GetTrackCuts(track1)->SetPseudoRapidityCuts(lowerCut,upperCut);}

void StAngleCorrMaker::SetRapidityCutsTrack2(TString analysisName, double lowerCut, double upperCut)
{ corrAnalysis.GetAnalysis(analysisName)->GetTrackCuts(track2)->SetPseudoRapidityCuts(lowerCut,upperCut);}

void StAngleCorrMaker::SetMultiplicityCuts(TString analysisName, double lowerCut, double upperCut)
{ corrAnalysis.GetAnalysis(analysisName)->GetEventCuts()->SetMultiplicityCuts(lowerCut,upperCut);}

void StAngleCorrMaker::SetNBackgroundPairs(TString analysisName, int number)
{corrAnalysis.GetAnalysis(analysisName)->SetNBackgroundPairs(number);}

void StAngleCorrMaker::SetNBackgroundEvents(TString analysisName, int number)
{corrAnalysis.GetAnalysis(analysisName)->SetNBackgroundEvents(number);}

void StAngleCorrMaker::SetFastestTrackAnalysis(TString analysisName, int fastAnalysis)
{corrAnalysis.GetAnalysis(analysisName)->SetFastestTrackAnalysis(fastAnalysis);}

void StAngleCorrMaker::SetSignalHist(TString analysisName, TH1D* sHist)
{ corrAnalysis.GetAnalysis(analysisName)->SetSignalHist(sHist);}

void StAngleCorrMaker::SetBackgroundHist(TString analysisName, TH1D* bHist)
{ corrAnalysis.GetAnalysis(analysisName)->SetBackgroundHist(bHist);}

void StAngleCorrMaker::SetDiagnosticsON(TString analysisName)
{ corrAnalysis.GetAnalysis(analysisName)->SetDiagnosticsON();}


void StAngleCorrMaker::SetCorrelationFunction(TString analysisName, TString functionName)
{corrAnalysis.GetAnalysis(analysisName)->SetCorrelationFunction(functionName);}



