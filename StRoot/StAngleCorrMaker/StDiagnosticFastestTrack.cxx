#include "StDiagnosticFastestTrack.h"
#include <TH1.h>
#include "StTrackForPool.h"

StDiagnosticFastestTrack::StDiagnosticFastestTrack() 
{
  fasttrackNTPC  = new TH1D("fasttrackNTPC","FastTrack:Number of TPC Hits",100,0,100);
  fasttrackMom  = new TH1D("fasttrackMom","FastTrack:Momentum",1000,0,100);
  fasttrackCh  = new TH1D("fasttrackCh","FastTrack:Charge",11,-5,5);
}

StDiagnosticFastestTrack::~StDiagnosticFastestTrack() 
{
}

void 
StDiagnosticFastestTrack::Write() 
{
  fasttrackNTPC->Write();
  fasttrackMom->Write();
  fasttrackCh->Write();
}

void 
StDiagnosticFastestTrack::Fill(StTrackForPool* t) 
{
  fasttrackNTPC ->Fill(t->GetNTPCPoints());
  
  Double_t mom;
  t->GetMomentum(mom);
  fasttrackMom ->Fill(mom);
  
  Int_t ch = t->GetCharge();
  fasttrackCh ->Fill(ch);
}

TString
StDiagnosticFastestTrack::GetName() 
{
  TString name = "DiagnoseFastestTrack";
  return name;
}
