#include "StDiagnosticTrack2.h"
#include <TH1.h>
#include "StTrackForPool.h"

StDiagnosticTrack2::StDiagnosticTrack2() 
{
  track2NTPC  = new TH1D("track2NTPC","Track2:Number of TPC Hits",100,0,100);
  track2Mom  = new TH1D("track2Mom","Track2:Momentum",1000,0,100);
  track2Ch  = new TH1D("track2Ch","Track2:Charge",11,-5,5);
}

StDiagnosticTrack2::~StDiagnosticTrack2() 
{
}

void 
StDiagnosticTrack2::Write() 
{
  track2NTPC->Write();
  track2Mom->Write();
  track2Ch->Write();
}

void 
StDiagnosticTrack2::Fill(StTrackForPool* t) 
{
  track2NTPC ->Fill(t->GetNTPCPoints());
  
  Double_t mom;
  t->GetMomentum(mom);
  track2Mom ->Fill(mom);
  
  Int_t ch = t->GetCharge();
  track2Ch ->Fill(ch);
}

TString
StDiagnosticTrack2::GetName() 
{
  TString name = "DiagnoseTrack2";
  return name;
}
