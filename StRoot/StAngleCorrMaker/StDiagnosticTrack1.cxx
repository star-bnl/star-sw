#include "StDiagnosticTrack1.h"
#include <TH1.h>
#include "StTrackForPool.h"

StDiagnosticTrack1::StDiagnosticTrack1() 
{
  track1NTPC  = new TH1D("track1NTPC","Track1:Number of TPC Hits",100,0,100);
  track1Mom  = new TH1D("track1Mom","Track1:Momentum",1000,0,100);
  track1Ch  = new TH1D("track1Ch","Track1:Charge",11,-5,5);
}

StDiagnosticTrack1::~StDiagnosticTrack1() 
{
}

void 
StDiagnosticTrack1::Write() 
{
  track1NTPC->Write();
  track1Mom->Write();
  track1Ch->Write();
}

void 
StDiagnosticTrack1::Fill(StTrackForPool* t) 
{
  track1NTPC ->Fill(t->GetNTPCPoints());
  
  Double_t mom;
  t->GetMomentum(mom);
  track1Mom ->Fill(mom);
  
  Int_t ch = t->GetCharge();
  track1Ch ->Fill(ch);
}

TString
StDiagnosticTrack1::GetName() 
{
  TString name = "DiagnoseTrack1";
  return name;
}
