#include "StDiagnosticTracks.h"
#include <TH1.h>
#include "StTrackForPool.h"

StDiagnosticTracks::StDiagnosticTracks() 
{
  tracksNTPC  = new TH1D("tracksNTPC","Tracks:Number of TPC Hits",100,0,100);
  tracksMom  = new TH1D("tracksMom","Tracks:Momentum",1000,0,100);
  tracksCh  = new TH1D("tracksCh","Tracks:Charge",11,-5,5);
}

StDiagnosticTracks::~StDiagnosticTracks() 
{
}

void 
StDiagnosticTracks::Write() 
{
  tracksNTPC->Write();
  tracksMom->Write();
  tracksCh->Write();
}

void 
StDiagnosticTracks::Fill(StTrackForPool* t) 
{
  tracksNTPC ->Fill(t->GetNTPCPoints());
  
  Double_t mom;
  t->GetMomentum(mom);
  tracksMom ->Fill(mom);
  
  Int_t ch = t->GetCharge();
  tracksCh ->Fill(ch);
}

TString
StDiagnosticTracks::GetName() 
{
  TString name = "DiagnoseTracks";
  return name;
}
