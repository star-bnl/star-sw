#include "StDiagnosticBackground.h"
#include <TH1.h>
#include "StTrackForPool.h"


StDiagnosticBackground::StDiagnosticBackground() 
{
  backgroundCut  = new TH1D("backgroundCut","Background Cuts: cut1",100,0,500);
}

StDiagnosticBackground::~StDiagnosticBackground() 
{
}

void 
StDiagnosticBackground::Write()
{
   backgroundCut->Write();
}

void 
StDiagnosticBackground::Fill(StTrackForPool* t1, StTrackForPool* t2) 
{
  // do nothing here for now
}

TString
StDiagnosticBackground::GetName() 
{
  TString name = "DiagnoseBackground";
  return name;
}
