#include "StDiagnosticSignal.h"
#include <TH1.h>
#include "StTrackForPool.h"


StDiagnosticSignal::StDiagnosticSignal() 
{
  signalCut  = new TH1D("signalCut","Signal Cuts: cut1",100,0,500);
}

StDiagnosticSignal::~StDiagnosticSignal() 
{
}

void 
StDiagnosticSignal::Write()
{
   signalCut->Write();
}

void 
StDiagnosticSignal::Fill(StTrackForPool* t1, StTrackForPool* t2) 
{
  // do nothing here for now
}

TString
StDiagnosticSignal::GetName() 
{
  TString name = "DiagnoseSignal";
  return name;
}
