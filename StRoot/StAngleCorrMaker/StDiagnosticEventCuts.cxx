#include "StDiagnosticEventCuts.h"
#include <TH1.h>
#include "StEvent.h"

StDiagnosticEventCuts::StDiagnosticEventCuts() 
{
  evcutMULT  = new TH1D("evcutMULT","Event Cuts:Multiplicity",1000,0,50000);
}

StDiagnosticEventCuts::~StDiagnosticEventCuts() 
{
  evcutMULT ->Write();
  cout << "wrote event cuts diagnostic hist's ..." << endl;
}

void 
StDiagnosticEventCuts::Write()
{
  evcutMULT->Write();
}

void 
StDiagnosticEventCuts::Fill(StEvent& ev) 
{
  evcutMULT->Fill(ev.trackCollection()->size() );

}

TString
StDiagnosticEventCuts::GetName() 
{
  TString name = "DiagnoseEventCuts";
  return name;
}
