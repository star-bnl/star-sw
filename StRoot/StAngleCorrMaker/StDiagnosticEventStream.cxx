#include "StDiagnosticEventStream.h"
#include <TH1.h>


StDiagnosticEventStream::StDiagnosticEventStream() 
{
   evstreamMULT  = new TH1D("evstreamMULT","Event Stream:Multiplicity",1000,0,50000);
}

StDiagnosticEventStream::~StDiagnosticEventStream() 
{
}

void 
StDiagnosticEventStream::Write() 
{
  evstreamMULT->Write();
}

void 
StDiagnosticEventStream::Fill(StEvent& ev) 
{
  // evstreamMULT->Fill(ev.trackCollection()->size() );
}

TString
StDiagnosticEventStream::GetName() 
{
  TString name = "DiagnoseEventStream";
  return name;
}
