#include "StDiagnosticTool.h"
#include <TNtuple.h>

StDiagnosticTool::StDiagnosticTool() {}
StDiagnosticTool::~StDiagnosticTool() {}

void 
StDiagnosticTool::Fill(StEvent& ev) 
{
  //  ntuple->Fill(ev.vertexCollection()->size(),ev.trackCollection()->size() ,ev.tpcHitCollection()->size());
}


TString
StDiagnosticTool::GetName() 
{
  TString name = "DiagnosticTool";
  return name;
}
