#include "StDiagnosticTool.h"
#include <TNtuple.h>
#include "StTrackForPool.h"
#include "StAngleCorrFunction.h"

StDiagnosticTool::StDiagnosticTool() {}
StDiagnosticTool::~StDiagnosticTool() {}

void 
StDiagnosticTool::Fill(StEvent& ev) 
{

}

void 
StDiagnosticTool::Fill(StTrackForPool* t) 
{

}

void 
StDiagnosticTool::Fill(StTrackForPool* t1,  StTrackForPool* t2) 
{

}
void
StDiagnosticTool::Write()
{

}

void 
StDiagnosticTool::SetCorrelationFunction(StAngleCorrFunction* func)
{
  corrFunc = func;
}

TString
StDiagnosticTool::GetName() 
{
  TString name = "DiagnosticTool";
  return name;
}
