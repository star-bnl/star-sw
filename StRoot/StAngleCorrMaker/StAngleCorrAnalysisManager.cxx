#include "StAngleCorrAnalysisManager.h"
#include "StEvent.h"
#include "iostream.h"
#include "StAngleCorrAnalysis.h"
#include "TStopwatch.h"

StAngleCorrAnalysisManager::StAngleCorrAnalysisManager() 
{
  stopwatch =new TStopwatch();
}

StAngleCorrAnalysisManager::~StAngleCorrAnalysisManager() 
{
  if (stopwatch != NULL) delete stopwatch;
}

void
StAngleCorrAnalysisManager::AddAnalysis(TString analysisName)
{
  vec.push_back(new StAngleCorrAnalysis(analysisName));
}

StAngleCorrAnalysis*
StAngleCorrAnalysisManager::GetAnalysis(TString analysisName) 
{
  uint vecSize=vec.size();
  for (uint index=0;index<vecSize;index++) 
    {
      if (vec[index]->GetName()==analysisName) return vec[index]; 
    }
  return NULL;
}


void
StAngleCorrAnalysisManager::ProcessEvent(StEvent& ev) 
{
  cout << "starting event analysis ..." << endl;
  Double_t startTime = stopwatch->GetCPUTime();
  DoEvents(ev);
  DoSignals();
  DoBackgrounds();    
  cout << "run time: "  << stopwatch->GetCPUTime() - startTime << "  seconds " << endl << endl;
}


void
StAngleCorrAnalysisManager::DoEvents(StEvent& ev) 
{
  
  uint j;
  for (j=0;j<vec.size();j++) {vec[j]->ProcessEvent(ev);}
  return;
}


void
StAngleCorrAnalysisManager::DoSignals() 
{
  uint j;
  for (j=0;j<vec.size();j++) {vec[j]->AnalyseRealPairs();}
  return;
}

void
StAngleCorrAnalysisManager::DoBackgrounds() 
{
  uint j;
  for (j=0;j<vec.size();j++) {vec[j]->AnalyseBackgroundPairs();}
  return;
}


void
StAngleCorrAnalysisManager::WriteDiagnostic() 
{
  uint j;
  for (j=0;j<vec.size();j++) {vec[j]->WriteDiagnostic();}
  return;
}
