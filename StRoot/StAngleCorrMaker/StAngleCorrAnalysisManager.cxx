#include "StAngleCorrAnalysisManager.h"

StAngleCorrAnalysisManager::StAngleCorrAnalysisManager() {}
StAngleCorrAnalysisManager::~StAngleCorrAnalysisManager() {}

void
StAngleCorrAnalysisManager::ProcessEvent(StEvent& ev) 
{
  DoEvents(ev);
  DoSignals();
  DoBackgrounds();    
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
StAngleCorrAnalysisManager::WriteDiagnostic() 
{
  uint j;
  for (j=0;j<vec.size();j++) {vec[j]->WriteDiagnostic();}
  return;
}
