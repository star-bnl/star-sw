#ifndef StAngleCorrAnalysisManager_HH
#define StAngleCorrAnalysisManager_HH

#include <vector>
#include "StAngleCorrAnalysis.h"

class StEvent;

class StAngleCorrAnalysisManager {

private:
  
  vector<StAngleCorrAnalysis*> vec;
  int mNumberOfTracksInPool,mNumberOfEventsInPool;
  
public:
  
  StAngleCorrAnalysisManager();
  ~StAngleCorrAnalysisManager();
  
  void ProcessEvent(StEvent& ev);
  void DoEvents(StEvent& ev);
  void DoSignals(); 
  void DoBackgrounds();
  void AddAnalysis(TString analysisName);
  void WriteDiagnostic();
  StAngleCorrAnalysis* GetAnalysis(TString analysisName); 
  
};

#endif
