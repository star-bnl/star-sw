#ifndef StAngleCorrAnalysisManager_HH
#define StAngleCorrAnalysisManager_HH

#include <vector>
#include "StAngleCorrAnalysis.h"

class StEvent;

class StAngleCorrAnalysisManager {

private:
#ifdef ST_NO_TEMPLATE_DEF_ARGS
  vector<StAngleCorrAnalysis*, allocator<StAngleCorrAnalysis*> > vec;
#else
  vector<StAngleCorrAnalysis*> vec;
#endif
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
