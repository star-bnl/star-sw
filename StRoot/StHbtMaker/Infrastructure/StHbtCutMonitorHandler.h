#ifndef StHbtCutMonitorHandler_hh
#define StHbtCutMonitorHandler_hh


#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Infrastructure/StHbtV0.hh"
#include "StHbtMaker/Infrastructure/StHbtCutMonitorCollection.hh"
#include "StHbtMaker/Base/StHbtCutMonitor.hh"

class StHbtCutMonitorHandler{
  
 public:
  
  StHbtCutMonitorHandler();
  virtual ~StHbtCutMonitorHandler();
  
  StHbtCutMonitorCollection* PassMonitorColl(); 
  StHbtCutMonitorCollection* FailMonitorColl(); 
  StHbtCutMonitor* PassMonitor(int n); 
  StHbtCutMonitor* FailMonitor(int n); 
  void AddCutMonitor(StHbtCutMonitor* cutMoni1, StHbtCutMonitor* cutMoni2); 
  void AddCutMonitor(StHbtCutMonitor* cutMoni); 
  void AddCutMonitorPass(StHbtCutMonitor* cutMoni); 
  void AddCutMonitorFail(StHbtCutMonitor* cutMoni); 
  void FillCutMonitor(const StHbtEvent* event, bool pass); 
  void FillCutMonitor(const StHbtTrack* track, bool pass); 
  void FillCutMonitor(const StHbtV0* v0, bool pass); 
  void Finish();
  
 private:
  bool mCollectionsEmpty;
  StHbtCutMonitorCollection* mPassColl; 
  StHbtCutMonitorCollection* mFailColl; 
#ifdef __ROOT__  
  ClassDef(StHbtCutMonitorHandler, 0)
#endif  
  
};

inline StHbtCutMonitorCollection* StHbtCutMonitorHandler::PassMonitorColl() { return mPassColl;}
inline StHbtCutMonitorCollection* StHbtCutMonitorHandler::FailMonitorColl() { return mFailColl;}

#endif
