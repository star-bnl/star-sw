#ifndef StHbtCutMonitorHandler_hh
#define StHbtCutMonitorHandler_hh


#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Infrastructure/StHbtV0.hh"
#include "StHbtMaker/Infrastructure/StHbtKink.hh"
#include "StHbtMaker/Infrastructure/StHbtPair.hh" //Gael 12/04/02
#include "StHbtMaker/Infrastructure/StHbtParticleCollection.hh" // Gael 19/06/02
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
  void FillCutMonitor(const StHbtKink* kink, bool pass);
  void FillCutMonitor(const StHbtPair* pair, bool pass);//Gael 11/04/02
  void FillCutMonitor(const StHbtParticleCollection* partColl);// Gael 19/06/02
  void FillCutMonitor(const StHbtEvent* event, const StHbtParticleCollection* partColl);// Gael 19/06/02
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
