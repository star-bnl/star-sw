
#include "StHbtMaker/Infrastructure/StHbtCutMonitorHandler.h"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

#ifdef __ROOT__
ClassImp(StHbtCutMonitorHandler)
#endif
// ---------------------------------------------------------------------------
StHbtCutMonitorHandler::StHbtCutMonitorHandler() {
  cout << " *** StHbtCutMonitorHandler::StHbtCutMonitorHandler() " << endl;
  mCollectionsEmpty = 0;
  mPassColl = new StHbtCutMonitorCollection();
  mFailColl = new StHbtCutMonitorCollection();
}
// ---------------------------------------------------------------------------
StHbtCutMonitorHandler::~StHbtCutMonitorHandler() { 
  delete mPassColl;
  delete mFailColl;
}   
// ---------------------------------------------------------------------------
void StHbtCutMonitorHandler::FillCutMonitor(const StHbtEvent* event, bool pass) { 
  if (mCollectionsEmpty) return;
  StHbtCutMonitorIterator iter;
  StHbtCutMonitor* CM;
  if ( pass) {
    for (iter=mPassColl->begin(); iter!=mPassColl->end(); iter++){
      CM = *iter;
      CM->Fill(event);
    }
  } else {
    for (iter=mFailColl->begin(); iter!=mFailColl->end(); iter++){
      CM = *iter;
      CM->Fill(event);
    }
  }
}
// ---------------------------------------------------------------------------
void StHbtCutMonitorHandler::FillCutMonitor(const StHbtTrack* track, bool pass) { 
  if (mCollectionsEmpty) return;
  StHbtCutMonitorIterator iter;
  StHbtCutMonitor* CM;
  if ( pass) {
    for (iter=mPassColl->begin(); iter!=mPassColl->end(); iter++){
      CM = *iter;
      CM->Fill(track);
    }
  } else {
    for (iter=mFailColl->begin(); iter!=mFailColl->end(); iter++){
      CM = *iter;
      CM->Fill(track);
    }
  }
}
// ---------------------------------------------------------------------------
void StHbtCutMonitorHandler::FillCutMonitor(const StHbtV0* v0, bool pass) { 
  if (mCollectionsEmpty) return;
  StHbtCutMonitorIterator iter;
  StHbtCutMonitor* CM;
  if ( pass) {
    for (iter=mPassColl->begin(); iter!=mPassColl->end(); iter++){
      CM = *iter;
      CM->Fill(v0);
    }
  } else {
    for (iter=mFailColl->begin(); iter!=mFailColl->end(); iter++){
      CM = *iter;
      CM->Fill(v0);
    }
  }
}
// ---------------------------------------------------------------------------
void StHbtCutMonitorHandler::FillCutMonitor(const StHbtKink* kink, bool pass) { 
  if (mCollectionsEmpty) return;
  StHbtCutMonitorIterator iter;
  StHbtCutMonitor* CM;
  if ( pass) {
    for (iter=mPassColl->begin(); iter!=mPassColl->end(); iter++){
      CM = *iter;
      CM->Fill(kink);
    }
  } else {
    for (iter=mFailColl->begin(); iter!=mFailColl->end(); iter++){
      CM = *iter;
      CM->Fill(kink);
    }
  }
}
// ---------------------------------Gael/12/04/02-----------------------------
void StHbtCutMonitorHandler::FillCutMonitor(const StHbtPair* pair, bool pass) { 
  if (mCollectionsEmpty) return;
  StHbtCutMonitorIterator iter;
  StHbtCutMonitor* CM;
  if ( pass) {
    for (iter=mPassColl->begin(); iter!=mPassColl->end(); iter++){
      CM = *iter;
      CM->Fill(pair);
    }
  } else {
    for (iter=mFailColl->begin(); iter!=mFailColl->end(); iter++){
      CM = *iter;
      CM->Fill(pair);
    }
  }
}
// ---------------------------------Gael/19/06/02-----------------------------
void StHbtCutMonitorHandler::FillCutMonitor(const StHbtParticleCollection* partColl) {
  if (mCollectionsEmpty) return;
  StHbtCutMonitorIterator iter;
  StHbtCutMonitor* CM;
  
  for (iter=mPassColl->begin(); iter!=mPassColl->end(); iter++){
    CM = *iter;
    CM->Fill(partColl);
  }
}
// ------------------------------------Gael/19/06/02-------------------------
void StHbtCutMonitorHandler::FillCutMonitor(const StHbtEvent* event,const StHbtParticleCollection* partColl) {
  
  cout<<"In StHbtCutMonitorHandler::FillCutMonitor(const StHbtEvent* event, StHbtPicoEvent* picoEvent)"<<endl;
  if (mCollectionsEmpty) return;
  StHbtCutMonitorIterator iter;
  StHbtCutMonitor* CM;
  
  for (iter=mPassColl->begin(); iter!=mPassColl->end(); iter++){
    CM = *iter;
    CM->Fill(event,partColl);
  }
}
// ---------------------------------------------------------------------------
void StHbtCutMonitorHandler::Finish() { 
  StHbtCutMonitorIterator iter;
  for (iter=mPassColl->begin(); iter!=mPassColl->end(); iter++){
    (*iter)->Finish();
  }
  for (iter=mFailColl->begin(); iter!=mFailColl->end(); iter++){
    (*iter)->Finish();
  }
}
// ---------------------------------------------------------------------------
void StHbtCutMonitorHandler::AddCutMonitor(StHbtCutMonitor* cutMoni1, StHbtCutMonitor* cutMoni2) { 
  mPassColl->push_back(cutMoni1);
  mFailColl->push_back(cutMoni2);
  mCollectionsEmpty=false;
}
// ---------------------------------------------------------------------------
void StHbtCutMonitorHandler::AddCutMonitor(StHbtCutMonitor* cutMoni) { 
  cout << " make a copy of the cutmonitor and push both into the collections " << endl;
  cout << " not yet implemented" << endl;
  mPassColl->push_back(cutMoni);
  cout << " only pass collection pushed" << endl;
  mCollectionsEmpty=false;
}
// ---------------------------------------------------------------------------
void StHbtCutMonitorHandler::AddCutMonitorPass(StHbtCutMonitor* cutMoni) { 
  mPassColl->push_back(cutMoni);
  mCollectionsEmpty=false;
}
// ---------------------------------------------------------------------------
void StHbtCutMonitorHandler::AddCutMonitorFail(StHbtCutMonitor* cutMoni) { 
  mFailColl->push_back(cutMoni);
  mCollectionsEmpty=false;
}
// ---------------------------------------------------------------------------
StHbtCutMonitor* StHbtCutMonitorHandler::PassMonitor(int n) { 
  StHbtCutMonitorIterator iter = mPassColl->begin();
  if ( (int)mPassColl->size() <= n ) return NULL;
  for ( int i=0; i<n; i++)
    iter++;
  return *iter;
}
// ---------------------------------------------------------------------------
StHbtCutMonitor* StHbtCutMonitorHandler::FailMonitor(int n) { 
  StHbtCutMonitorIterator iter = mFailColl->begin();
  if ( (int)mFailColl->size() <= n ) return NULL;
  for ( int i=0; i<n; i++)
    iter++;
  return *iter;
}



 
 
