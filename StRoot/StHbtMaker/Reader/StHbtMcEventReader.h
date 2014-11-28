#ifndef StHbtMcEventReader_hh
#define StHbtMcEventReader_hh

#include <ctime>
#include "StMaker.h"
#include "StHbtMaker/Infrastructure/StHbtCheckPdgIdList.h"
#include "StHbtMaker/Base/StHbtEventReader.hh"
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"

class StHbtMcEventReader : public StHbtEventReader, public StHbtCheckPdgIdList {
  
 private:
  // pointers to other makers

  long  mV0;        //! Number of v0s looked at to date
  time_t timeStamp; // to display the time/event

  StHbt3DHisto* mMotherMinvYPt;
  StHbt3DHisto* mMotherMinvYMt;
  StHbt3DHisto* mMotherMinvEtaPt;

 protected:
  
 public:
  StHbtMcEventReader();
  ~StHbtMcEventReader();
  
  StHbtEvent* ReturnHbtEvent();
  StHbtString Report();
  
  // sets and gets for the other makers
  void SetTheMcEventMaker(StMaker*);       // NOTE! this is now obsolete, as we get the maker via "GetDataSet"
                                           // but I leave it in just to not break any macros - malisa 28sep2005
                                           // You can Set it if you want, but it just doesn't do anything

  StMaker* TheMcEventMaker();

#ifdef __ROOT__  
  ClassDef(StHbtMcEventReader, 1)
#endif
};
#endif
