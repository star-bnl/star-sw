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
  StMaker* mTheMcEventMaker;      //! this is the chain where the StEventReaderMaker is

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
  void SetTheMcEventMaker(StMaker*); 
  StMaker* TheMcEventMaker();

#ifdef __ROOT__  
  ClassDef(StHbtMcEventReader, 1)
#endif
};
    
inline void StHbtMcEventReader::SetTheMcEventMaker(StMaker* mcMaker){mTheMcEventMaker=mcMaker;}
inline StMaker* StHbtMcEventReader::TheMcEventMaker(){return mTheMcEventMaker;}

#endif
