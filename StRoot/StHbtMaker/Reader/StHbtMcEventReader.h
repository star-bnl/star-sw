#ifndef StHbtMcEventReader_hh
#define StHbtMcEventReader_hh

#include <ctime>
#include "StMaker.h"
#include "StHbtMaker/Base/StHbtEventReader.hh"
#include "StV0MiniDstMaker/StV0MiniDstMaker.h"
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"

class TOrdCollection;
class StHbtMcEventReader : public StHbtEventReader{
  
 private:
  // pointers to other makers
  StMaker* mTheMcEventMaker;      //! this is the chain where the StEventReaderMaker is
  StV0MiniDstMaker* mTheV0Maker;  //! this is the chain where the StV0MiniDstMaker is

  long  mV0;        //! Number of v0s looked at to date
  time_t timeStamp; // to display the time/event

 protected:
  TOrdCollection *mCollection; //!
  
 public:
  StHbtMcEventReader();
  ~StHbtMcEventReader();
  
  StHbtEvent* ReturnHbtEvent();
  StHbtString Report();
  
  // sets and gets for the other makers
  void SetTheMcEventMaker(StMaker*); 
  void SetTheV0Maker(StV0MiniDstMaker*);
  StMaker* TheMcEventMaker();
  StV0MiniDstMaker* TheV0Maker();

#ifdef __ROOT__  
  ClassDef(StHbtMcEventReader, 1)
#endif
};
    
inline void StHbtMcEventReader::SetTheMcEventMaker(StMaker* mcMaker){mTheMcEventMaker=mcMaker;}
inline void StHbtMcEventReader::SetTheV0Maker(StV0MiniDstMaker* maker){mTheV0Maker=maker;}
inline StMaker* StHbtMcEventReader::TheMcEventMaker(){return mTheMcEventMaker;}
inline StV0MiniDstMaker* StHbtMcEventReader::TheV0Maker(){return mTheV0Maker;}

#endif
