// Cons tries to include even if there is a  '#ifdef #endif' around
// For that reson I had to change all '#include' statements into
// '//(notTheY2KBuf)#include' 
#undef McEventExists
#ifdef McEventExists

#ifndef StHbtMcEventReader_hh
#define StHbtMcEventReader_hh

//(notTheY2KBug)#include <ctime>
//(notTheY2KBug)#include "StMaker.h"
//(notTheY2KBug)#include "StHbtMaker/Base/StHbtEventReader.hh"
//(notTheY2KBug)#include "StV0MiniDstMaker/StV0MiniDstMaker.h"
//(notTheY2KBug)#include "StHbtMaker/Base/StHbtEventCut.h"
//(notTheY2KBug)#include "StHbtMaker/Base/StHbtTrackCut.h"
//(notTheY2KBug)#include "StHbtMaker/Base/StHbtV0Cut.h"

class TOrdCollection;
class StHbtMcEventReader : public StHbtEventReader{
  
 private:
  // pointers to other makers
  StMaker* mTheMcEventMaker;      //! this is the chain where the StEventReaderMaker is
  StV0MiniDstMaker* mTheV0Maker;  //! this is the chain where the StV0MiniDstMaker is

  // pointers to front-loaded cuts
  StHbtEventCut* mEventCut; //!
  StHbtTrackCut* mTrackCut; //!
  StHbtV0Cut*    mV0Cut;    //!

  long              mV0;        //! Number of v0s looked at to date
  time_t timeStamp; // to display the time/event

 protected:
  TOrdCollection *mCollection; //!
  
 public:
  StHbtMcEventReader();
  ~StHbtMcEventReader();
  
  virtual StHbtEvent* ReturnHbtEvent();
  virtual StHbtString Report();
  
  // sets and gets for the other makers
  void SetTheMcEventMaker(StMaker*); 
  void SetTheV0Maker(StV0MiniDstMaker*);
  StMaker* TheMcEventMaker();
  StV0MiniDstMaker* TheV0Maker();

  // sets and gets for the front-loaded cuts
  void SetEventCut(StHbtEventCut*);
  void SetTrackCut(StHbtTrackCut*);
  void SetV0Cut(StHbtV0Cut*);
  StHbtEventCut* EventCut();
  StHbtTrackCut* TrackCut();
  StHbtV0Cut*    V0Cut();

  //  ClassDef(StHbtMcEventReader, 1)
};
    
inline void StHbtMcEventReader::SetTheMcEventMaker(StMaker* mcMaker){mTheMcEventMaker=mcMaker;}
inline void StHbtMcEventReader::SetTheV0Maker(StV0MiniDstMaker* maker){mTheV0Maker=maker;}
inline StMaker* StHbtMcEventReader::TheMcEventMaker(){return mTheMcEventMaker;}
inline StV0MiniDstMaker* StHbtMcEventReader::TheV0Maker(){return mTheV0Maker;}

inline void StHbtMcEventReader::SetEventCut(StHbtEventCut* ecut){mEventCut=ecut;}
inline void StHbtMcEventReader::SetTrackCut(StHbtTrackCut* tcut){mTrackCut=tcut;}
inline void StHbtMcEventReader::SetV0Cut(StHbtV0Cut* v0cut){mV0Cut=v0cut;}
inline StHbtEventCut* StHbtMcEventReader::EventCut(){return mEventCut;}
inline StHbtTrackCut* StHbtMcEventReader::TrackCut(){return mTrackCut;}
inline StHbtV0Cut*    StHbtMcEventReader::V0Cut(){return mV0Cut;}

#endif

#else // McEventExists
class StHbtMcEventReader {
  int fake() { return 0;}
};
#endif
