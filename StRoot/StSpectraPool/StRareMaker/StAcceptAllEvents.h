#ifndef STACCEPTALLEVENTS_HH
#define STACCEPTALLEVENTS_HH
#include "StRareEventCut.h"
#include <Stiostream.h>
class StEvent;
class StAcceptAllEvents : public StRareEventCut {
 
 public:
  StAcceptAllEvents(){};
  ~StAcceptAllEvents(){};
  int  Accept(StEvent* event);
  void Report();  
  ClassDef(StAcceptAllEvents,1)
};

#endif
