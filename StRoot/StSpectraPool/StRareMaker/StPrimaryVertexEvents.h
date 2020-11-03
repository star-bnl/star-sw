#ifndef STPrimaryVertexEVENTS_HH
#define STPrimaryVertexEVENTS_HH
#include "StRareEventCut.h"
#include <Stiostream.h>
class StEvent;
class StPrimaryVertexEvents : public StRareEventCut {
 
 public:
  StPrimaryVertexEvents(){};
  ~StPrimaryVertexEvents(){};
  int  Accept(StEvent* event);
  void Report();  
  ClassDef(StPrimaryVertexEvents,1)
};

#endif
