#include "StAcceptAllEvents.h"
#include "StEventTypes.h"
  ClassImp(StAcceptAllEvents)
int StAcceptAllEvents::Accept(StEvent* event){return 1;}
void StAcceptAllEvents::Report(){cout << "StAcceptAllEvents: No track cuts for uDST" << endl;}
