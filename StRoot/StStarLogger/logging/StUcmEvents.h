#ifndef STUCMEVENTS_H
#define STUCMEVENTS_H

#include "StUcmEvent.h"
namespace TxLogging {
class Iterator;
class StUcmEvents : public StUcmEvent {
public: 
   StUcmEvents();
   virtual ~StUcmEvents();
   RecordList &getEvents();
   const RecordList &getEvents() const;
   Iterator eventIterator();
};
}
#endif
