#ifndef STUCMEVENT_H
#define STUCMEVENT_H

#include "StRecord.h"

namespace TxLogging {
class StUcmEvent : public StRecord {
public: 
   StUcmEvent();
   virtual ~StUcmEvent();
   const FieldList &getFields() const;
   FieldList &getFields();
};
}
#endif
