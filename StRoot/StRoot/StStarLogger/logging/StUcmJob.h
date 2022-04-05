#ifndef STUCMJOB_H
#define STUCMJOB_H

#include "StRecord.h"
namespace TxLogging {
class StUcmJob : public StRecord {
public: 
   StUcmJob();
   virtual ~StUcmJob();
   const FieldList &getFields() const;
};
}
#endif
