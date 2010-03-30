#ifndef STUCMJOBS_H
#define STUCMJOBS_H

#include "StUcmJob.h"
namespace TxLogging {
class Iterator;
class StUcmJobs : public StUcmJob {
public: 
   StUcmJobs();
   virtual ~StUcmJobs();
   RecordList &getJobs();   
   const RecordList &getJobs() const;
   Iterator jobIterator();
};
}
#endif
