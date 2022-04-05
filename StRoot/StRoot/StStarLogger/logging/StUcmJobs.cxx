#include "StUcmJobs.h"
#include "StRecordIterator.h"
using namespace TxLogging;
//__________________________________________________
StUcmJobs::StUcmJobs()  {;}
//__________________________________________________
StUcmJobs::~StUcmJobs() {;}
//__________________________________________________
const RecordList &StUcmJobs::getJobs() const
{   return getRecords(); }
//__________________________________________________
RecordList &StUcmJobs::getJobs()
{   return getRecords(); }

//__________________________________________________
Iterator StUcmJobs::jobIterator()
{   return getJobs().iterator();   }
