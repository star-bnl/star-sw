#include "StUcmJob.h"
using namespace TxLogging;
StUcmJob::StUcmJob()  {;}
StUcmJob::~StUcmJob() {;}
const FieldList &StUcmJob::getFields() const
{
   return StRecord::getFields();
}

