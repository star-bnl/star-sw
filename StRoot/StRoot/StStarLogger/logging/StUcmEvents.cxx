#include "StUcmEvents.h"
#include "StRecordIterator.h"
using namespace TxLogging;
StUcmEvents::StUcmEvents()  {;}
StUcmEvents::~StUcmEvents() {;}
const RecordList &StUcmEvents::getEvents() const
{   return getRecords(); }
RecordList &StUcmEvents::getEvents()
{   return getRecords(); }
Iterator StUcmEvents::eventIterator()
{   return getEvents().iterator(); }


