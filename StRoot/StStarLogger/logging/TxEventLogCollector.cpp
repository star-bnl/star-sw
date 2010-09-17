/*****************************************************************
 * @file TxEventLogFile.cpp
 * @author Valeri Fine
 *
 * @(#)cpp/api:$Id: TxEventLogCollector.cpp,v 1.8 2010/09/17 19:34:54 fine Exp $
 *
 * Please see TxEventLogFile.h for more documentation.
 *****************************************************************/

#include "TxEventLogCollector.h"
#include "TxUCMCollector.h"
#include "StUcmTasks.h"
#include "StUcmJobs.h"
#include "StUcmEvents.h"

#include <iostream>

#include <string>
#include <cassert>

using namespace TxLogging;
using namespace std;

//______________________________________________________________
TxEventLogCollector::TxEventLogCollector() :
fCollector(), fTasks(), fJobs(),fEvents(),fDbInit()
{
  cout << __FILE__ << " :: "<< __FUNCTION__ << " : " << __LINE__ <<endl;
  fCollector  = new TxUCMCollector;
}

//______________________________________________________________
TxEventLogCollector::~TxEventLogCollector ()
{
   delete fCollector; fCollector = 0;
   delete fTasks;     fTasks     = 0;
   delete fJobs;      fJobs      = 0;
   delete fEvents;    fEvents    = 0;
}
//______________________________________________________________
void TxEventLogCollector::InitDb()
{
   if (!fDbInit) { fCollector->initDb(); fDbInit=true;}
}

//______________________________________________________________
void TxEventLogCollector::writeDown(const std::string& message)
{
   
   InitDb();
   fCollector->processMessage(message);
}

//______________________________________________________________
StUcmTasks  *TxEventLogCollector::getTaskList (int limit, int offset)
{
   if (! fTasks) {
      InitDb();
      fTasks = new StUcmTasks;
   }
   fCollector->fillTaskList(*fTasks,limit,offset);
   return fTasks;
}
//______________________________________________________________
StUcmJobs   *TxEventLogCollector::getJobList  (StRecord *task,int limit, int offset)
{
   if (!fJobs) { 
      InitDb();
      fJobs  = new StUcmJobs;
   }
   fCollector->fillJobList(*fJobs,limit,offset,task);
   return fJobs;
}

//______________________________________________________________
int   TxEventLogCollector::getJobId(const char *reqName, const char *taskBrokerId, int jobBrokerId)
{
   if (! fEvents)  {
      InitDb();
      fEvents    = new StUcmEvents;
   }
   return fCollector->getJobId(reqName,taskBrokerId, jobBrokerId);
}

//______________________________________________________________
StUcmEvents *TxEventLogCollector::getEventList(StRecord *job, int limit, int offset)
{
   if (! fEvents)  {
      InitDb();
      fEvents    = new StUcmEvents;
   }
   fCollector->fillEventList(*fEvents,limit,offset,job);
   return fEvents;
}

//______________________________________________________________
void TxEventLogCollector::setBrokerTaskID (const std::string& brokerTaskID) 
{
   TxEventLogFile::setBrokerTaskID (brokerTaskID);
   fCollector->setBrokerTaskID(brokerTaskID);
}

//______________________________________________________________
void TxEventLogCollector::setBrokerJobID (int bJobID) {
   TxEventLogFile::setBrokerJobID (bJobID);
   fCollector->setBrokerJobID (bJobID);
}

//______________________________________________________________
void TxEventLogCollector::setRequesterName (const std::string& reqName) 
{
   TxEventLogFile::setRequesterName (reqName);
   fCollector->setRequesterName (reqName);
}

//______________________________________________________________
void TxEventLogCollector::setDbJobID(int bJobID)
{
   fCollector->setDbJobID(bJobID);
}

//______________________________________________________________
int  TxEventLogCollector::queryTableSize(const char *tableName, const char *where)
{
  InitDb();
  return fCollector->queryTableSize(tableName, where);
}

//______________________________________________________________
int  TxEventLogCollector::queryTableSize(const char *tableName, const StRecord   *where)
{
  InitDb();
  return fCollector->queryTableSize(tableName, where);
}


TXEVENT_DEFAULT_IMPLEMENTAION(TxEventLogCollector)
