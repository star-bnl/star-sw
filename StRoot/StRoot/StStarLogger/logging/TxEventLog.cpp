#include "TxEventLog.h"

using namespace TxLogging;
using namespace std;

//________________________________________________________________
TxEventLog::TxEventLog (){;}
//________________________________________________________________
TxEventLog::~TxEventLog (){;}
//________________________________________________________________
void TxEventLog::writeDown(const char *message)
{
    this->writeDown(string(message)); 
}

//________________________________________________________________
void TxEventLog::setEnvBrokerTaskID (const char *envBrokerTaskID)
{
   this->setEnvBrokerTaskID(string(envBrokerTaskID));
}
//________________________________________________________________
void TxEventLog::setEnvBrokerJobID (const char *envBrokerJobID)
{
   this->setEnvBrokerJobID(string(envBrokerJobID));
}
//________________________________________________________________
void TxEventLog::setBrokerTaskID (const char *brokerTaskID)
{
   this->setBrokerTaskID (string(brokerTaskID));
}
//________________________________________________________________
 void TxEventLog::setRequesterName (const char *requester)
{
   this->setRequesterName(string(requester));
}
//________________________________________________________________
void TxEventLog::setContext (const char *context)
{
   this->setContext (string(context));
}

//________________________________________________________________
 void TxEventLog::logStart (const char *key, const char *value)
{
    this-> logStart (string(key),string(value));
}
//________________________________________________________________
void TxEventLog::logJobAttribute (const char *key, const char *value)
{
   this->logJobAttribute(string(key),string(value));
}

//________________________________________________________________
void TxEventLog::logJobSubmitLocation (const char *url)
{
   this->logJobSubmitLocation(string(url));
}

//________________________________________________________________
void TxEventLog::logTask (const char *taskAttributes)
{
   this->logTask (string(taskAttributes));
}
 
//________________________________________________________________
void TxEventLog::setJobSubmitLocation (const char *url)
{
   this->setJobSubmitLocation (string(url));
}
//________________________________________________________________
void TxEventLog::logJobSubmitID (const char *ID)
{
   this->logJobSubmitID(string(ID));
}

//________________________________________________________________
void TxEventLog::setJobSubmitID (const char *ID)
{
   this->setJobSubmitID(string(ID));
}
//________________________________________________________________
void TxEventLog::logEvent (const char *logMsg, 
		   Level level, Stage stage, const char *msgContext)
{
  this->logEvent (string(logMsg),level,stage,string(msgContext));
}

//________________________________________________________________
void TxEventLog::logEvent (const char *userKey, 
		   const char *userValue,   Level level,
		   Stage stage, const char *msgContext)
{
   this->logEvent (string(userKey),string(userValue),
         level, stage, string(msgContext));
}

//________________________________________________________________
void TxEventLog::logEnd (const char *key, const char *value)
{
   this->logEnd (string(key),string(value));
}

//________________________________________________________________
StUcmTasks  *TxEventLog::getTaskList()
{ return getTaskList(0); }
//________________________________________________________________
StUcmTasks  *TxEventLog::getTaskList (int limit) 
{ return getTaskList(limit,0); }

//________________________________________________________________
StUcmJobs   *TxEventLog::getJobList() 
{ return getJobList((StRecord *)0); }

//________________________________________________________________
StUcmJobs   *TxEventLog::getJobList(StRecord *task) 
{ return getJobList(task,0); }
	  
//________________________________________________________________
StUcmJobs   *TxEventLog::getJobList(int limit)
{ return getJobList(0,limit,0); }
  
//________________________________________________________________
StUcmJobs   *TxEventLog::getJobList(StRecord *task,int limit)
{ return getJobList(task,limit,0); }
//________________________________________________________________
StUcmJobs   *TxEventLog::getJobList (int limit, int offset)
{   return getJobList(0,limit,offset); }
//________________________________________________________________
StUcmEvents *TxEventLog::getEventList() 
{ return getEventList((StRecord *)0);                            }
//________________________________________________________________
StUcmEvents *TxEventLog::getEventList(StRecord *job) 
{ return getEventList(job,0);                                    }

//________________________________________________________________
StUcmEvents *TxEventLog::getEventList(StRecord *job,int limit) 
{ return getEventList(job,limit, 0);                             }

//________________________________________________________________
StUcmEvents *TxEventLog::getEventList(int limit) 
{ return getEventList(0,limit, 0);                               }
//________________________________________________________________
StUcmEvents *TxEventLog::getEventList(int limit, int offset)
{ return getEventList(0,limit, offset);                          }

//________________________________________________________________
int TxEventLog::queryTableSize(const char *tableName)
{   return queryTableSize(tableName,(const char*)0);             }
