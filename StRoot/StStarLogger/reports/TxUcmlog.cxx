#include "TxUcmlog.h"

//_________________________________________________________________
    TXUcmLog::TXUcmLog(){}

// Logging options other than the defaults 
//_________________________________________________________________
TXUcmLog *TXUcmLog::openlog(const char* ident,int logopt, EFacility facility)
{  
   return new TXUcmLog();
}      
//_________________________________________________________________
TXUcmLog *TXUcmLog::openlog(const char* ident,int logopt)
{  
   return new TXUcmLog();
}   

//_________________________________________________________________
TXUcmLog *TXUcmLog::openlog(const char* ident)
{  
   return new TXUcmLog();
}      

//_________________________________________________________________
// Generate and Send the ucm record, 
// the optional attributes, and the optional user's message
void TXUcmLog::ucmlog(unsigned int pripority, const char*message)
{ }

//_________________________________________________________________
// Generate and Send the record that may contains the attributes only
void TXUcmLog::ucmlog(const char*message)
{       }

//_________________________________________________________________
// Generates and Send the ucm record that contains 
// Begin/End tag, the optional attributes and optional user's message
void TXUcmLog::ucmlogevent(EStage stage, const char*message)
{ }

//_________________________________________________________________
// send the message that contains the attributes and tags only
void TXUcmLog::ucmlogmessage(const char*message)
{ }

//_________________________________________________________________
// send "Job finished" record and close the ucm systen.
int TXUcmLog::closelog()
{ return 0; }

//_________________________________________________________________
// register the task with the ID provided and make it current
void TXUcmLog::openTask(unsigned int brokerTaskID)
{   }

//_________________________________________________________________
// register the job of the current task the ID provided
void TXUcmLog::openJob(unsigned int brokerJobID)
{ }

//_________________________________________________________________
// register the current task
void TXUcmLog::openTask()
{ }

//_________________________________________________________________
// register the current job
void TXUcmLog::openJob()
{ }

//_________________________________________________________________
void TXUcmLog::setlogmask(unsigned int logmask)
{ }
//_________________________________________________________________
// set the pair of the attribute : value
void TXUcmLog::setAttribute(const char *key, const char * value)
{ }

//_________________________________________________________________
// set the current task ID
void TXUcmLog::setTaskId(unsigned int brokerTaskID)
{ }

//_________________________________________________________________
// set the current job ID
void TXUcmLog::setJobId(unsigned int brokerJobID)
{}

//_________________________________________________________________
// set the current event ID
void TXUcmLog::setEventId(unsigned int taskId)
{  } 

//_________________________________________________________________
// return the attribute value by the attribute key
const char *TXUcmLog::getAttribute(const char *key) const
{ 
   return 0;
}

//_________________________________________________________________
// return the attribute value by the attribute index
const char *TXUcmLog::getAttribute(int attributeIndex) const
{ 
   return 0;
}


//_________________________________________________________________
// return the number of the different attributes;
int   TXUcmLog::getNumberofAttributes() const
{return 0;}

//_________________________________________________________________
// Reset the "key" attribute
void TXUcmLog::resetAttribute(const char *key)
{ }


//_________________________________________________________________
// Reset all attributes (clean up the "known" attribute list)
void TXUcmLog::resetAttributes()
{ }

//_________________________________________________________________
unsigned long TXUcmLog::getTaskId()  const
{return 0;}
//_________________________________________________________________
unsigned long TXUcmLog::getJobId()   const
{return 0;}
//_________________________________________________________________
unsigned long TXUcmLog::getEventId() const
{return 0;}
//_________________________________________________________________
bool TXUcmLog::isTaskOpen()  const
{return false;}
//_________________________________________________________________
bool TXUcmLog::isJobOpen()   const
{return false;}
