/*
 * @file TxEventLog.h
 * @author Roopa Pundaleeka
 *
 * @(#)cpp/api:$Id: TxEventLog.h,v 1.7 2010/09/17 19:34:54 fine Exp $
 *
 * TxEventLog provides an interface for applications so that they can write
 * event information into a CEDPS formated file.
 */
 
#ifndef TX_EVENT_LOG_H
#define TX_EVENT_LOG_H

#include <string>

#include "TxUCMConstants.h"

namespace TxLogging {
class StUcmTasks;
class StUcmJobs;
class StUcmEvents;
class StRecord;

    /**
     * TxEventLog is an abstarct inteface to the logginh system
     *
     */    
  class TxEventLog {

  public:
    enum Stage {
      START  = 1,
      STATUS = 2,
      END    = 3
    };

    enum Level {
      LEVEL_UNKNOWN  = 0,
      LEVEL_TRACE    = 1,
      LEVEL_DEBUG    = 2,
      LEVEL_INFO     = 3,
      LEVEL_NOTICE   = 4,
      LEVEL_WARNING  = 5,
      LEVEL_ERROR    = 6,
      LEVEL_CRITICAL = 7,
      LEVEL_ALERT    = 8,
      LEVEL_FATAL    = 9,
      TOTAL_LEVELS
    };

    enum State {
      UNKNOWN        = 0,
      UNSUBMITTED    = 1,
      STAGEIN        = 2,
      PENDING        = 3,
      ACTIVE         = 4,
      SUSPENDED      = 5,
      STAGEOUT       = 6,
      CLEANUP        = 7,
      DONE           = 8,
      FAILED         = 9,
      TOTAL_STATES
    };

protected:
    /**
     * Constructor
     * - Gets hostname and sets nodeLocation for the running job.  
     *
     */    
    TxEventLog ();
    virtual void writeDown(const std::string& message)=0;
public:

    /**
     * Destructor: 
     * - Sets job state to finished (STAGE = END) 
     *
     */
    virtual ~TxEventLog ();

    virtual void writeDown(const char *message);
   /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the task ID by passing the
     * name of the environment variable
     *
     * @param string envBrokerTaskID, Name of the environment variable
     *
     */
    virtual void setEnvBrokerTaskID (const std::string& envBrokerTaskID)=0;
    void setEnvBrokerTaskID (const char *envBrokerTaskID);
    
    /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the job ID by passing the
     * name of the environment variable
     *
     * @param string envBrokerJobID, Name of the environment variable
     *
     */
    virtual void setEnvBrokerJobID (const std::string& envBrokerJobID)=0;
    void setEnvBrokerJobID (const char *envBrokerJobID);
    
    /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the value of Task ID here.
     *       
     * @param string brokerTaskID, value of Task ID
     *
     */
    virtual void setBrokerTaskID (const std::string& brokerTaskID)=0;
    void setBrokerTaskID (const char *brokerTaskID);
    
    /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the value of Job ID here.
     *       
     * @param int brokerJobID, value of Job ID
     *
     */
    virtual void setBrokerJobID (int brokerJobID)=0;
    
    /**
     * Set the requester name
     *       
     * @param string requesterName
     *
     */
    virtual void setRequesterName (const std::string& requester)=0;
    void setRequesterName (const char *requester);
    
    /**
     * Set context for this task/job
     *
     * @param context, the message context
     */
    virtual void setContext (const std::string& context)=0;
    void setContext (const char *context);

    /**
     * Called by the *application* to log a START event
     *
     */
    virtual void logStart (const std::string& key, const std::string& value)=0;
    void logStart (const char *key = TxUCMConstants::appStart,
		   const char *value = "application started");

    /**
     * Log the job submit location. This method will be called by the
     * Broker.
     *
     * @param string url, Job submit location. 
     *
     */    
    virtual void logJobSubmitLocation (const std::string& url)=0;
    void logJobSubmitLocation (const char *url);
    virtual void setJobSubmitLocation (const std::string& url)=0;
    void setJobSubmitLocation (const char *url);

    /**
     * Log the job attrbutes. This method will be called by the
     * Broker.
     *
     * @param string attrbutes key,  For example: "que"
     * @param string attrbutes value, For example queu name 
     *
     */ 
    virtual void logJobAttribute (const std::string& key, const std::string& value)=0;
    void logJobAttribute (const char *key, const char *value);
  /**
     * Log the task size. This method will be called by the
     * Broker to log the new task and its size.
     *
     * @param int size, The new task size (the total number of the jobs)
     *
     */    
    virtual void logTask (unsigned int size=1)=0;
    
   /**
     * Log the task with the attributes. 
     * This method will be called by the
     * Broker to log the new task and its attributes.
     * The attribuites MUST match the "Task" MySQL table
     * column names. The attribute types must match 
     * the mySql Task table column types
     *
     * @param string attributes. Task attributes
     *
     */    
    virtual void logTask (const std::string& taskAttributes)=0;
    void logTask (const char *taskAttributes);
    /**
     * Log the job state. This method will be called by the Broker.
     *
     * @param enum Stage, Job state. 
     *
     */    
    virtual void logJobSubmitState (State state)=0;
    virtual void setJobSubmitState (State state)=0;

    /**
     * Log the job submit ID. This method will be called by the
     * Broker.
     *
     * @param string ID, Job submit ID. 
     *
     */    
    virtual void logJobSubmitID (const std::string& ID)=0;
    void logJobSubmitID (const char *ID);
    virtual void setJobSubmitID (const std::string& ID)=0;
    void setJobSubmitID (const char *ID);

    /**
     * Log a simple message event. This event is always associated
     * with the currnet brokerTaskID and brokerJobID.
     *
     * @param logMsg The message to log.  
     * @param level The level of severity of the event.
     * @param stage The stage of the application assocated with the event.
     * @param msgContext The context in which the event occurs.
     *
     */
    virtual void logEvent (const std::string& logMsg, 
		   Level level = LEVEL_INFO,
		   Stage stage = STATUS, 
		   const std::string& msgContext = TxUCMConstants::defaultContext)=0;
    
    void logEvent (const char *logMsg, 
		   Level level = LEVEL_INFO, 
		   Stage stage = STATUS, 
		   const char *msgContext = TxUCMConstants::defaultContext);
    
    /**
     * Log a key-value pair type event. This event is always
     * associated with the current brokerTaskID and brokerJobID.
     *
     * @param userKey, The key of the key-value to log. 
     * @param userValue,  The value of the key-value to log. 
     * @param level The level of severity of the event.
     * @param stage The stage of the application assocated with the event.
     * @param msgContext The context in which the event occurs.
     *
     */    
    virtual void logEvent (const std::string& userKey, 
		   const std::string& userValue, 
		   Level level = LEVEL_INFO, 
		   Stage stage = STATUS, 
		   const std::string& msgContext = TxUCMConstants::defaultContext)=0;
    void logEvent (const char *userKey, 
		   const char *userValue,
		   Level level = LEVEL_INFO,
		   Stage stage = STATUS, 
		   const char *msgContext = TxUCMConstants::defaultContext);

    /**
     * Called by the *application* to log an END event
     *
     */
    virtual void logEnd (const std::string& key, const std::string& value)=0;
    void logEnd (const char *key   = TxUCMConstants::appEnd,
		 const char *value = "application ended");
//_______________________
//    
      virtual  StUcmTasks  *getTaskList ();
      virtual  StUcmTasks  *getTaskList (int limit);
      virtual  StUcmTasks  *getTaskList (int limit, int offset) = 0; 
	  
      virtual  StUcmJobs   *getJobList();
      virtual  StUcmJobs   *getJobList(StRecord *task);
      virtual  StUcmJobs   *getJobList(StRecord *task, int limit);
      virtual  StUcmJobs   *getJobList(int limit);
      virtual  StUcmJobs   *getJobList(int limit, int offset);
      virtual  StUcmJobs   *getJobList(StRecord *task, int limit, int offset)=0;
      virtual  int          getJobId(const char *requester, const char *taskBrokerId, int jobBrokerId) = 0; 

      virtual  StUcmEvents *getEventList();
      virtual  StUcmEvents *getEventList(StRecord *job);
      virtual  StUcmEvents *getEventList(StRecord *job,int limit);
      virtual  StUcmEvents *getEventList(int limit);
      virtual  StUcmEvents *getEventList(int limit, int offset);
      virtual  StUcmEvents *getEventList(StRecord *job,int limit, int offset)=0;

      virtual  int  queryTableSize(const char *tableName);
      virtual  int  queryTableSize(const char *tableName, const char *where)=0;
      virtual  int  queryTableSize(const char *tableName, const StRecord   *where)=0;

    /* The current Db for the last selected job
     *       
     * @param int dbJobID, value of Job ID
     *
     */
     virtual void setDbJobID (int dbJobID)=0;
  };
}
#ifndef TXEVENT_DEFAULT_IMPLEMENTAION
#define TXEVENT_DEFAULT_IMPLEMENTAION(classname)       \
 StUcmTasks  *classname::getTaskList ()                \
 { return TxEventLog::getTaskList() ; }                \
 StUcmTasks  *classname::getTaskList (int limit)       \
 { return TxEventLog::getTaskList(limit); }            \
 StUcmJobs  *classname::getJobList ()                  \
 { return TxEventLog::getJobList() ; }                 \
 StUcmJobs  *classname::getJobList (StRecord *task)    \
 { return TxEventLog::getJobList(task) ; }             \
 StUcmJobs  *classname::getJobList (int limit)         \
 { return TxEventLog::getJobList(limit); }             \
 StUcmJobs  *classname::getJobList (StRecord *task,int limit) \
 { return TxEventLog::getJobList(task,limit); }        \
 StUcmJobs  *classname::getJobList (int limit,int offset)     \
 { return TxEventLog::getJobList(limit,offset); }    \
 StUcmEvents  *classname::getEventList ()              \
 { return TxEventLog::getEventList() ; }               \
 StUcmEvents  *classname::getEventList (StRecord *job) \
 { return TxEventLog::getEventList(job) ; }            \
 StUcmEvents  *classname::getEventList (int limit)     \
 { return TxEventLog::getEventList(limit); }           \
 StUcmEvents  *classname::getEventList (StRecord *job,int limit) \
 { return TxEventLog::getEventList(job,limit); }       \
 StUcmEvents  *classname::getEventList (int limit,int offset)    \
 { return TxEventLog::getEventList(limit,offset); }    \
 int classname::queryTableSize (const char *table_name)\
 { return TxEventLog::queryTableSize(table_name); }    \
 void classname::writeDown(const char *message)        \
 { return TxEventLog::writeDown(message); } 

#define TXEVENT_DEFAULT_IMPLEMENTAION_2(classname)            \
 int classname::getJobId (const char *requester, const char *taskBrokerId, int jobBrokerId)\
 {  assert(0 && "getJobId has not been implemented yet");     \
    return 0; }                                               \
 StUcmTasks  *classname::getTaskList (int limit, int offset)  \
 {  assert(0 && "getTaskList has not been implemented yet");  \
    return 0; }                                               \
 StUcmJobs  *classname::getJobList (StRecord *task,int limit, int offset)                 \
 { assert(0 && task && limit && offset &&  "getJobList has not been implemented yet");    \
    return 0; }                                               \
 StUcmEvents  *classname::getEventList (StRecord *job,int limit, int offset)              \
 { assert(0 && job && limit && offset&& "getEventList has not been implemented yet");     \
    return 0; }                                               \
 int classname::queryTableSize (const char *table_name, const char *where)                \
 { assert(0 && table_name && where && "queryTableSize has not been implemented yet");     \
   return 0; }                                                \
 int classname::queryTableSize (const char *table_name, const StRecord *where)            \
 { assert(0 && table_name && where && "queryTableSize has not been implemented yet");     \
   return 0; }                                                \
 void classname::setDbJobID (int bJobID)                      \
 { assert(0 && bJobID && "setDbJobID has not been implemented yet"); }
#endif
#endif
