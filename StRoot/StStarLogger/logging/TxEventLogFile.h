/*
 * @file TxEventLogFile.h
 * @author Roopa Pundaleeka
 *
 * @(#)cpp/api:$Id: TxEventLogFile.h,v 1.5 2010/09/17 19:34:54 fine Exp $
 *
 * TxEventLogFile provides an interface for applications so that they can write
 * event information into a CEDPS formated file.
 */
 
#ifndef TX_EVENT_LOG_FILE_H
#define TX_EVENT_LOG_FILE_H

#include "TxEventLog.h"

#include <string>
#include <cstdlib>
#include <vector>
#include <sys/types.h>
#include <unistd.h>
#include <ctime>
#include <sstream>
#include <iostream>
#include <fstream>
#include <map>

#include "TxUCMUtils.h"

namespace TxLogging {
  class TxEventLogFile: public TxEventLog {
  protected:
    /**
     * Write down the prepared plain message using the concete media 
     * 
     * @param string message , Message to be written out
     *
     */
    virtual void writeDown(const std::string& message);
    virtual void writeDown(const char *message);

  public:
    /**
     * Constructor
     * - Gets hostname and sets nodeLocation for the running job.  
     *
     */    
    TxEventLogFile ();

    /**
     * Destructor: 
     * - Sets job state to finished (STAGE = END) 
     *
     */
    virtual ~TxEventLogFile ();

    /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the task ID by passing the
     * name of the environment variable
     *
     * @param string envBrokerTaskID, Name of the environment variable
     *
     */
    virtual void setEnvBrokerTaskID (const std::string& envBrokerTaskID);
    
    /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the job ID by passing the
     * name of the environment variable
     *
     * @param string envBrokerJobID, Name of the environment variable
     *
     */
    virtual void setEnvBrokerJobID (const std::string& envBrokerJobID);
    
    /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the value of Task ID here.
     *       
     * @param string brokerTaskID, value of Task ID
     *
     */
    virtual void setBrokerTaskID (const std::string& brokerTaskID);
    
    /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the value of Job ID here.
     *       
     * @param int brokerJobID, value of Job ID
     *
     */
    virtual void setBrokerJobID (int brokerJobID);
    
    /**
     * Set the requester name
     *       
     * @param string requesterName
     *
     */
    virtual void setRequesterName (const std::string& requester);
    
    /**
     * Set context for this task/job
     *
     * @param context, the message context
     */
    virtual void setContext (const std::string& context);

    /**
     * Called by the *application* to log a START event
     *
     */
    virtual void logStart (const std::string& key, const std::string& value);

    /**
     * Log the job attrbutes. This method will be called by the
     * Broker.
     *
     * @param string attrbutes key,  For example: "que"
     * @param string attrbutes value, For example queu name 
     *
     */
     
    virtual void logJobAttribute (const std::string& key, const std::string& value);
    /**
     * Log the job submit location. This method will be called by the
     * Broker.
     *
     * @param string url, Job submit location. 
     *
     */    
    virtual void logJobSubmitLocation (const std::string& url);
    virtual void setJobSubmitLocation (const std::string& url);

    /**
     * Log the job state. This method will be called by the Broker.
     *
     * @param enum Stage, Job state. 
     *
     */
     virtual void logJobSubmitState (State state);
     virtual void setJobSubmitState (State state);

   /**
     * Log the task size. This method will be called by the
     * Broker to log the new task and its size.
     *
     * @param int size, The new task size (the total number of the jobs)
     *
     */
    virtual void logTask (unsigned int size=1);

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

    virtual void logTask (const std::string& taskAttributes);

    /**
     * Log the job submit ID. This method will be called by the
     * Broker.
     *
     * @param string ID, Job submit ID. 
     *
     */    
    virtual void logJobSubmitID (const std::string& ID);
    virtual void setJobSubmitID (const std::string& ID);

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
		   const std::string& msgContext = TxUCMConstants::defaultContext);
    
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
		   const std::string& msgContext = TxUCMConstants::defaultContext);

    /**
     * Called by the *application* to log an END event
     *
     */
   virtual  void logEnd (const std::string& key,
		 const std::string& value);

  private:
    /**
     * Read properties file to get the location of the log file
     */
    virtual void readProperties ();
  
    /**
     * Set default context, hostname, and call readProperties
     */
    void setDefaults ();

    /**
     * Creates a CEDPS formatted header
     */
    const char* createHeader ();

    /**
     * Creates a CEDPS formatted message
     */
    void writeMessage (const std::string& event,
		       const std::string& context,
		       const Level& level,
		       const Stage& stage,
		       const std::string& key,
		       const std::string& value);

    std::string brokerJobID, brokerTaskID, requester, context;
    std::map <std::string, std::string> ucmLogProps;
    std::string timestamp, logFilePath;
    std::string username, hostname;
    bool startMsgWritten;
	

      virtual  StUcmTasks  *getTaskList ();
      virtual  StUcmTasks  *getTaskList (int limit);
      virtual  StUcmTasks  *getTaskList (int limit, int offset); 
	  
      virtual  StUcmJobs   *getJobList();
      virtual  StUcmJobs   *getJobList(StRecord *task);
      virtual  StUcmJobs   *getJobList(StRecord *task, int limit);
      virtual  StUcmJobs   *getJobList(int limit);
      virtual  StUcmJobs   *getJobList(int limit, int offset);
      virtual  StUcmJobs   *getJobList(StRecord *task, int limit, int offset);
      virtual  int          getJobId(const char *reqName,const char *taskBrokerId, int jobBrokerId);

      virtual  StUcmEvents *getEventList();
      virtual  StUcmEvents *getEventList(StRecord *job);
      virtual  StUcmEvents *getEventList(StRecord *job,int limit);
      virtual  StUcmEvents *getEventList(int limit);
      virtual  StUcmEvents *getEventList(int limit, int offset);
      virtual  StUcmEvents *getEventList(StRecord *job,int limit, int offset);

 	 virtual  void setDbJobID (int bJobID);

    virtual  int  queryTableSize(const char *tableName);
    virtual  int  queryTableSize(const char *tableName, const char *where);
    virtual  int  queryTableSize(const char *tableName, const StRecord   *where);

  };
}
#endif
