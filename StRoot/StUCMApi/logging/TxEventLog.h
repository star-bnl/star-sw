/*
 * @file TxEventLog.h
 * @author Roopa Pundaleeka
 *
 * @(#)cpp/api:$Id: TxEventLog.h,v 1.1 2009/04/07 19:00:27 fine Exp $
 *
 * TxEventLog provides an interface for applications so that they can write
 * event information into a CEDPS formated file.
 */
 
#ifndef TX_EVENT_LOG_H
#define TX_EVENT_LOG_H

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
#include "TxUCMConstants.h"

namespace TxLogging {
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
      LEVEL_FATAL    = 9
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
      FAILED         = 9
    };

    /**
     * Constructor
     * - Gets hostname and sets nodeLocation for the running job.  
     *
     */    
    TxEventLog ();

    /**
     * Destructor: 
     * - Sets job state to finished (STAGE = END) 
     *
     */
    ~TxEventLog ();

    /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the task ID by passing the
     * name of the environment variable
     *
     * @param string envBrokerTaskID, Name of the environment variable
     *
     */
    void setEnvBrokerTaskID (const std::string& envBrokerTaskID);
    
    /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the job ID by passing the
     * name of the environment variable
     *
     * @param string envBrokerJobID, Name of the environment variable
     *
     */
    void setEnvBrokerJobID (const std::string& envBrokerJobID);
    
    /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the value of Task ID here.
     *       
     * @param string brokerTaskID, value of Task ID
     *
     */
    void setBrokerTaskID (const std::string& brokerTaskID);
    
    /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the value of Job ID here.
     *       
     * @param int brokerJobID, value of Job ID
     *
     */
    void setBrokerJobID (int brokerJobID);
    
    /**
     * Set the requester name
     *       
     * @param string requesterName
     *
     */
    void setRequesterName (const std::string& requester);
    
    /**
     * Set context for this task/job
     *
     * @param context, the message context
     */
    void setContext (const std::string& context);

    /**
     * Called by the *application* to log a START event
     *
     */
    void logStart (const std::string& key   = TxUCMConstants::appStart,
		   const std::string& value = "application started");

    /**
     * Log the job submit location. This method will be called by the
     * Broker.
     *
     * @param string url, Job submit location. 
     *
     */    
    void setJobSubmitLocation (const std::string& url);

    /**
     * Log the job state. This method will be called by the Broker.
     *
     * @param enum Stage, Job state. 
     *
     */    
    void setJobSubmitState (State state);

    /**
     * Log the job submit ID. This method will be called by the
     * Broker.
     *
     * @param string ID, Job submit ID. 
     *
     */    
    void setJobSubmitID (const std::string& ID);

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
    void logEvent (const std::string& logMsg, 
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
    void logEvent (const std::string& userKey, 
		   const std::string& userValue, 
		   Level level = LEVEL_INFO, 
		   Stage stage = STATUS, 
		   const std::string& msgContext = TxUCMConstants::defaultContext);

    /**
     * Called by the *application* to log an END event
     *
     */
    void logEnd (const std::string& key   = TxUCMConstants::appEnd,
		 const std::string& value = "application ended");

  private:
    /**
     * Read properties file to get the location of the log file
     */
    void readProperties ();
  
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
		       const std::string& key = "",
		       const std::string& value = "");

    std::string brokerJobID, brokerTaskID, requester, context;
    std::map <std::string, std::string> ucmLogProps;
    std::string timestamp, logFilePath;
    std::string username, hostname;
    bool startMsgWritten;
  };
}
#endif
