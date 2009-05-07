/*
 * @file TxEventLog.h
 * @author Roopa Pundaleeka
 *
 * @(#)cpp/api:$Id: TxEventLog.h,v 1.3 2009/05/07 20:32:34 fine Exp $
 *
 * TxEventLog provides an interface for applications so that they can write
 * event information into a CEDPS formated file.
 */
 
#ifndef TX_EVENT_LOG_H
#define TX_EVENT_LOG_H

#include <string>

#include "TxUCMConstants.h"

namespace TxLogging {

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

    /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the task ID by passing the
     * name of the environment variable
     *
     * @param string envBrokerTaskID, Name of the environment variable
     *
     */
    virtual void setEnvBrokerTaskID (const std::string& envBrokerTaskID)=0;
    
    /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the job ID by passing the
     * name of the environment variable
     *
     * @param string envBrokerJobID, Name of the environment variable
     *
     */
    virtual void setEnvBrokerJobID (const std::string& envBrokerJobID)=0;
    
    /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the value of Task ID here.
     *       
     * @param string brokerTaskID, value of Task ID
     *
     */
    virtual void setBrokerTaskID (const std::string& brokerTaskID)=0;
    
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
    
    /**
     * Set context for this task/job
     *
     * @param context, the message context
     */
    virtual void setContext (const std::string& context)=0;

    /**
     * Called by the *application* to log a START event
     *
     */
    virtual void logStart (const std::string& key   = TxUCMConstants::appStart,
		   const std::string& value = "application started")=0;

    /**
     * Log the job submit location. This method will be called by the
     * Broker.
     *
     * @param string url, Job submit location. 
     *
     */    
    virtual void setJobSubmitLocation (const std::string& url)=0;

    /**
     * Log the job state. This method will be called by the Broker.
     *
     * @param enum Stage, Job state. 
     *
     */    
    virtual void setJobSubmitState (State state)=0;

    /**
     * Log the job submit ID. This method will be called by the
     * Broker.
     *
     * @param string ID, Job submit ID. 
     *
     */    
    virtual void setJobSubmitID (const std::string& ID)=0;

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

    /**
     * Called by the *application* to log an END event
     *
     */
    virtual void logEnd (const std::string& key   = TxUCMConstants::appEnd,
		 const std::string& value = "application ended")=0;
  };
}
#endif
