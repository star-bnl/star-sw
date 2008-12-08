/*
 * @file TxEventLog.h
 * @author Stephen Tramer
 *
 * @(#)cpp/api:$Id: TxEventLog.h,v 1.1 2008/12/08 21:10:25 fine Exp $
 *
 * TxEventLog provides an interface for applications so that they can write
 * event information into the JobEvents collection.
 */
 
#ifndef TX_EVENT_LOG_H
#define TX_EVENT_LOG_H

#include <string>

class TxStoreHandler;

/**
 * Abstraction for the collection in memory, with facilities to manipulate 
 * collection data in the store.
 */
namespace TxTrackingAPI {
  class TxEventLog {
  public:
    enum Stage {
      START=1,
      STATUS=2,
      END=3
    };

    enum Level {
      LEVEL_TRACE=1,
      LEVEL_DEBUG=2,
      LEVEL_INFO=3,
      LEVEL_NOTICE=4,
      LEVEL_WARNING=5,
      LEVEL_ERROR=6,
      LEVEL_CRITICAL=7,
      LEVEL_ALERT=8,
      LEVEL_FATAL=9
    };

  public:
    /**
     * Constructor: Connects event log to a store, which events are written to.
     * Looks for brokerJobID: if available lookup jobID, if no brokerJobID create 
     * orphan task/job entries.  Gets hostname and sets nodeLocation for
     * the running job.  Sets startTime in Jobs table, change stateID to "Active".
     * Uses envrionment variable UCM_STORE_INFO to configure module.
     */
    TxEventLog(   const char *envStoreInfo     = 0
                , const char *envBrokerJobID   = 0
                , const char *envLocalUser     = 0
                , const char *envGRAMLocalUser = 0 );

    /**
     * Destructor: Sets job state to finished, decrements task remaining job count.
     */
    ~TxEventLog();

    /**
     * Log an event not associated with the job.
     * @param stage The stage of the application assocated with the event.  
     *              Should NOT be necessary for system events.
     * @param level The level of severity of the event.
     * @param systemContext The context in which the event occurs.
     * @param systemMsg The message to log.
     */
    void logSystemEvent(Stage stage, Level level, const std::string& systemContext, 
			const std::string& systemMsg);

    /**
     * Log an event associated with the current jobID.
     * @param stage The stage of the application assocated with the event.  
     * @param level The level of severity of the event.
     * @param userContext The context in which the event occurs.
     * @param userKey A key to associate with the message.
     * @param userMsg The message to log.
     */
    void logUserEvent(Stage stage, Level level, const std::string& userContext, 
		      const std::string& userKey, const std::string& userMsg);

  private:
    /**
     * @internal
     * Confirms that the named collection exists.
     * @param name The name of the collection.
     * @throws TxUCMException If the collection is not found.
     */
    void confirmCollectionExists(const std::string& name);

  private:
    long jobID_;
    TxStoreHandler *store_;
  };
}
#endif
