/**
 * @file TxUCMDBConstants.h
 * @author Stephen Tramer
 *
 * @(#)cpp/api:$Id: TxUCMConstants.h,v 1.4 2010/04/15 20:23:05 fine Exp $
 *
 * This file is a collection of constants (collection names and record names)
 * used by the UCM API.  Rather than create a static object to contain all of
 * this information, it is hidden behind the TxUCMConstants namespace.
 * This header also contains documentation about collection structures in the
 * ucm store.
 */

#ifndef __TX_UCM_CONSTANTS__
#define __TX_UCM_CONSTANTS__

#include <string>

namespace TxUCMConstants {
  /**
   * Properties file
   */
  static const char* propsFile   = "ucmlogging.properties";
  static const char* logFileDir  = "directory.name";
  static const char* logFileName = "logfile.name";

  /**
   * Env. variable names:
   * These are environment variables which are used by the UCM
   * API.  Stored as const char* to avoid c_str() conversions when
   * calling getenv().
   */
  static const char* envBrokerTaskID   = "REQUESTID"; // "UCM_BROKER_TASKID";
  static const char* envBrokerJobID    = "JOBINDEX";  // "UCM_BROKER_JOBID";
  static const char* defaultContext    = "UCM Default Context";
  static const char* defaultRequester  = "UCM Default Requester";

  /**
   * Event types
   */
  static const char* logEvent     = "com.txcorp.ucm.log.event"; 
  static const char* submitEvent  = "com.txcorp.ucm.submit.event";

  /**
   * Special Message keys for messages from broker
   */
  static const char* newTask      = "com.txcorp.ucm.newtask";
  static const char* updateTask   = "com.txcorp.ucm.updatetask";
  static const char* addJob       = "com.txcorp.ucm.addjob";
  static const char* updateJob    = "com.txcorp.ucm.updatejob";

  static const char* siteLocation = "com.txcorp.ucm.job.siteLocation";
  static const char* stateID      = "com.txcorp.ucm.job.stateID";
  static const char* gridJobID    = "com.txcorp.ucm.job.gridJobID";
  static const char* defaultKey   = "com.txcorp.ucm.message";
  static const char* appStart     = "com.txcorp.ucm.app.start";
  static const char* appEnd       = "com.txcorp.ucm.app.end";
}
#ifndef UNUSED_UCM__MESSAGES 
#define UNUSED_UCM__MESSAGES           \
namespace {                            \
   bool unused =                       \
        TxUCMConstants::propsFile &&   \
        TxUCMConstants::logFileDir &&  \
        TxUCMConstants::logFileName && \
        TxUCMConstants::envBrokerTaskID  && \
        TxUCMConstants::envBrokerJobID   && \
        TxUCMConstants::defaultRequester && \
        TxUCMConstants::logEvent &&     \
        TxUCMConstants::submitEvent &&  \
        TxUCMConstants::newTask &&      \
        TxUCMConstants::updateTask &&   \
        TxUCMConstants::addJob &&       \
        TxUCMConstants::updateJob &&    \
        TxUCMConstants::siteLocation && \
        TxUCMConstants::stateID &&      \
        TxUCMConstants::gridJobID &&    \
        TxUCMConstants::defaultKey      \
        ;                               \
}
UNUSED_UCM__MESSAGES
#endif
#endif
