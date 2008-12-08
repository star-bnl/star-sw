/**
 * @file TxUCMDBConstants.h
 * @author Stephen Tramer
 *
 * @(#)cpp/api:$Id: TxUCMConstants.h,v 1.1 2008/12/08 21:10:26 fine Exp $
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

namespace TxUCMConstant {
  /**
   * Env. variable names:
   * These are environment variables which are used by the UCM
   * API.  Stored as const char* to avoid c_str() conversions when
   * calling getenv().
   */
  static const char* envStoreInfo = "UCM_STORE_INFO";
  static const char* envBrokerJobID = "UCM_BROKER_JOBID";
  static const char* envLocalUser = "USER";
  static const char* envGRAMLocalUser = "LOGNAME";
  static const char* envBrokerRequester = "UCM_BROKER_USERNAME";
  

  /**
   * Common names:
   * These field names are shared by multiple collections.  See
   * collection patterns to get more information.
   */
  static const std::string taskID = "taskID";
  static const std::string brokerID = "brokerID";
  static const std::string requesterID = "requesterID";
  static const std::string jobID = "jobID";
  static const std::string stateID = "stateID";
  static const std::string stageID = "stageID";
  static const std::string levelID = "levelID";
  static const std::string updateTime = "updateTime";
  
  /**
   * Pattern for Tasks is:
   * taskID::brokerTaskID::brokerID::requesterID::taskName::taskDescription::
   * taskSize::taskRemainSize::submitTime::updateTime
   */

  static const std::string taskCollection = "Tasks";
  static const std::string brokerTaskID = "brokerTaskID";
  static const std::string taskName = "taskName";
  static const std::string taskDesc = "taskDescription";
  static const std::string taskSize = "taskSize";
  static const std::string taskRemain = "taskRemainSize";
  static const std::string submitTime = "submitTime";

  /**
   * Pattern for Jobs is:
   * jobID::brokerJobID::taskID::gridJobID::localJobID::gridSubmitTime::
   * localSubmitTime::siteLocation::queue::queuePosition::nodeLocation::
   * startTime::updateTime::executionUserName::stateID
   */
  static const std::string jobCollection = "Jobs";
  static const std::string brokerJobID = "brokerJobID";
  static const std::string gridJobID = "gridJobID";
  static const std::string localJobID = "localJobID";
  static const std::string gridSubmitTime = "gridSubmitTime";
  static const std::string localSubmitTime = "localSubmitTime";
  static const std::string siteLocation = "siteLocation";
  static const std::string queue = "queue";
  static const std::string queuePosition = "queuePosition";
  static const std::string nodeLocation = "nodeLocation";
  static const std::string startTime = "startTime";
  static const std::string executionUserName = "executionUserName";


  /**
   * Pattern for JobEvents is:
   * eventID::jobID::levelID::context::time::stageID::messageKey::messageValue
   */
  static const std::string eventCollection = "JobEvents";
  static const std::string eventID = "eventID";
  static const std::string context = "context";
  static const std::string time = "time";
  static const std::string messageKey = "messageKey";
  static const std::string messageValue = "messageValue";

  /**
   * Pattern for RequesterDictionary is:
   * requesterID::requesterName::requesterDescription::requesterAuthor::
   * requesterUpdateTime
   */
  static const std::string requesterDict = "RequesterDictionary";
  static const std::string requesterName = "requesterName";
  static const std::string requesterDescription = "requesterDescription";
  static const std::string requesterAuthor = "requesterAuthor";
  static const std::string requesterUpdateTime = "requesterUpdateTime";

  /**
   * Pattern for LevelDictionary is:
   * levelID::levelName::levelDescription::levelAuthor::levelUpdateTime
   */
  static const std::string levelDict = "LevelDictionary";
  static const std::string levelName = "levelName";
  static const std::string levelDesc = "levelDescription";
  static const std::string levelAuthor = "levelAuthor";
  static const std::string levelUpdateTime = "levelUpdateTime";

  /**
   * Pattern for StateDictionary is:
   * stateID::stateName::stateDescription::stateAuthor::stateUpdateTime
   */
  static const std::string stateDict = "StateDictionary";
  static const std::string stateName = "stateName";
  static const std::string stateDesc = "stateDescription";
  static const std::string stateAuthor = "stateAuthor";
  static const std::string stateUpdateTime = "stateUpdateTime";

  /**
   * Pattern for BrokerDictionary is:
   * brokerID::brokerName::brokerDescription::brokerAuthor::brokerUpdateTime
   */
  static const std::string brokerDict = "BrokerDictionary";
  static const std::string brokerName = "brokerName";
  static const std::string brokerDesc = "brokerDescription";
  static const std::string brokerAuthor = "brokerAuthor";
  static const std::string brokerUpdateTime = "brokerUpdateTime";

  /**
   * Pattern for StageDictionary is:
   * stageID::stageName::stageDescription::stageAuthor::stageUpdateTime
   */
  static const std::string stageDict = "StageDictionary";
  static const std::string stageName = "stageName";
  static const std::string stageDesc = "stageDescription";
  static const std::string stageAuthor = "stageAuthor";
  static const std::string stageUpdateTime = "stageUpdateTime";
}

#endif
