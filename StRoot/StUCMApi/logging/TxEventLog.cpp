/*****************************************************************
 * @file TxEventLog.cpp
 * @author Roopa Pundaleeka
 *
 * @(#)cpp/api:$Id: TxEventLog.cpp,v 1.1 2009/04/07 19:00:27 fine Exp $
 *
 * Please see TxEventLog.h for more documentation.
 *****************************************************************/

#include "TxEventLog.h"

TxLogging::TxEventLog::TxEventLog () {
  // No broker task/job id info provided. Check if the default env
  // vars are set. If not, assume they are orphan messages
  brokerTaskID = TxUCMUtils::getEnv (TxUCMConstants::envBrokerTaskID);
  brokerJobID = TxUCMUtils::getEnv (TxUCMConstants::envBrokerJobID);
  
  // set context, hostname and read properties file
  this->setDefaults ();
}

TxLogging::TxEventLog::~TxEventLog() {
}

void TxLogging::TxEventLog::setEnvBrokerTaskID (const std::string& envBrokerTaskID) {
  this->brokerTaskID =
    TxUCMUtils::getEnv (envBrokerTaskID.c_str ());
}

void TxLogging::TxEventLog::setEnvBrokerJobID (const std::string& envBrokerJobID) {
  this->brokerJobID = 
    TxUCMUtils::getEnv (envBrokerJobID.c_str ());
}

void TxLogging::TxEventLog::setBrokerTaskID (const std::string& brokerTaskID) {
  this->brokerTaskID = std::string (brokerTaskID);
}

void TxLogging::TxEventLog::setBrokerJobID (int bJobID) {
  this->brokerJobID = TxUCMUtils::itoa (bJobID);
}

void TxLogging::TxEventLog::setRequesterName (const std::string& reqName) {
  this->requester = std::string (reqName);
}

void TxLogging::TxEventLog::setContext (const std::string& context) {
  this->context = context;
}

void TxLogging::TxEventLog::logStart (const std::string& key, 
				      const std::string& value) {
  this->writeMessage (TxUCMConstants::logEvent,
		      this->context,
		      TxEventLog::LEVEL_INFO,
		      TxEventLog::START,
		      key,
		      value);
}

void TxLogging::TxEventLog::logEvent (const std::string& logMessage, 
				      Level level, 
				      Stage stage, 
				      const std::string& msgContext) {
  this->writeMessage (TxUCMConstants::logEvent,
		      msgContext,
		      level,
		      stage,
		      TxUCMConstants::defaultKey,
		      logMessage);
}

void TxLogging::TxEventLog::logEvent (const std::string& userKey, 
				      const std::string& userValue, 
				      Level level, 
				      Stage stage, 
				      const std::string& msgContext) {
  this->writeMessage (TxUCMConstants::logEvent,
		      msgContext,
		      level,
		      stage,
		      userKey,
		      userValue);
}

void TxLogging::TxEventLog::setJobSubmitLocation (const std::string& url) {
  this->writeMessage (TxUCMConstants::submitEvent,
		      this->context,
		      TxEventLog::LEVEL_INFO,
		      TxEventLog::STATUS,
		      TxUCMConstants::siteLocation,
		      url);
}

void TxLogging::TxEventLog::setJobSubmitState (State state) {
  this->writeMessage (TxUCMConstants::submitEvent,
		      this->context,
		      TxEventLog::LEVEL_INFO,
		      TxEventLog::STATUS,
		      TxUCMConstants::stateID,
		      TxUCMUtils::itoa (state));
}

void TxLogging::TxEventLog::setJobSubmitID (const std::string& ID) {
  this->writeMessage (TxUCMConstants::submitEvent,
		      this->context,
		      TxEventLog::LEVEL_INFO,
		      TxEventLog::STATUS,
		      TxUCMConstants::gridJobID,
		      ID);
}

void TxLogging::TxEventLog::logEnd (const std::string& key, 
				    const std::string& value) {
  // Write the END message
  this->writeMessage (TxUCMConstants::logEvent,
		      this->context,
		      TxEventLog::LEVEL_INFO,
		      TxEventLog::END,
		      key,
		      value);
}

void TxLogging::TxEventLog::readProperties () {
  std::string line;
  std::ifstream file (TxUCMConstants::propsFile);
  if (file.is_open()) {
    while (!file.eof()) {
      getline (file, line);

      if ((line.find ("#") != 1) 
	  && line.length () > 0) {
	int delim = line.find ("=");
	if (delim == std::string::npos) {
	  continue;
	}
	std::string propKey = line.substr (0, delim);
	std::string propValue = line.substr (delim + 1);
	ucmLogProps [TxUCMUtils::trimString (propKey)] = 
	  TxUCMUtils::trimString (propValue);
      }
    }
    file.close();

    // Get the log file name
    logFilePath = ucmLogProps [TxUCMConstants::logFileDir];
    logFilePath += "/";
    logFilePath += this->hostname;
    logFilePath += "_";
    logFilePath += this->username;
    logFilePath += "_";
    logFilePath += TxUCMUtils::itoa (getpid());
    logFilePath += ".log";
  }
}

void TxLogging::TxEventLog::setDefaults () {
  // set context to default values
  context = TxUCMConstants::defaultContext;
 
  // set host name
  this->hostname = TxUCMUtils::getEnv ("HOSTNAME");

  // set user name
  this->username = TxUCMUtils::getEnv ("LOGNAME");

  // requester is "orphan" by default
  this->requester = "orphan";

  // read properties file location of the log file
  this->readProperties ();
 
  this->startMsgWritten = false;
}

const char* TxLogging::TxEventLog::createHeader () {  
  std::string header = "";
  this->timestamp = TxUCMUtils::getTimeStamp ();
  header += this->timestamp; header += " ";
  header += this->hostname;  header += " ";
  header += "processName:";  header += " ";

  return header.c_str ();
}

void TxLogging::TxEventLog::writeMessage (const std::string& event,
					  const std::string& context,
					  const Level& level,
					  const Stage& stage,
					  const std::string& key,
					  const std::string& value) {
  std::string msg = "";
  // msg += this->createHeader ();
  msg += "ts=\"" + timestamp;                         msg += "\" ";
  msg += "event=\"" + event;                          msg += "\" ";
  msg += "broker.job.id=\"" + this->brokerJobID;      msg += "\" ";
  msg += "broker.task.id=\"" + this->brokerTaskID;    msg += "\" ";
  msg += "requester.name=\"" + this->requester;       msg += "\" ";
  msg += "context=\"" + context;                      msg += "\" ";
  msg += "level=\""; msg += TxUCMUtils::itoa (level); msg += "\" ";
  msg += "stage=\""; msg += TxUCMUtils::itoa (stage); msg += "\" ";
  msg += "key=\"" + key;                              msg += "\" ";
  msg += "value=\"" + value;                          msg += "\"\n";

  std::ofstream logFile;
  logFile.open (logFilePath.c_str (), std::ios::app);
  logFile << msg.c_str ();
  logFile.close();
}
