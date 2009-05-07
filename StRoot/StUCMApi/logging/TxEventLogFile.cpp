/*****************************************************************
 * @file TxEventLogFile.cpp
 * @author Roopa Pundaleeka
 *
 * @(#)cpp/api:$Id: TxEventLogFile.cpp,v 1.3 2009/05/07 22:52:22 fine Exp $
 *
 * Please see TxEventLogFile.h for more documentation.
 *****************************************************************/

#include "TxEventLogFile.h"

#include <string>
#include <cstdlib>
#include <vector>
#include <sys/types.h>
#include <unistd.h>
#include <ctime>
#include <sstream>
#include <iostream>
#include <fstream>

TxLogging::TxEventLogFile::TxEventLogFile ():TxLogging::TxEventLog() {
  // No broker task/job id info provided. Check if the default env
  // vars are set. If not, assume they are orphan messages
  brokerTaskID = TxUCMUtils::getEnv (TxUCMConstants::envBrokerTaskID);
  brokerJobID = TxUCMUtils::getEnv (TxUCMConstants::envBrokerJobID);
  if (brokerJobID=="orphan")brokerJobID = "0";
  
  // set context, hostname and read properties file
  this->setDefaults ();
}

TxLogging::TxEventLogFile::~TxEventLogFile() {
}

void TxLogging::TxEventLogFile::setEnvBrokerTaskID (const std::string& envBrokerTaskID) {
  this->brokerTaskID =
    TxUCMUtils::getEnv (envBrokerTaskID.c_str ());
}

void TxLogging::TxEventLogFile::setEnvBrokerJobID (const std::string& envBrokerJobID) {
  this->brokerJobID = 
    TxUCMUtils::getEnv (envBrokerJobID.c_str ());
}

void TxLogging::TxEventLogFile::setBrokerTaskID (const std::string& brokerTaskID) {
  this->brokerTaskID = std::string (brokerTaskID);
}

void TxLogging::TxEventLogFile::setBrokerJobID (int bJobID) {
  this->brokerJobID = TxUCMUtils::itoa (bJobID);
}

void TxLogging::TxEventLogFile::setRequesterName (const std::string& reqName) {
  this->requester = std::string (reqName);
}

void TxLogging::TxEventLogFile::setContext (const std::string& context) {
  this->context = context;
}

void TxLogging::TxEventLogFile::logStart (const std::string& key, 
				      const std::string& value) {
  this->writeMessage (TxUCMConstants::logEvent,
		      this->context,
		      TxEventLogFile::LEVEL_INFO,
		      TxEventLogFile::START,
		      key,
		      value);
}

void TxLogging::TxEventLogFile::logEvent (const std::string& logMessage, 
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

void TxLogging::TxEventLogFile::logEvent (const std::string& userKey, 
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

void TxLogging::TxEventLogFile::setJobSubmitLocation (const std::string& url) {
  this->writeMessage (TxUCMConstants::submitEvent,
		      this->context,
		      TxEventLogFile::LEVEL_INFO,
		      TxEventLogFile::STATUS,
		      TxUCMConstants::siteLocation,
		      url);
}

void TxLogging::TxEventLogFile::setJobSubmitState (State state) {
  this->writeMessage (TxUCMConstants::submitEvent,
		      this->context,
		      TxEventLogFile::LEVEL_INFO,
		      TxEventLogFile::STATUS,
		      TxUCMConstants::stateID,
		      TxUCMUtils::itoa (state));
}

void TxLogging::TxEventLogFile::setJobSubmitID (const std::string& ID) {
  this->writeMessage (TxUCMConstants::submitEvent,
		      this->context,
		      TxEventLogFile::LEVEL_INFO,
		      TxEventLogFile::STATUS,
		      TxUCMConstants::gridJobID,
		      ID);
}

void TxLogging::TxEventLogFile::logEnd (const std::string& key, 
				    const std::string& value) {
  // Write the END message
  this->writeMessage (TxUCMConstants::logEvent,
		      this->context,
		      TxEventLogFile::LEVEL_INFO,
		      TxEventLogFile::END,
		      key,
		      value);
}

void TxLogging::TxEventLogFile::readProperties () {
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

void TxLogging::TxEventLogFile::setDefaults () {
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

const char* TxLogging::TxEventLogFile::createHeader () {  
  std::string header = "";
  this->timestamp = TxUCMUtils::getTimeStamp ();
  header += this->timestamp; header += " ";
  header += this->hostname;  header += " ";
  header += "processName:";  header += " ";

  return header.c_str ();
}

void TxLogging::TxEventLogFile::writeMessage (const std::string& event,
					  const std::string& context,
					  const Level& level,
					  const Stage& stage,
					  const std::string& key,
					  const std::string& value) {
  std::string msg = "";
  // msg += this->createHeader ();
  this->timestamp = TxUCMUtils::getTimeStamp ();
  msg += "ts=\"" + timestamp;                         msg += "\" ";
  msg += "event=\"" + event;                          msg += "\" ";
  msg += "broker.job.id=\"" + this->brokerJobID;      msg += "\" ";
  msg += "broker.task.id=\"" + this->brokerTaskID;    msg += "\" ";
  msg += "requester.name=\"" + this->requester;       msg += "\" ";
  msg += "context=\"" + context;                      msg += "\" ";
  msg += "level=\""; msg += TxUCMUtils::itoa (level); msg += "\" ";
  msg += "stage=\""; msg += TxUCMUtils::itoa (stage); msg += "\" ";
  msg += "key=\"" + key;                              msg += "\" ";
  msg += "value=\"" + value;                          msg += "\" ";
  
  writeDown(msg);
}

void TxLogging::TxEventLogFile::writeDown(const std::string& message)
{
  std::ofstream logFile(logFilePath.c_str (), std::ios::app);
  logFile << message.c_str () << "\n";
  logFile.close();

  std::string httpstring="wget -b  -q -o /dev/null ";
  httpstring+= "-O /dev/null \'http://connery.star.bnl.gov/ucm/?m=";
  httpstring+=message;
  httpstring+="\'";
  system( httpstring.c_str());
}
