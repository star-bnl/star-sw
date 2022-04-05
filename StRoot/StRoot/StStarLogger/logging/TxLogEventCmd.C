#include "TxEventLog.h"
#include "TxEventLogFactory.h"

#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <string>
#include <iostream>

void printUsage () {
  std::cout << "\nUSAGE:"
	    << "\n  TxLogEvent"
	    << "\n    -r/--requester    <requester name> :  required"
	    << "\n    -j/--brokerjobid  <broker job id>  :  required"
	    << "\n    -t/--brokertaskid <broker task id> :  required"
	    << "\n    -k/--key          <message key>    :  required"
	    << "\n    -v/--value        <message value>  :  required"
	    << "\n    -l/--level        <event level>    :  optional"
	    << "\n    -c/--context      <event context>  :  optional"
	    << std::endl << std::endl;
}
   
int getOpts (int argc, char** argv, std::string& requester,
	     std::string& brokerJobID, std::string& brokerTaskID,
	     std::string& key, std::string& value, 
	     std::string& context, std::string& level)
{
  static const char *optString = "r:j:t:c:l:s:k:v:h?";
  static struct option longOpts [] = {
    {"requester",    required_argument, NULL, 'r'},
    {"brokerjobid",  required_argument, NULL, 'j'},
    {"brokertaskid", required_argument, NULL, 't'},
    {"key",          required_argument, NULL, 'k'},
    {"value",        required_argument, NULL, 'v'},
    {"context",      required_argument, NULL, 'c'},
    {"level",        required_argument, NULL, 'l'},
    {"help",         no_argument,       NULL, 'h'}
  };
  int longIndex = 0;     

  int opt = getopt_long (argc, argv, optString, longOpts, &longIndex);
  if (opt == -1) {
    printUsage ();
    return 1;
  }

  bool rOpt = false, jOpt = false, tOpt = false, kOpt = false, vOpt = false;
  while (opt != -1) {
    switch (opt) {
    case 'r':
      rOpt = true;
      requester = optarg;
      break;
      
    case 'j':
      jOpt = true;
      brokerJobID = optarg;
      break;
      
    case 't':
      tOpt = true;
      brokerTaskID = optarg;
      break;
      
    case 'k':
      kOpt = true;
      key = optarg;
      break;
      
    case 'v':
      vOpt = true;
      value = optarg;
      break;
      
    case 'c':
      context = optarg;
      break;
      
    case 'l':
      level = optarg;
      break;
      
    case 'h':   
    case '?':
    case -1:
    default:
      printUsage();
      return 1;
      break;
    }
    opt = getopt_long (argc, argv, optString, longOpts, &longIndex);
  }

  if (!rOpt || !jOpt || !tOpt || !kOpt || !vOpt) {
    printUsage ();
    return 1;
  }

  return 0;
}

TxLogging::TxEventLog::Level getLevel (const std::string& level) {
  int intLevel = std::atoi (level.c_str ());
  switch (intLevel) {
  case 1:
    return TxLogging::TxEventLog::LEVEL_TRACE;

  case 2: 
    return TxLogging::TxEventLog::LEVEL_DEBUG;

  case 3:
    return TxLogging::TxEventLog::LEVEL_INFO;
    
  case 4:
    return TxLogging::TxEventLog::LEVEL_NOTICE;

  case 5:
    return TxLogging::TxEventLog::LEVEL_WARNING;

  case 6: 
    return TxLogging::TxEventLog::LEVEL_ERROR;

  case 7:
    return TxLogging::TxEventLog::LEVEL_CRITICAL;
    
  case 8:
    return TxLogging::TxEventLog::LEVEL_ALERT;

  case 9:
    return TxLogging::TxEventLog::LEVEL_FATAL;
   
  default:
    return TxLogging::TxEventLog::LEVEL_UNKNOWN;
  }
}

int main (int argc, char** argv) {
  std::string requester = "",
    brokerJobID = "",
    brokerTaskID = "",
    key = "",
    value = "",
    context = "",
    level = "";

  if (getOpts (argc, argv, requester,
	       brokerJobID, brokerTaskID,
	       key, value,
	       context, level)) {
    return 1;
  }

  TxLogging::TxEventLog* log = TxLogging::TxEventLogFactory::create("ucm");
  int intBJobID = std::atoi (brokerJobID.c_str ());
  log->setBrokerJobID (intBJobID);
  log->setBrokerTaskID (brokerTaskID);
  log->setRequesterName (requester);
  log->logEvent (key, value, 
		 getLevel (level), TxLogging::TxEventLog::STATUS, 
		 context);

  return 0;
}
