/*
 * @file TxEventLog.cpp
 * @author Stephen Tramer
 *
 * @(#)cpp/api:$Id: TxEventLog.cpp,v 1.1 2008/12/08 21:10:25 fine Exp $
 *
 * See TxEventLog.h for documentation.
 */

#include <cstdlib>
#include <vector>
#include <sys/types.h>
#include <unistd.h>
#include <ctime>
#include <sstream>

#include "TxEventLog.h"
#include "TxStoreHandler.h"
#include "TxCollectionHandler.h"
#include "TxModuleException.h"
#include "TxDataException.h"
#include "TxUCMException.h"
#include "TxTask.h"
#include "TxJob.h"
#include "TxUCMConstants.h"


TxTrackingAPI::TxEventLog::TxEventLog(const char *envStoreInfo
                                     ,const char *envBrokerJobID
                                     ,const char *envLocalUser
                                     ,const char *envGRAMLocalUser)
: store_(0) {
  store_ = new TxStoreHandler(envStoreInfo ? envStoreInfo : getenv(TxUCMConstant::envStoreInfo));
  try {
    if (!store_->isOpen() && !store_->open()) {
      throw TxUCMException(std::string("EventLog creation: Could not open store "
				       "with info: ") + getenv(TxUCMConstant::envStoreInfo),
			   TxUCMException::FATAL);
    }

    // Make sure that all the required collections exist
    confirmCollectionExists(TxUCMConstant::taskCollection);
    confirmCollectionExists(TxUCMConstant::jobCollection);
    confirmCollectionExists(TxUCMConstant::eventCollection);

    const char* brokerJobID = envBrokerJobID ? envBrokerJobID :
                                               getenv(TxUCMConstant::envBrokerJobID);

    pid_t pid = getpid();

    char hostname[64]={0};
    gethostname(hostname, 64);
    const char* user = envLocalUser ? envLocalUser
                                    : getenv(TxUCMConstant::envLocalUser);

    // Since the GRAM does not actually log in, if the job was submitted    
    // through a GRAM scheduler instead of being run directly, we need to 
    // load the appropriate environment variable. 
    // TODO: We need to make this compatable with as many schedulers as possible.                        

    if (!user) {
      user = envGRAMLocalUser ? envGRAMLocalUser : getenv(TxUCMConstant::envGRAMLocalUser);
      if (!user) {
        throw TxUCMException("Log creation: Unable to locate local username",
			     TxUCMException::FATAL);
      }
    }

    
    if (brokerJobID != NULL) {
      // If there is a broker jobID, then there is a task and jobID.  We want
      // the jobID to key information in the JobEvents table.
      TxRecord* pattern;
      std::vector<TxRecord*> jobList;
      TxRecord* job;
      TxCollectionHandler* jobs = store_->execute(TxHandler::SELECT, 
						 TxUCMConstant::jobCollection);      

      pattern = jobs->createPattern();
      pattern->getField(TxUCMConstant::brokerJobID)->setValueFromString(brokerJobID);

      jobList = jobs->execute(TxHandler::SELECT, pattern);

      if (jobList.size() != 1) {
	// Clear memory
	for (std::vector<TxRecord*>::iterator iter = jobList.begin();
	     iter != jobList.end();
	     iter++) {
	  delete (*iter);
	}
	delete jobs;
	delete pattern;
	store_->close();

	throw TxUCMException(std::string("EventLog creation: Retrieved nonunique job "
			      "for brokerJobID: ") + brokerJobID,
			     TxUCMException::FATAL);
      }

      job = jobList[0];
      jobID_ = job->getField(TxUCMConstant::jobID)->getValue<long>();

      job->getField(TxUCMConstant::nodeLocation)->setValueFromString(hostname);
      job->getField(TxUCMConstant::executionUserName)->setValueFromString(user);
      job->getField(TxUCMConstant::stateID)->setValue(4L); // ACTIVE
      job->getField(TxUCMConstant::localJobID)->setValue((long)pid);
      // TODO: Include updating queue postion to 0?
      job->getField(TxUCMConstant::queuePosition)->setValue(0L);

      jobs->execute(TxHandler::UPDATE, pattern, job);

      // Cleanup
      delete jobs;
      delete pattern;
      delete job;
    }
    else {
      // Create string for pid
      std::ostringstream converter;
      std::string pidStr;

      converter << (long)pid;
      pidStr = converter.str();

      // If there is no brokerJobID, create a 'fake' task and job to
      // represent the current process.
      std::vector<TxRecord*> records;
      std::string dummyTaskID = "localTask:" + pidStr;

      /*
      // Check to see if UCM_BROKER_USERNAME is set and if so,
      // load up the requester dictionary.  If the requester is
      // there, then assign the task the appropriate ID.
      // In all other cases, assign the task requesterID '1',
      // the 'orphan' requester.
      
      long requesterID = 1;
      if (getenv(TxUCMConstant::envBrokerRequester)) {
	TxCollectionHandler* requesterDictionary = 
	  store_->execute(TxHandler::SELECT, TxUCMConstant::requesterDict);
	
	TxRecord* pattern = requesterDictionary->createPattern();
	pattern->getField(TxUCMConstant::requesterName)->setValueFromString(getenv(TxUCMConstant::envBrokerRequester));
	
	std::vector<TxRecord*> users = 
	  requesterDictionary->execute(TxHandler::SELECT, pattern);

	if (!users.empty()) {
	  if (users.size() != 1) {
	    delete requesterDictionary;
	    delete pattern;

	    throw TxUCMException("Error creating orphan job task: Found multiple requesters");
	  }
	  
	  requesterID = users[0]->getField(TxUCMConstant::requesterID)->getValue<long>();
	}
      }
      */

      // HACK FOR BNL'S INCORRECT REQUESTERID
      std::string requesterID;
      if (getenv(TxUCMConstant::envBrokerRequester)) {
	requesterID = getenv(TxUCMConstant::envBrokerRequester);
      }
      else {
	requesterID = "orphan";
      }

      TxTask dummyTask(dummyTaskID, 1, requesterID, "orphan", "Orphan task");
      jobID_ = dummyTask.addJob((long)pid);

      TxJob dummyJob(dummyTaskID, (long)pid);
      
      // Local submit time must be 'date' because.. you know.
      // TODO: We need a better, more flexible way to perform matching.  It may be
      // worthwhile to seperate out the underlying TxRecord and pattern mechanisms.
      time_t now;
      time(&now);
      char date[20]; // Dates will always be exactly 19 characters in size (YYYY-MM-DD HH:MM:SS) + \0
      strftime(date, 20, "%F %T", localtime(&now));
      
      dummyJob.setProperty(TxJob::LOCAL_JOB_ID, pidStr);
      dummyJob.setProperty(TxJob::LOCAL_SUBMIT_TIME, date);
      dummyJob.setProperty(TxJob::SITE, hostname);
      dummyJob.setProperty(TxJob::NODE, hostname);
      dummyJob.setProperty(TxJob::EXECUTING_USER, user);
      dummyJob.setProperty(TxJob::STATE, "4");
    }
  }
  catch (const TxUCMException& e) {
    throw;
  }
  catch (const std::exception& e) {
    throw TxUCMException("stdlib error while creating UCM log: " + std::string(e.what()));
  }
  catch (...) {
    throw TxUCMException("Unspecified error occured while creating UCM log",
			 TxUCMException::FATAL);
  }
}


TxTrackingAPI::TxEventLog::~TxEventLog() {
  std::ostringstream converter;
  std::string jobIDStr;
  
  converter << jobID_;
  jobIDStr = converter.str();
  
  TxCollectionHandler* taskCollection = store_->execute(TxHandler::SELECT, 
						       TxUCMConstant::taskCollection);
  TxCollectionHandler* jobCollection = store_->execute(TxHandler::SELECT, 
						      TxUCMConstant::jobCollection);

  TxRecord* taskPattern = taskCollection->createPattern();
  TxRecord* jobPattern = jobCollection->createPattern();
  TxRecord* job;
  TxRecord* task;
  
  std::string taskID;

  jobPattern->getField(TxUCMConstant::jobID)->setValue(jobID_);

  std::vector<TxRecord*> jobs = jobCollection->execute(TxHandler::SELECT,
						       jobPattern);
  
  if (jobs.size() != 1) {
    // Clear memory
    for (std::vector<TxRecord*>::iterator iter = jobs.begin();
	 iter != jobs.end();
	 iter++) {
      delete (*iter);
    }
    delete taskCollection;
    delete jobCollection;
    delete taskPattern;
    delete jobPattern;

    throw TxUCMException("EventLog destruction: Found nonunique job for "
			  "jobID: " + jobIDStr,
			 TxUCMException::FATAL);
  }
  
  job = jobs[0];

  taskID = job->getField(TxUCMConstant::taskID)->getValueAsString();
  
  // TODO: What if the eventLog is destructed as part of a failure (stateID=9)?
  // Or what if a user marks a job as completed before destructing the eventlog?
  job->getField(TxUCMConstant::stateID)->setValue(8L); // DONE
  job->getField(TxUCMConstant::updateTime)->setValueFromString("CURRENT_TIMESTAMP");

  jobCollection->execute(TxHandler::UPDATE, jobPattern, job);
  

  taskPattern->getField(TxUCMConstant::taskID)->setValueFromString(taskID);
  
  std::vector<TxRecord*> tasks = taskCollection->execute(TxHandler::SELECT,
							 taskPattern);

  if (tasks.size() != 1) {
    // Clear memory
    for (std::vector<TxRecord*>::iterator iter = tasks.begin();
	 iter != tasks.end();
	 iter++) {
      delete (*iter);
    }
    delete taskCollection;
    delete jobCollection;
    delete taskPattern;
    delete jobPattern;
    delete job;

    throw TxUCMException("EventLog destruction: Found nonunique job for "
			  "jobID: " + jobIDStr,
			 TxUCMException::FATAL);
  }

  task = tasks[0];

  long remainingJobs = task->getField(TxUCMConstant::taskRemain)->getValue<long>();
  remainingJobs--;
  
  task->getField(TxUCMConstant::taskRemain)->setValue(remainingJobs);
  task->getField(TxUCMConstant::updateTime)->setValueFromString("CURRENT_TIMESTAMP");

  taskCollection->execute(TxHandler::UPDATE, taskPattern, task);

  delete taskCollection;
  delete jobCollection;
  delete taskPattern;
  delete jobPattern;
  delete job;
  delete task;
  delete store_;  store_ = 0;
}


void
TxTrackingAPI::TxEventLog::logSystemEvent(Stage stage, Level level, const std::string& systemContext,
			   const std::string& systemMsg) {
  TxCollectionHandler* jobEventCollection = store_->execute(TxHandler::SELECT,
							   TxUCMConstant::eventCollection);

  TxRecord* pattern = jobEventCollection->createPattern();

  pattern->getField(TxUCMConstant::jobID)->setValue(jobID_);
  pattern->getField(TxUCMConstant::levelID)->setValue((long)level);
  pattern->getField(TxUCMConstant::context)->setValueFromString(systemContext);
  pattern->getField(TxUCMConstant::time)->setValueFromString("CURRENT_TIMESTAMP");
  pattern->getField(TxUCMConstant::stageID)->setValue((long)stage);
  pattern->getField(TxUCMConstant::messageKey)->setValue("SYSTEM");
  pattern->getField(TxUCMConstant::messageValue)->setValueFromString(systemMsg);

  jobEventCollection->execute(TxHandler::INSERT, pattern);

  delete jobEventCollection;
  delete pattern;
}


void
TxTrackingAPI::TxEventLog::logUserEvent(Stage stage, Level level, const std::string& userContext,
			 const std::string& userKey, const std::string& userMsg) {
  TxCollectionHandler* jobEventCollection = store_->execute(TxHandler::SELECT,
							   TxUCMConstant::eventCollection);

  TxRecord* pattern = jobEventCollection->createPattern();

  pattern->getField(TxUCMConstant::jobID)->setValue(jobID_);
  pattern->getField(TxUCMConstant::levelID)->setValue((long)level);
  pattern->getField(TxUCMConstant::context)->setValueFromString(userContext);
  pattern->getField(TxUCMConstant::time)->setValueFromString("CURRENT_TIMESTAMP");
  pattern->getField(TxUCMConstant::stageID)->setValue((long)stage);
  pattern->getField(TxUCMConstant::messageKey)->setValue(userKey);
  pattern->getField(TxUCMConstant::messageValue)->setValueFromString(userMsg);

  jobEventCollection->execute(TxHandler::INSERT, pattern);

  delete jobEventCollection;
  delete pattern;
}


// TODO: Add checking for field existence as well.
void TxTrackingAPI::TxEventLog::confirmCollectionExists(const std::string& name) {
  TxCollectionHandler* collection;
  try {
    collection = store_->execute(TxHandler::SELECT, name);
  }
  catch (...) {
    throw TxUCMException("TxEventLog: Could not find required collection '" 
			  + name + "'.",
			 TxUCMException::FATAL);
  }
  delete collection;
}
