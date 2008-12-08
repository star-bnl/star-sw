/*
 * @file TxTask.cpp
 * @author Stephen Tramer
 *
 * @(#)cpp/api:$Id: TxTask.cpp,v 1.1 2008/12/08 21:10:25 fine Exp $
 *
 * See TxTask.h for documentation.
 */

#include "TxTask.h"

#include "TxUCMException.h"
#include "TxUCMConstants.h"


TxTrackingAPI::TxTask::TxTask(const std::string& brokerTaskID, long brokerID, const std::string& requesterID,
	       const std::string& taskName, const std::string& taskDesc) 
  : store_(getenv(TxUCMConstant::envStoreInfo)) {
  
  if (!store_.isOpen() && !store_.open()) {
    throw TxUCMException(std::string("Task creation: Could not open store "
				     "with info: ") + getenv(TxUCMConstant::envStoreInfo),
			 TxUCMException::FATAL);
  }
  
  confirmCollectionExists(TxUCMConstant::taskCollection);
  confirmCollectionExists(TxUCMConstant::jobCollection);
  
  TxCollectionHandler* taskCollection = store_.execute(TxHandler::SELECT, TxUCMConstant::taskCollection);
  
  time_t now;
  time(&now);
  char date[20]; // Dates will always be exactly 19 characters in size (YYYY-MM-DD HH:MM:SS) + \0
  strftime(date, 20, "%F %T", localtime(&now));
  
  TxRecord* pattern = taskCollection->createPattern();
  pattern->getField(TxUCMConstant::brokerTaskID)->setValueFromString(brokerTaskID);
  // TODO: Check that there is a valid entry in the broker/requester dictionaries?
  pattern->getField(TxUCMConstant::brokerID)->setValue(brokerID);
  pattern->getField(TxUCMConstant::requesterID)->setValue(requesterID);
  pattern->getField(TxUCMConstant::taskName)->setValueFromString(taskName);
  pattern->getField(TxUCMConstant::taskDesc)->setValueFromString(taskDesc);
  // Start with 0 jobs
  pattern->getField(TxUCMConstant::taskSize)->setValue(0L);
  pattern->getField(TxUCMConstant::taskRemain)->setValue(0L);
  pattern->getField(TxUCMConstant::submitTime)->setValue(date);
  
  try {
    taskCollection->execute(TxHandler::INSERT, pattern);
  }
  catch (...) {
    delete taskCollection;
    delete pattern;
    store_.close();

    throw;
  }
  
  std::vector<TxRecord*> tasks = taskCollection->execute(TxHandler::SELECT, 
							 pattern);

  if (tasks.size() != 1) {
    // Clear memory
    for (std::vector<TxRecord*>::iterator iter = tasks.begin();
	 iter != tasks.end();
	 iter++) {
      delete (*iter);
    }
    delete taskCollection;
    delete pattern;
    store_.close();
    
    throw TxUCMException("TxTask creation: Created nonunique task for "
 			  "brokerTaskID: " + brokerTaskID,
			 TxUCMException::FATAL);
  }

  taskID_ = tasks[0]->getField(TxUCMConstant::taskID)->getValue<long>();
  
  delete taskCollection;
  delete pattern;
  delete tasks[0];
}


TxTrackingAPI::TxTask::~TxTask()
{} // EMPTY


long TxTrackingAPI::TxTask::addJob(long brokerJobID) {
  TxCollectionHandler* jobCollection = store_.execute(TxHandler::SELECT,
						      TxUCMConstant::jobCollection);

  TxRecord* pattern = jobCollection->createPattern();
  pattern->getField(TxUCMConstant::brokerJobID)->setValue(brokerJobID);
  pattern->getField(TxUCMConstant::taskID)->setValue(taskID_);


  try {
    jobCollection->execute(TxHandler::INSERT, pattern);
  }
  catch (...) {
    delete jobCollection;
    delete pattern;

    throw;
  }

  std::vector<TxRecord*> jobs = jobCollection->execute(TxHandler::SELECT, 
						       pattern);

  if (jobs.size() != 1) {
    // Clear memory
    for (std::vector<TxRecord*>::iterator iter = jobs.begin();
	 iter != jobs.end();
	 iter++) {
      delete (*iter);
    }
    delete jobCollection;
    delete pattern;

    std::ostringstream converter;
    converter << brokerJobID;
    
    throw TxUCMException("TxTask: Created nonunique job for "
 			  "brokerJobID: " + converter.str());
  }

  long jobID = jobs[0]->getField(TxUCMConstant::jobID)->getValue<long>();
  
  delete jobCollection;
  delete pattern;
  delete jobs[0];

  // Once job is successfully added, grab collection and increment task info
  TxCollectionHandler* taskCollection = store_.execute(TxHandler::SELECT, TxUCMConstant::taskCollection);
  TxRecord* taskPattern = taskCollection->createPattern();

  taskPattern->getField(TxUCMConstant::taskID)->setValue(taskID_);
  
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
    delete taskPattern;

    std::ostringstream converter;
    converter << taskID_;

    throw TxUCMException("Job creation: Found nonunique task for "
			   "taskID: " + converter.str());
  }

  TxRecord* task = tasks[0];

  long remainingJobs = task->getField(TxUCMConstant::taskRemain)->getValue<long>();
  long totalSize = task->getField(TxUCMConstant::taskSize)->getValue<long>();
  remainingJobs++;
  totalSize++;
  
  task->getField(TxUCMConstant::taskRemain)->setValue(remainingJobs);
  task->getField(TxUCMConstant::taskSize)->setValue(totalSize);
  task->getField(TxUCMConstant::updateTime)->setValueFromString("CURRENT_TIMESTAMP");

  taskCollection->execute(TxHandler::UPDATE, taskPattern, task);

  return jobID;
}


long TxTrackingAPI::TxTask::getTaskID() const {
  return taskID_;
}


// TODO: Add checking for field existence as well.
void TxTrackingAPI::TxTask::confirmCollectionExists(const std::string& name) {
  TxCollectionHandler* collection;
  try {
    collection = store_.execute(TxHandler::SELECT, name);
  }
  catch (...) {
    throw TxUCMException("TxTask: Could not find required collection '" 
			 + name + "'.",
			 TxUCMException::FATAL);
  }
  delete collection;
}
