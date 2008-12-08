/*
 * @file TxTask.cpp
 * @author Stephen Tramer
 *
 * @(#)cpp/api:$Id: TxJob.cpp,v 1.1 2008/12/08 21:10:25 fine Exp $
 *
 * See file TxJob.
 */
 
#include "TxJob.h"

#include "TxCollectionHandler.h"
#include "TxRecord.h"
#include "TxField.h"
#include "TxUCMException.h"
#include "TxUCMConstants.h"

#include <vector>
#include <iomanip>
#include <cctype>


TxTrackingAPI::TxJob::TxJob(const std::string& brokerTaskID, long brokerJobID) 
  : store_(getenv(TxUCMConstant::envStoreInfo)),
    started_(false)
   , job_(NULL) {

  long taskID;

  TxCollectionHandler* taskCollection;
  TxCollectionHandler* jobCollection;

  TxRecord* taskPattern;
  TxRecord* jobPattern;

  std::vector<TxRecord*> tasks;
  std::vector<TxRecord*> jobs;

  TxRecord* task;

  if (!store_.isOpen() && !store_.open()) {
    throw TxUCMException(std::string("Job creation: Could not open store "
				     "with info: ") + getenv(TxUCMConstant::envStoreInfo),
			 TxUCMException::FATAL);
  }

  confirmCollectionExists(TxUCMConstant::taskCollection);
  confirmCollectionExists(TxUCMConstant::jobCollection);
  
  taskCollection = store_.execute(TxHandler::SELECT, TxUCMConstant::taskCollection);


  taskPattern = taskCollection->createPattern();
  taskPattern->getField(TxUCMConstant::brokerTaskID)->setValueFromString(brokerTaskID);

  try {
    tasks = taskCollection->execute(TxHandler::SELECT, taskPattern);
  }
  catch (...) {
    for (std::vector<TxRecord*>::iterator iter = tasks.begin();
	 iter != tasks.end();
	 iter++) {
      delete *iter;
    }
    delete taskCollection;
    delete taskPattern;

    throw;
  }

  if (tasks.size() != 1) {
    for (std::vector<TxRecord*>::iterator iter = tasks.begin();
	 iter != tasks.end();
	 iter++) {
      delete *iter;
    }
    delete taskCollection;
    delete taskPattern;

    throw TxUCMException("Job loading error: Found multiple tasks for brokerTaskID: "
			  + brokerTaskID,
			 TxUCMException::FATAL);
  }

  task = tasks[0];
  taskID = task->getField(TxUCMConstant::taskID)->getValue<long>();

  try {
    jobCollection = store_.execute(TxHandler::SELECT, TxUCMConstant::jobCollection);
  }
  catch (...) {
    delete taskCollection;
    delete taskPattern;
    delete task;

    throw;
  }
  
  jobPattern = jobCollection->createPattern();
  jobPattern->getField(TxUCMConstant::brokerJobID)->setValue(brokerJobID);
  jobPattern->getField(TxUCMConstant::taskID)->setValue(taskID);

  try {
    jobs = jobCollection->execute(TxHandler::SELECT, jobPattern);
  }
  catch (...) {
    delete taskCollection;
    delete taskPattern;
    delete task;

    delete jobCollection;
    delete jobPattern;
    for (std::vector<TxRecord*>::iterator iter = jobs.begin();
	 iter != jobs.end();
	 iter++) {
      delete *iter;
    }

    throw;
  }

  if (jobs.size() != 1) {
    delete taskCollection;
    delete taskPattern;
    delete task;

    delete jobCollection;
    delete jobPattern;
    for (std::vector<TxRecord*>::iterator iter = jobs.begin();
	 iter != jobs.end();
	 iter++) {
      delete *iter;
    }

    std::ostringstream converter;
    converter << brokerJobID;

    throw TxUCMException("Job loading error: Found multiple jobs for brokerJobID: " +
 			  converter.str(),
			 TxUCMException::FATAL);
  }

  job_ = jobs[0];
  job_->getField(TxUCMConstant::jobID)->setIgnore(false);

  delete taskCollection;
  delete taskPattern;
  delete task;

  delete jobCollection;
  delete jobPattern;
}


TxTrackingAPI::TxJob::~TxJob()
{
  delete job_;
}


std::string TxTrackingAPI::TxJob::value(JobProperty prop) const {
  TxField* field = job_->getField(jobPropertyToString(prop));
  return field->getValueAsString();
}


void TxTrackingAPI::TxJob::setProperty(JobProperty prop, const std::string& value) {
  // Some properties are not user-set
  if (prop == START_TIME ||
      prop == UPDATE_TIME) {
    throw TxUCMException("Cannot set START or UPDATE properties for job");
  }
  
  TxRecord* updatedJob = new TxRecord(*job_);
  TxField* field = updatedJob->getField(jobPropertyToString(prop));

  // Perform type checking
  switch (field->getType()) {
    case TxField::INT:
    case TxField::LONG:
    case TxField::BOOL:
    {
      std::istringstream convertToType(value);
      std::ostringstream convertFromType;
      
      long typedValue;
      convertToType >> typedValue;
      convertFromType << typedValue;

      if (value != convertFromType.str()) {
	throw TxUCMException("Error setting property '" + jobPropertyToString(prop) 
			      + "' with value '" + value + "': Type mismatch.");
      }
      break;
    }
    case TxField::UINT:
    case TxField::ULONG:
    {
      std::istringstream convertToType(value);
      std::ostringstream convertFromType;
      
      unsigned long typedValue;
      convertToType >> typedValue;
      convertFromType << typedValue;

      if (value != convertFromType.str()) {
	throw TxUCMException("Error setting property '" + jobPropertyToString(prop) 
			      + "' with value '" + value + "': Type mismatch.");
      }
      break;
    }
    case TxField::DOUBLE:
    {
      std::istringstream convertToType(value);
      std::ostringstream convertFromType;
      
      double typedValue;
      convertToType >> typedValue;
      convertFromType << typedValue;

      if (value != convertFromType.str()) {
	throw TxUCMException("Error setting property '" + jobPropertyToString(prop) 
			      + "' with value '" + value + "': Type mismatch.");
      }
      break;
    }
    case TxField::CHAR:
      if ( value.size() != 1 ) {
	throw TxUCMException("Error setting property '" + jobPropertyToString(prop) 
			      + "' with value '" + value + "': Type mismatch.");
      }
      break;
    case TxField::STRING:
      // Do nothing; string's okay
      break;
    default:
      throw TxUCMException("Attempted to set property on column of unknown type.");
      break;
  }

  // Date is necessary to prevent CURRENT_TIME mismatches
  time_t now;
  time(&now);
  char date[20]; // Dates will always be exactly 19 characters in size (YYYY-MM-DD HH:MM:SS) + \0
  strftime(date, 20, "%F %T", localtime(&now));
  
  field->setValueFromString(value);
  updatedJob->getField(TxUCMConstant::updateTime)->setValueFromString(date);

  // When the state is set to '4' for the first time, start the job.
  if (prop == STATE && !started_ && value == "4") {
    started_ = true;
    updatedJob->getField(TxUCMConstant::startTime)->setValueFromString(date);
  }

  TxCollectionHandler* jobCollection = store_.execute(TxHandler::SELECT, TxUCMConstant::jobCollection);
  jobCollection->execute(TxHandler::UPDATE, job_, updatedJob);

  delete job_;
  delete jobCollection;
  job_ = updatedJob;
}


std::string TxTrackingAPI::TxJob::jobPropertyToString(JobProperty prop) const {
  switch (prop) {
    case GRID_JOB_ID:
      return TxUCMConstant::gridJobID;
      break;
    case LOCAL_JOB_ID:
      return TxUCMConstant::localJobID;
      break;
    case GRID_SUBMIT_TIME:
      return TxUCMConstant::gridSubmitTime;
      break;
    case LOCAL_SUBMIT_TIME:
      return TxUCMConstant::localSubmitTime;
      break;
    case SITE:
      return TxUCMConstant::siteLocation;
      break;
    case QUEUE:
      return TxUCMConstant::queue;
      break;
    case QUEUE_POSITION:
      return TxUCMConstant::queuePosition;
      break;
    case NODE:
      return TxUCMConstant::nodeLocation;
      break;
    case EXECUTING_USER:
      return TxUCMConstant::executionUserName;
      break;
    case STATE:
      return TxUCMConstant::stateID;
      break;
    case START_TIME:
      return TxUCMConstant::startTime;
      break;
    case UPDATE_TIME:
      return TxUCMConstant::updateTime;
      break;
    default:
    {
      std::ostringstream converter;
      converter << prop;
      throw TxUCMException("Could not find property identified by ID: " + converter.str());
    }
      
  }
}


// TODO: Add checking for field existence as well.
void TxTrackingAPI::TxJob::confirmCollectionExists(const std::string& name) const {
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
