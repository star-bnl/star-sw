/*
 * @file TxTask.h
 * @author Stephen Tramer
 *
 * @(#)cpp/api:$Id: TxJob.h,v 1.1 2008/12/08 21:10:25 fine Exp $
 *
 * TxJob provides an interface for the local scheduler (via the RMD) to
 * write information about job status and progress.
 */
 
#ifndef TX_JOB_H
#define TX_JOB_H

#include <string>

#include "TxStoreHandler.h"
#include "TxRecord.h"

namespace TxTrackingAPI {
  class TxJob {
  public:
    enum JobProperty {
      GRID_JOB_ID,
      LOCAL_JOB_ID,
      GRID_SUBMIT_TIME,
      LOCAL_SUBMIT_TIME,
      SITE,
      QUEUE,
      QUEUE_POSITION,
      NODE,
      EXECUTING_USER,
      STATE,
      START_TIME,
      UPDATE_TIME
    };

  public:

    /**
     * Looks up an entry in the Jobs collection with the provided information.
     * @param brokerTaskID The taskID assigned by the broker
     * @param brokerJobID The jobID assigned by the broker
     */
    TxJob(const std::string& brokerTaskID, long brokerJobID);

    /**
     * Destructor: No-op.  
     */
    ~TxJob();

  
    /**
     * Sets the provided property with the given value.
     * @param prop The property to set.
     * @param value The value to set.
     */
    void setProperty(JobProperty prop, const std::string& value);

  
    /**
     * Returns the specified propery.
     * @param prop The property to retrieve.
     */
    std::string value(JobProperty prop) const;

  private:
    /**
     * @internal
     * Converts a JobProperty into a proper column name.
     * @param The JobProperty to convert.
     */
    std::string jobPropertyToString(JobProperty prop) const;

    /**
     * @internal
     * Confirms that the named collection exists.
     * @param name The name of the collection.
     * @throws TxUCMException If the collection is not found.
     */
    void confirmCollectionExists(const std::string& name) const;

  private:
    TxRecord* job_;
    TxStoreHandler store_;
    bool started_;
  };
}
#endif
