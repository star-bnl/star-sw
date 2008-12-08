/*
 * @file TxTask.h
 * @author Stephen Tramer
 *
 * @(#)cpp/api:$Id: TxTask.h,v 1.1 2008/12/08 21:10:26 fine Exp $
 *
 * TxTask provides an interface for the broker so that it can create
 * tasks in the store and associate jobs with them.
 */
 
#ifndef TX_TASK_H
#define TX_TASK_H

#include <string>

#include "TxStoreHandler.h"

/**
 * Abstraction for the collection in memory, with facilities to manipulate 
 * collection data in the store.
 */
namespace TxTrackingAPI {
  class TxTask {
  public:
    /**
     * Creates a new task entry in the Tasks collection.  Uses the environment
     * variable UCM_STORE_INFO to connect to the store.
     * @param brokerTaskID The broker task ID
     * @param brokerID The ID of the broker which created the task.  This value is
     *                 associated with an entry in the BrokerDictionary collection.
     * @param requesterID The ID of the requester who created the task.  This value
     *                    is associated with an entry in the RequesterDictionary
     *                    collection.
     * @param taskName An optional name for the task.
     * @param taskDesc An optional description of the task.
     * @throws TxUCMException If unable to connect to the store specified 
     *                        in UCM_STORE_INFO.
     * WARNING: CURRENTLY HACKED TO WORK WITH BNL'S INCORRECT REQUESTERID
     */
    TxTask(const std::string& brokerTaskID, long brokerID, const std::string& requesterID, 
	   const std::string& taskName = "", const std::string& taskDesc = "");

    /**
     * Destructor: No-op.  Store should not be closed unless there is a genuine error,
     * as other API components may be using the store and will behave badly if it is
     * closed before they go out of scope.
     */
    ~TxTask();

    /**
     * Adds a job to the task.  Only sets the jobID, brokerJobID, and taskID.
     * All other job information is set by the Resource Monitoring Daemon (RMD)
     * @param brokerJobID The brokerJobID for the job.
     * @returns The jobID of the newly created job.
     */
    long addJob(long brokerJobID);

    /**
     * Returns taskID.
     */
    long getTaskID() const;

  private:
    /**
     * @internal
     * Confirms that the named collection exists.
     * @param name The name of the collection.
     * @throws TxUCMException If the collection is not found.
     */
    void confirmCollectionExists(const std::string& name);

  private:
    long taskID_;
    TxStoreHandler store_;
  };
}
#endif
