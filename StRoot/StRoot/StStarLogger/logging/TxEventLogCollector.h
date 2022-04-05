/*
 * @file TxEventLogCollector.h
 * @author Valeri Fine
 *
 * @(#)cpp/api:$Id: TxEventLogCollector.h,v 1.6 2010/09/17 19:34:54 fine Exp $
 *
 * TxEventLogCollector provides an interface for applications so that they can send
 * event across of the Collector into a CEDPS formated messages.
 */
 
#ifndef TX_EVENT_LOG_COLLECTOR_H
#define TX_EVENT_LOG_COLLECTOR_H

#include "TxEventLogFile.h"
namespace TxLogging {
class TxUCMCollector;
}
namespace TxLogging {
  class TxEventLogCollector: public TxEventLogFile {
  private:
    TxUCMCollector *fCollector;
    StUcmTasks     *fTasks;  ///< Current tasks
    StUcmJobs      *fJobs;   ///< Current jobs
    StUcmEvents    *fEvents; ///< Current events  
    int             fDbInit; ///< Init Db flag
  protected:
    /**
     * Write down the prepared plain message using the Collector interface 
     * 
     * @param string message , Message to be written out
     *
     */
    virtual void writeDown(const std::string& message);
    virtual void writeDown(const char *message);
    virtual void InitDb();
  
  public:
    /**
     * Constructor
     * - Gets hostname and sets nodeLocation for the running job.  
     *
     */    
    TxEventLogCollector();

    /**
     * Destructor: 
     * - Sets job state to finished (STAGE = END) 
     *
     */
    virtual ~TxEventLogCollector ();
	

      virtual  StUcmTasks  *getTaskList ();
      virtual  StUcmTasks  *getTaskList (int limit);
      virtual  StUcmTasks  *getTaskList (int limit, int offset); 
	  
      virtual  StUcmJobs   *getJobList();
      virtual  StUcmJobs   *getJobList(StRecord *task);
      virtual  StUcmJobs   *getJobList(StRecord *task, int limit);
      virtual  StUcmJobs   *getJobList(int limit);
      virtual  StUcmJobs   *getJobList(int limit, int offset);
      virtual  StUcmJobs   *getJobList(StRecord *task, int limit, int offset);
      virtual  int          getJobId(const char *reqName, const char *taskBrokerId, int jobBrokerId);

      virtual  StUcmEvents *getEventList();
      virtual  StUcmEvents *getEventList(StRecord *job);
      virtual  StUcmEvents *getEventList(StRecord *job,int limit);
      virtual  StUcmEvents *getEventList(int limit);
      virtual  StUcmEvents *getEventList(int limit, int offset);
      virtual  StUcmEvents *getEventList(StRecord *job,int limit, int offset);
	
    virtual  void setBrokerTaskID (const std::string& brokerTaskID);
    virtual  void setBrokerJobID (int bJobID);
    virtual  void setDbJobID (int bJobID);
    virtual  void setRequesterName (const std::string& reqName);
	
    virtual  int  queryTableSize(const char *tableName);
    virtual  int  queryTableSize(const char *tableName, const char *where);
    virtual  int  queryTableSize(const char *tableName, const StRecord   *where);
	
  };
}
#endif
