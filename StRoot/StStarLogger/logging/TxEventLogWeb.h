/*
 * @file TxEventLogWeb.h
 * @author Valeri Fine
 *
 * @(#)cpp/api:$Id: TxEventLogWeb.h,v 1.6 2010/09/17 19:34:54 fine Exp $
 *
 * TxEventLogWeb provides an interface for applications so that they can send
 * event across of the Web into a CEDPS formated messages.
 */
 
#ifndef TX_EVENT_LOG_WEB_H
#define TX_EVENT_LOG_WEB_H

#include "TxEventLogFile.h"


namespace TxLogging {
  class TxEventLogWeb: public TxEventLogFile {
  protected:
    /**
     * Write down the prepared plain message using the Web interface 
     * 
     * @param string message , Message to be written out
     *
     */
    virtual void writeDown(const char *message);
    virtual void writeDown(const std::string& message);

  public:
    /**
     * Constructor
     * - Gets hostname and sets nodeLocation for the running job.  
     *
     */    
    TxEventLogWeb();

    /**
     * Destructor: 
     * - Sets job state to finished (STAGE = END) 
     *
     */
    virtual ~TxEventLogWeb (){}
	// --- 
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
	
 	 virtual  void setDbJobID (int bJobID);

    virtual  int  queryTableSize(const char *tableName);
    virtual  int  queryTableSize(const char *tableName, const char *where);
    virtual  int  queryTableSize(const char *tableName, const StRecord   *where);

	
  };
}
#endif
