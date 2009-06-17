/*
 * @file TxEventLogWeb.h
 * @author Valeri Fine
 *
 * @(#)cpp/api:$Id: TxEventLogWeb.h,v 1.1 2009/06/17 22:12:00 fine Exp $
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
    virtual void writeDown(const std::string& message);
  
  public:
    /**
     * Constructor
     * - Gets hostname and sets nodeLocation for the running job.  
     *
     */    
    TxEventLogWeb(){}

    /**
     * Destructor: 
     * - Sets job state to finished (STAGE = END) 
     *
     */
    virtual ~TxEventLogWeb (){}
  };
}
#endif
