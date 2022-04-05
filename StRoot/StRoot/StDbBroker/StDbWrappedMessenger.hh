/***************************************************************************
 *
 * $Id: StDbWrappedMessenger.hh,v 1.1 2001/01/22 18:40:25 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Implements the abstract StDbMessService class with
 *              a pointer to the StUtilities singleton StMessageManager
 *              to make use of the general STAR offline messaging utility
 *
 ***************************************************************************
 * $Log: StDbWrappedMessenger.hh,v $
 * Revision 1.1  2001/01/22 18:40:25  porter
 * Added a wrapper for StMessage so one can use it in StDbLib
 *
 **************************************************************************/
#ifndef STDBWRAPPEDMESSENGER_HH
#define STDBWRAPPEDMESSENGER_HH

#include "StDbLib/StDbMessService.hh"

class StMessMgr;

class StDbWrappedMessenger : public StDbMessService {

 protected:

  StMessMgr* mMessenger;

 public:

  StDbWrappedMessenger();
  virtual ~StDbWrappedMessenger(){};

  virtual void printMessage(const char* message,
                            StDbMessLevel dbLevel,
                            int lineNumber,
                            const char* className, 
                            const char* methodName) ;

  virtual void printMessage(const char* message,
                            const char* levelString,
                            int lineNumber,
                            const char* className, 
                            const char* methodName)  ;


};


#endif
