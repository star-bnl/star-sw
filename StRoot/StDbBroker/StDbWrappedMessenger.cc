/***************************************************************************
 *
 * $Id: StDbWrappedMessenger.cc,v 1.2 2001/01/26 14:48:41 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Implements the abstract StDbMessService class with
 *              a pointer to the StUtilities singleton StMessageManager
 *              to make use of the general STAR offline messaging utility
 *
 ***************************************************************************
 *
 * $Log: StDbWrappedMessenger.cc,v $
 * Revision 1.2  2001/01/26 14:48:41  porter
 * fixed tag so verbose output works correctly
 *
 * Revision 1.1  2001/01/22 18:40:25  porter
 * Added a wrapper for StMessage so one can use it in StDbLib
 *
 **************************************************************************/

#include "StDbWrappedMessenger.hh"
#include <string.h>
#include "StUtilities/StMessageManager.h"

//////////////////////////////////////////////////////////////////////////

StDbWrappedMessenger::StDbWrappedMessenger() {
  mMessenger=StMessageManager::Instance();
};

//////////////////////////////////////////////////////////////////////////

void
StDbWrappedMessenger::printMessage(const char* message, StDbMessLevel dbLevel, int lineNumber, const char* className, const char* methodName) {

if(dbLevel<mdbLevel)return;

 char lString[64];
 switch(dbLevel){
 case dbMDebug:
   {
      strcpy(lString,"I");
      break;
   }
 case dbMWarn:
   {
      strcpy(lString,"W");
      break;
   }
 case dbMConnect:
   {
      strcpy(lString,"I");
      break;
   }
 case dbMErr:
   {
     strcpy(lString,"E");
     break;
   }
 default:
   { 
     strcpy(lString," ");
     break;
   }
 }

printMessage(message,(const char*)lString,lineNumber,className,methodName);
}

//////////////////////////////////////////////////////////////////////////

void 
StDbWrappedMessenger::printMessage(const char* message, const char* levelString, int lineNumber, const char* className, const char* methodName) {

  ostrstream mtxt;  
  mtxt<<className<<"::"<<methodName<<" line="<<lineNumber<<" "<<message<<ends;
  mMessenger->Message(mtxt.str(),levelString);
  delete mtxt.str();

}


