/***************************************************************************
 *
 * $Id: StDbWrappedMessenger.cc,v 1.10 2011/01/07 17:12:28 dmitry Exp $
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
 * Revision 1.10  2011/01/07 17:12:28  dmitry
 * fixed pseudo-leaks in c-string and xml-string assignments
 *
 * Revision 1.9  2009/08/25 17:00:43  fine
 * fix the compilation issues under SL5_64_bits  gcc 4.3.2
 *
 * Revision 1.8  2007/08/08 20:51:04  fine
 * replace the cutom messanger with the standard STAR logger
 *
 * Revision 1.7  2006/08/08 14:28:09  deph
 * fixed delete to delete allocated array
 *
 * Revision 1.6  2005/12/08 18:13:27  deph
 * Made message length dynamic. We were bumping up against the 1024 limit in verbose mode.
 *
 * Revision 1.5  2003/09/12 01:48:06  porter
 * removed all strstream objects in favor of stringstream+string directly
 *
 * Revision 1.4  2001/06/05 22:08:34  perev
 * HP corr
 *
 * Revision 1.3  2001/04/23 14:01:58  porter
 * fixed bug in messages
 *
 * Revision 1.2  2001/01/26 14:48:41  porter
 * fixed tag so verbose output works correctly
 *
 * Revision 1.1  2001/01/22 18:40:25  porter
 * Added a wrapper for StMessage so one can use it in StDbLib
 *
 **************************************************************************/

#include "StDbWrappedMessenger.hh"
#include <string.h>
#include <assert.h>
#include "StUtilities/StMessageManager.h"

//////////////////////////////////////////////////////////////////////////

StDbWrappedMessenger::StDbWrappedMessenger() {
  mMessenger=StMessageManager::Instance();
};

//////////////////////////////////////////////////////////////////////////

void
StDbWrappedMessenger::printMessage(const char* message, StDbMessLevel dbLevel, int lineNumber, const char* className, const char* methodName) {

if (dbLevel<mdbLevel) return;

// int n = strlen(message)+1000;
// char * str = new char[n];

 std::ostringstream ostr;
 ostr << className << "::" << methodName << " line=" << lineNumber << " " << message;

 std::string str = ostr.str();

// sprintf(str,"%s::%s line=%d %s",className,methodName,lineNumber,message);

 char lString[64];
 switch(dbLevel){
 case dbMDebug:
   {
      strcpy(lString,"I");
      LOG_DEBUG << str << endm;
      break;
   }
 case dbMWarn:
   {
      strcpy(lString,"W");
      LOG_WARN << str << endm;
      break;
   }
 case dbMConnect:
   {
      strcpy(lString,"I");
      LOG_INFO << str << endm;
      break;
   }
 case dbMErr:
   {
     strcpy(lString,"E");
     LOG_ERROR << str << endm;
     break;
   }
 default:
   { 
     strcpy(lString," ");
     LOG_INFO << str << endm;
     break;
   }
  // delete [] str;
 }

// printMessage(message,(const char*)lString,lineNumber,className,methodName);
}

//////////////////////////////////////////////////////////////////////////

void 
StDbWrappedMessenger::printMessage(const char* message, const char* levelString, int lineNumber, const char* className, const char* methodName) {

  //
  // limit here of 1024 only hits us for the StDbManager in verbose mode 
  // which isn't available directly in StRoot 
  //

//  char str[1024];
  assert(0);
  int n = strlen(message)+1000;
  char * str = new char[n];

  sprintf(str,"%s::%s line=%d %s",className,methodName,lineNumber,message);
  mMessenger->Message(str,levelString);
  delete [] str;
}


