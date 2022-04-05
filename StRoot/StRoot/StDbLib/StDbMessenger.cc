/***************************************************************************
 *
 * $Id: StDbMessenger.cc,v 1.4 2004/01/15 00:02:25 fisyak Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Implementation of StDbMessService used within StDbLib
 *               in stand-alone mode
 *
 ***************************************************************************
 *
 * $Log: StDbMessenger.cc,v $
 * Revision 1.4  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.3  2003/09/16 22:44:17  porter
 * got rid of all ostrstream objects; replaced with StString+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.2  2003/09/02 17:57:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2001/01/22 18:37:57  porter
 * Update of code needed in next year running. This update has little
 * effect on the interface (only 1 method has been changed in the interface).
 * Code also preserves backwards compatibility so that old versions of
 * StDbLib can read new table structures.
 *  -Important features:
 *    a. more efficient low-level table structure (see StDbSql.cc)
 *    b. more flexible indexing for new systems (see StDbElememtIndex.cc)
 *    c. environment variable override KEYS for each database
 *    d. StMessage support & clock-time logging diagnostics
 *  -Cosmetic features
 *    e. hid stl behind interfaces (see new *Impl.* files) to again allow rootcint access
 *    f. removed codes that have been obsolete for awhile (e.g. db factories)
 *       & renamed some classes for clarity (e.g. tableQuery became StDataBaseI
 *       and mysqlAccessor became StDbSql)
 *
 *
 **************************************************************************/
#include <string.h>
#include "stdb_streams.h"
#include "StDbMessenger.hh"

////////////////////////////////////////////////////////////////////////////

void
StDbMessenger::printMessage(const char* message, StDbMessLevel dbLevel, int lineNumber, const char* className, const char* methodName) {

if(dbLevel<mdbLevel)return;

 char lString[64];
 switch(dbLevel){
 case dbMDebug:
   {
      strcpy(lString,"Debug");
      break;
   }
 case dbMWarn:
   {
      strcpy(lString,"Warning");
      break;
   }
 case dbMConnect:
   {
      strcpy(lString,"Info");
      break;
   }
 case dbMErr:
   {
     strcpy(lString,"Error");
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

//////////////////////////////////////////////////////////////////////////////

void 
StDbMessenger::printMessage(const char* message, const char* levelString, int lineNumber, const char* className, const char* methodName) {
  mos<<" *** "<<levelString<<" line="<<lineNumber<<" *** ";
  mos<<className<<"::"<<methodName<<" "<<endl<<message<<endl;
}





