/***************************************************************************
 *
 * $Id: StDbServer.cc,v 1.14 2001/01/22 18:37:58 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Server class for DB-access
 *
 ***************************************************************************
 *
 * $Log: StDbServer.cc,v $
 * Revision 1.14  2001/01/22 18:37:58  porter
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
 * Revision 1.13  2000/08/15 22:51:52  porter
 * Added Root2DB class from Masashi Kaneta
 * + made code more robust against requesting data from non-existent databases
 *
 * Revision 1.12  2000/03/01 20:56:16  porter
 * 3 items:
 *    1. activated reConnect for server timeouts
 *    2. activated connection sharing; better resource utilization but poorer
 *       logging
 *    3. made rollback method in mysqlAccessor more robust (affects writes only)
 *
 * Revision 1.11  2000/02/24 20:30:47  porter
 * fixed padding for uchar; beginTime in mysqlAccessor;
 * added rollback safety checkes in StDbManger
 *
 * Revision 1.10  2000/02/15 20:27:44  porter
 * Some updates to writing to the database(s) via an ensemble (should
 * not affect read methods & haven't in my tests.
 *  - closeAllConnections(node) & closeConnection(table) method to mgr.
 *  - 'NullEntry' version to write, with setStoreMode in table;
 *  -  updated both StDbTable's & StDbTableDescriptor's copy-constructor
 *
 * Revision 1.9  2000/01/27 05:54:34  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
 * Revision 1.8  2000/01/19 20:20:06  porter
 * - finished transaction model needed by online
 * - fixed CC5 compile problem in StDbNodeInfo.cc
 * - replace TableIter class by StDbTableIter to prevent name problems
 *
 * Revision 1.7  2000/01/10 20:37:54  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.6  1999/12/03 22:24:01  porter
 * expanded functionality used by online, fixed bug in
 * mysqlAccessor::getElementID(char*), & update StDbDataSet to
 * conform to changes in Xml reader & writer
 *
 * Revision 1.5  1999/09/30 02:06:08  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "StDbServer.hh"
#include "StDbTable.h"
#include "StDbConfigNode.hh"
#include <string.h>

#define __CLASS__ "StDbServer"

////////////////////////////////////////////////////////////////

StDbServer::StDbServer(): mserverName(0),mhostName(0), munixSocket(0), muserName(0), mpword(0), misDefault(false){};

StDbServer::StDbServer(const char* name, const char* host, const char* sock, int port) : mserverName(0),mhostName(0), munixSocket(0), muserName(0), mpword(0), mportNumber(port), misDefault(false) {

  setServerName(name);
  setHostName(host);
  setUnixSocket(sock);
};
   
StDbServer::StDbServer(StDbServer& server) :  mserverName(0), mhostName(0), munixSocket(0) , muserName(0), mpword(0), misDefault(false) {

  setServerName(server.printServerName());
  setHostName(server.printHostName());
  setUnixSocket(server.printUnixSocket());
  setPortNumber(server.getPortNumber());
}
////////////////////////////////////////////////////////////////

StDbServer::~StDbServer(){
   if(mserverName)delete [] mserverName;
   if(mhostName)  delete [] mhostName;
   if(munixSocket)delete [] munixSocket;
   if(muserName)  delete [] muserName;
   if(mpword)     delete [] mpword;
}
////////////////////////////////////////////////////

void StDbServer::setHostName(const char* name)  {mhostName=mstringDup(name); }
void StDbServer::setUnixSocket(const char* name){munixSocket=mstringDup(name);}
void StDbServer::setServerName(const char* name){mserverName=mstringDup(name);}

////////////////////////////////////////////////////

void
StDbServer::setUser(const char* name, const char* pword){
  muserName=mstringDup(name);
  mpword=mstringDup(pword);
}

////////////////////////////////////////////////////

char* StDbServer::getHostName() const   { return mstringDup(mhostName); };
char* StDbServer::getUnixSocket() const { return mstringDup(munixSocket); }
char* StDbServer::getServerName() const { return mstringDup(mserverName); }

////////////////////////////////////////////////////

char* StDbServer::mstringDup(const char* str) const {

char* retString=0;
if(!str)return retString;
retString = new char[strlen(str)+1];
strcpy(retString,str);
return retString;
}

#undef __CLASS__
