/***************************************************************************
 *
 * $Id: StDbServer.cc,v 1.10 2000/02/15 20:27:44 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Server class for DB-access
 *
 ***************************************************************************
 *
 * $Log: StDbServer.cc,v $
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
#include "mysqlAccessor.hh"
#include "StDbManager.hh"
#include "StDbTable.h"
#include "StDbConfigNode.hh"
#include <strstream.h>

////////////////////////////////////////////////////////////////

StDbServer::StDbServer(StDbServer& server) :  mserverName(0), mhostName(0), munixSocket(0), mdbName(0),  mdomainName(0), mtypeName(0), mconnectState(false), misDefault(false), mdatabase(0) {

 char * name;
 name = server.getServerName(); setServerName(name);  if(name) delete [] name;
 name = server.getHostName();   setHostName(name);    if(name) delete [] name;
 name = server.getUnixSocket(); setUnixSocket(name);  if(name) delete [] name;
 name = server.getDbName();     setDbName(name);      if(name) delete [] name;
 name = server.getDomainName(); setDomainName(name);  if(name) delete [] name;
 name = server.getTypeName();   setTypeName(name);    if(name) delete [] name;

 setPortNumber(server.getPortNumber());
 

}

////////////////////////////////////////////////////////////////

StDbServer::StDbServer(StDbType type, StDbDomain domain, const char* typeName, const char* domainName){

  mdbName = 0;
  mdatabase = 0;
  mconnectState = false;

  setDataBase(type,domain,typeName,domainName);

}

////////////////////////////////////////////////////////////////

StDbServer::StDbServer(StDbType type, StDbDomain domain){

  mdbName = 0;
  mdatabase = 0;
  mconnectState = false;

  char* typeName = StDbManager::Instance()->getDbTypeName(type);
  char* domainName = StDbManager::Instance()->getDbDomainName(domain);
  setDataBase(type,domain,typeName,domainName);
 
  delete [] typeName; delete [] domainName;
 
}

////////////////////////////////////////////////////////////////

bool
StDbServer::initServer(){

if(!mdbName){
   cerr << "StDbServer:: DataBase not Identified" << endl;
   cerr << "port = "<< mportNumber;
   cerr << " type = " << (int)mdbType <<" domain = " << (int)mdbDomain << endl;
   if(mserverName) cerr << " server = " << mserverName;
   if(mhostName) cerr << " host = " << mhostName;
   cerr<<endl;
   return false;
}

if(!mconnectState){
  if(!mdatabase)mdatabase = new mysqlAccessor(mdbType, mdbDomain);
  if(mdatabase->initDbQuery(mdbName,mserverName,mhostName,mportNumber)){
     mconnectState = true;
     if(!StDbManager::Instance()->IsQuiet()){
       cout << "Server connecting to DB =" << mdbName ;
       cout << " On Host = " << mhostName << endl; 
     }
  }
}


return true;
}

////////////////////////////////////////////////////////////////
bool
StDbServer::reConnect(){ return initServer();}


////////////////////////////////////////////////////////////////

StDbServer::~StDbServer(){

   if(mserverName)delete [] mserverName;
   if(mhostName)delete [] mhostName;
   if(munixSocket)delete [] munixSocket;
   if(mdbName)delete [] mdbName;
   if(mdomainName) delete [] mdomainName;
   if(mtypeName) delete [] mtypeName;
   if(mdatabase) delete mdatabase;

}

////////////////////////////////////////////////////////////////

void
StDbServer::setDbName(const char* dbName){

if(!dbName)return;

mdbName = new char[strlen(dbName)+1];
strcpy(mdbName,dbName);

char* id = strstr(mdbName,"_");

 if(!id){

   mdomainName = new char[5];
   strcpy(mdomainName,"Star");
   mtypeName = new char[strlen(dbName)+1];
   strcpy(mtypeName,dbName);

 } else {

   int iloc = id-mdbName;
   mtypeName = new char[iloc+1];
   strncpy(mtypeName,mdbName,iloc);
   mtypeName[iloc]='\0';

   iloc = strlen(dbName)-iloc;
   id++;
   mdomainName = new char[iloc+1];
   strncpy(mdomainName,id,iloc);
   mdomainName[iloc]='\0';

 }

 mdbType = StDbManager::Instance()->getDbType(mtypeName);
 mdbDomain = StDbManager::Instance()->getDbDomain(mdomainName);
  
}

////////////////////////////////////////////////////

void
StDbServer::setDataBase(StDbType type, StDbDomain domain, const char* typeName, const char* domainName) {

  mdbType = type;
  mdbDomain = domain;
  if(mtypeName) delete [] mtypeName;
  if(mdomainName) delete [] mdomainName;

  mtypeName = new char[strlen(typeName)+1];
  strcpy(mtypeName,typeName);

  mdomainName = new char[strlen(domainName)+1];
  strcpy(mdomainName,domainName);

  int len = strlen(mtypeName);

  if(mdomainName && strcmp(mdomainName,"Star") != 0){
    len+=strlen(mdomainName);
    len++;
  }

  mdbName = new char[len+1];
  ostrstream ost(mdbName,len+1);

  ost << mtypeName;
  if(mdomainName && strcmp(mdomainName,"Star") != 0) ost << "_" << mdomainName;
  ost << ends;
  
  //StDbManager::Instance()->setVerbose(true);

  if(StDbManager::Instance()->IsVerbose()) {
   cout << "New Server to db " << mdbName;
   cout << " DB Type   = "<<mtypeName;
   cout <<"  DB Domain = "<< mdomainName<<endl;
  }

}


////////////////////////////////////////////////////

void
StDbServer::setHostName(const char* name){

if(!name)return;

if(mhostName)delete [] mhostName;
mhostName = new char[strlen(name)+1];
strcpy(mhostName,name);

}

////////////////////////////////////////////////////

void
StDbServer::setUnixSocket(const char* name){

if(!name)return;

if(munixSocket)delete [] munixSocket;
munixSocket = new char[strlen(name)+1];
strcpy(munixSocket,name);

}

////////////////////////////////////////////////////

void
StDbServer::setDomainName(const char* name){

if(!name)return;

if(mdomainName)delete [] mdomainName;
mdomainName = new char[strlen(name)+1];
strcpy(mdomainName,name);

}

////////////////////////////////////////////////////

void
StDbServer::setTypeName(const char* name){

if(!name)return;

if(mtypeName)delete [] mtypeName;
mtypeName = new char[strlen(name)+1];
strcpy(mtypeName,name);

}

////////////////////////////////////////////////////

void
StDbServer::setServerName(const char* name){

if(!name)return;

if(mserverName)delete [] mserverName;
mserverName = new char[strlen(name)+1];
strcpy(mserverName,name);

}

////////////////////////////////////////////////////

char*
StDbServer::getHostName() const {

char* name = 0;
if(mhostName) name = mstringDup(mhostName);

return name;
}

////////////////////////////////////////////////////

char*
StDbServer::getUnixSocket() const {

char* name = 0;
if(munixSocket) name = mstringDup(munixSocket);

return name;
}

////////////////////////////////////////////////////

char*
StDbServer::getDomainName() const {

char* name = 0;
if(mdomainName) name = mstringDup(mdomainName);

return name;
}

////////////////////////////////////////////////////

char*
StDbServer::getTypeName() const {

char* name = 0;
if(mtypeName) name = mstringDup(mtypeName);

return name;
}


////////////////////////////////////////////////////

char*
StDbServer::getDbName() const {

char* name = 0;
if(mdbName) name = mstringDup(mdbName);

return name;
}

////////////////////////////////////////////////////

char*
StDbServer::getServerName() const {

char* name = 0;
if(mserverName) name = mstringDup(mserverName);

return name;
}

////////////////////////////////////////////////////////////////

unsigned int 
StDbServer::getUnixTime(const char* time) { 
return mdatabase->getUnixTime(time);
}

////////////////////////////////////////////////////////////////

char* 
StDbServer::getDateTime(unsigned int time){
return mdatabase->getDateTime(time);
}


////////////////////////////////////////////////////////////////

bool
StDbServer::QueryDb(StDbTable* table, unsigned int reqTime) { 

  if(!mconnectState) initServer();
  if(!mconnectState){ connectError(); return false; }

  if(!mdatabase->QueryDb(table,reqTime)){
    cerr<<"WARNING:: ";
    if(table) cerr << "Read table ["<<table->getMyName()<<"] Failed. "; 
    cerr<< " Data is not Filled from DB" << endl;
    return false;
  }

return true;
}

////////////////////////////////////////////////////////////////

bool 
StDbServer::QueryDb(StDbTable* table, const char* whereClause) { 

  if(!mconnectState) initServer();
  if(!mconnectState){ connectError(); return false; }

  if(!mdatabase->QueryDb(table,whereClause)){
    cerr<<"WARNING:: ";
    if(table) cerr << "Read table ["<<table->getMyName()<<"] Failed. "; 
    cerr<< " Data is not Filled from DB " << endl;
    return false;
  }

return true;
}

////////////////////////////////////////////////////////////////

bool 
StDbServer::WriteDb(StDbTable* table, unsigned int storeTime) { 

  if(!mconnectState) initServer();
  if(!mconnectState){ connectError(); return false; }

bool retVal = false;
 
 if(!mdatabase->WriteDb(table, storeTime)){
   if(table) cout << "Write table ["<<table->getName()<<"] Failed"<<endl;
   return retVal;
  }

return true;
}


///////////////////////////////////////////////////////////////////////

bool
StDbServer::QueryDb(StDbConfigNode* node) { 

  if(!mconnectState) initServer();
  if(!mconnectState){ connectError(); return false; }
  if(!mdatabase->QueryDb(node))return false;

return true;
}

///////////////////////////////////////////////////////////////////////

bool
StDbServer::QueryDb(StDbNode* node) { 

  if(!mconnectState) initServer();
  if(!mconnectState){ connectError(); return false; }
  if(!mdatabase->QueryDb(node))return false;

return true;
}

////////////////////////////////////////////////////////////////

int
StDbServer::WriteDb(StDbConfigNode* node,int currentID) { 

  if(!mconnectState) initServer();
if(!mconnectState){ connectError(); return 0; }
return mdatabase->WriteDb(node,currentID);

}

////////////////////////////////////////////////////////////////

bool
StDbServer::rollBack(StDbNode* node) { 

  if(!mconnectState) initServer();
  if(!mconnectState){ connectError(); return false; }
return mdatabase->rollBack(node);

}

////////////////////////////////////////////////////////////////

bool
StDbServer::rollBack(StDbTable* table) { 

  if(!mconnectState) initServer();
if(!mconnectState){ connectError(); return false; }
return mdatabase->rollBack(table);

}


////////////////////////////////////////////////////////////////

bool
StDbServer::QueryDescriptor(StDbTable* table) { 

  if(!mconnectState) initServer();
  if(!mconnectState){ connectError(); return false; }
  if(!mdatabase->QueryDescriptor(table)){
    if(table) cout << "table ["<<table->getName()<<"] ";
    cout << "Table Descriptor is not found " << endl;
    return false;
  }
return true;
}

/////////////////////////////////////////////////////////////////////

char*
StDbServer::mstringDup(const char* str) const {

char* retString=0;
if(!str)return retString;
retString = new char[strlen(str)+1];
strcpy(retString,str);

return retString;
}












