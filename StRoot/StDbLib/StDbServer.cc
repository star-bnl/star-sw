#include "StDbServer.hh"
#include "mysqlAccessor.hh"
#include "StDbManager.hh"
#include "StDbTable.h"
#include "StDbConfigNode.hh"
#include <strstream.h>

////////////////////////////////////////////////////////////////

StDbServer::StDbServer(StDbServer& server) : mserverName(0), mhostName(0), munixSocket(0), mdomainName(0), mtypeName(0), mdbName(0), mconnectState(false), misDefault(false) {

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

void
StDbServer::initServer(){

  if(!mdbName){
    cerr << "StDbServer:: DataBase not Identified" << endl;
  } else {

cout << "Server connecting to database " << mdbName ;
cout << " on host = " << mhostName << endl;

    mdatabase = new mysqlAccessor();
    //    mdatabase->initDbQuery(mdbName,"dummy","duvall.star.bnl.gov", 0);
    mdatabase->initDbQuery(mdbName,mserverName,mhostName, 0);
    mconnectState = true;

  }
}

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

StDbServer::~StDbServer(){

   if(mserverName)delete [] mserverName;
   if(mhostName)delete [] mhostName;
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
 //cout << "Type = "<<mtypeName<<" & Domain = "<<mdomainName<<endl;
 //cout << "Type = "<<mdbType<<" & Domain = "<<mdbDomain<<endl;
 
  
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
if(mserverName) {
name = new char[strlen(mserverName)+1];
strcpy(name,mserverName);
//name = mstringDup(mserverName);
}

return name;
}

////////////////////////////////////////////////////////////////

void 
StDbServer::QueryDb(StDbTable* table) { 

char* name=0;
  if(!mdatabase->QueryDb(table)){
    if(table){
      if((name = table->getTableName())){
        cout << "table ["<<name<<"] ";
       delete [] name;
      }
    cout << "Table is not Updated" << endl;
    }
  }

}

////////////////////////////////////////////////////////////////

void 
StDbServer::WriteDb(StDbTable* table) { 

  if(!mdatabase->WriteDb(table)){
    if(table) cout << "Wrote table ["<<table->getTableName()<<"] ";
    cout << "Table is not Updated" << endl;
  }
}



////////////////////////////////////////////////////////////////

void 
StDbServer::QueryDescriptor(StDbTable* table) { 

  if(!mdatabase->QueryDescriptor(table)){
    if(table) cout << "table ["<<table->getTableName()<<"] ";
    cout << "Table Descriptor is not found " << endl;
  }
}


///////////////////////////////////////////////////////////////////////

void 
StDbServer::QueryDb(StDbConfigNode* node) { 

  // simple call to database navigation
  mdatabase->QueryDb(node);

}

/////////////////////////////////////////////////////////////////////

char*
StDbServer::mstringDup(const char* str) const {

if(!str)return str;
char* retString = new char[strlen(str)+1];
strcpy(retString,str);

return retString;
}






