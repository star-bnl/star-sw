#include "StDbServer.hh"
#include "mysqlAccessor.hh"
#include "StDbManager.hh"
#include "StDbTable.h"
#include "StDbConfigNode.hh"
#include <strstream.h>

////////////////////////////////////////////////////////////////

StDbServer::StDbServer(StDbType type, StDbDomain domain, const char* typeName, const char* domainName){

  mdbType = type;
  mdbDomain = domain;
  mdbName = 0;
  mconnectState = false;
  mdatabase = 0;

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
   initServer();
}

////////////////////////////////////////////////////////////////

StDbServer::StDbServer(StDbType type, StDbDomain domain){

  mdbType = type;
  mdbDomain = domain;
  mdbName = 0;
  mdatabase = 0;
  mconnectState = false;

  mtypeName = StDbManager::Instance()->getDbTypeName(type);
  mdomainName = StDbManager::Instance()->getDbDomainName(domain);

  int len = strlen(mtypeName);
  if(mdomainName && strcmp(mdomainName,"Star") != 0){
    len+=strlen(mdomainName);
    len++;
  }
  mdbName = new char[len+1];
  ostrstream ost(mdbName,len);
  ost << mtypeName;
  if(mdomainName && strcmp(mdomainName,"Star") != 0) ost << "_" << mdomainName << ends;
  //  initServer();

}

////////////////////////////////////////////////////////////////

void
StDbServer::initServer(){

  if(!mdbName){
    cerr << "StDbServer:: DataBase not Identified" << endl;
  } else {

    // just now for testing only one duvall.star.bnl.gov.....

    cout << "Server connection to database " << mdbName << endl;

    mdatabase = new mysqlAccessor();
    mdatabase->initDbQuery(mdbName,"dummy","duvall.star.bnl.gov", 0);
    mconnectState = true;

  }
}

////////////////////////////////////////////////////////////////

StDbServer::StDbServer(const char* server, const char* hostname, int port)
{
   mserverName = new char[strlen(server)+1];
   strcpy(mserverName,server);
   mhostName = new char[strlen(hostname)+1];
   strcpy(mhostName,hostname);
   mportNumber = port;
   mconnectState = false;

   // Set munixSocket to /tmp/mysql/ServerNamePortNumber.Socket 
   setUnixSocket();
   mdatabase = 0;
   //   if(port!=0)initServer();
 
}

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
StDbServer::setUnixSocket(){

   char tempstr[1024];
   ostrstream ost(tempstr,1024);
   ost << "/tmp/mysql/" << mserverName << mportNumber << ".sock" << ends;
   munixSocket = new char[strlen(tempstr)+1];
   strcpy(munixSocket,tempstr);   

}

////////////////////////////////////////////////////////////////

char *
StDbServer::getServerName() const {
return strdup(mserverName);
}

////////////////////////////////////////////////////////////////

char* 
StDbServer::getHostName() const {
return strdup(mhostName);
}

////////////////////////////////////////////////////////////////

int
StDbServer::getPortNumber() const {
return mportNumber;
}

////////////////////////////////////////////////////////////////

void 
StDbServer::QueryDb(StDbTable* table) { 

  if(!mdatabase->QueryDb(table)){
    if(table) cout << "table ["<<table->getTableName()<<"] ";
    cout << "Table is not Updated" << endl;
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








