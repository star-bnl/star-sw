#include "StDbServer.hh"
#include "mysqlAccessor.hh"
#include "StDbManager.hh"
#include "StDbTableComponent.h"
#include "StDbConfigNode.hh"
#include <strstream.h>

////////////////////////////////////////////////////////////////

StDbServer::StDbServer(StDbType type, StDbDomain domain, const char* typeName, const char* domainName){

  mdbType = type;
  mdbDomain = domain;
  mdbName = 0;
  mconnectState = false;
  mdatabase = 0;

  char* mtypeName = new char[strlen(typeName)+1];
  strcpy(mtypeName,typeName);
  char* mdomainName = new char[strlen(domainName)+1];
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

  char* mtypeName = StDbManager::Instance()->getDbTypeName(type);
  char* mdomainName = StDbManager::Instance()->getDbDomainName(domain);


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
    cerr << "DataBase not Identified" << endl;
  } else {

    // just now for testing only one duvall.star.bnl.gov.....

    cout << "Server connection to database " << mdbName << endl;
    //   mquery.init(mdbName,"dummy","localhost",0);   
    mdatabase = new mysqlAccessor();
    mdatabase->initDbQuery(mdbName,"dummy","duvall.star.bnl.gov", 0);
    mconnectState = true;
 
   // mysql_init(&stardb);
   //if(!mysql_real_connect(&stardb,"localhost","","",mdbName,0,NULL,0)){
   // cerr << "Connection to db failed" << endl;
   // }

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
StDbServer::QueryDb(StDbTableComponent* table) { 
  if(mdatabase->QueryDb(table)){
  table->StreamAccessor(mdatabase->getReader());
  table->Streamer(mdatabase->getReader());
  } else {
    if(table) cout << "table ["<<table->getTableName()<<"] ";
    cout << "Table is not Updated" << endl;
  }
  //  mdatabase.freeQuery();
}


////////////////////////////////////////////////////////////////

void 
StDbServer::QueryDb(StDbConfigNode* node) { 

  mdatabase->QueryDb(node);
//mdatabase.freeQuery();

}

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

















