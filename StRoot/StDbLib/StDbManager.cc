#include "StDbManager.hh"
#include "StDbLists.hh"
#include "StDbConfigNode.hh"
#include "StDbServer.hh"
#include "StDbTable.h"

StDbManager* StDbManager::mInstance=0;


////////////////////////////////////////////////////////////////

StDbManager::~StDbManager(){
 

  //  if(m_configNode)m_configNode->deleteTree();
  deleteServers();
  deleteDomains();
  deleteTypes();

}

////////////////////////////////////////////////////////////////

void
StDbManager::deleteServers() {  

  ServerList::iterator itr;
  StDbServer* server;

  do {
      for(itr = mservers.begin(); itr != mservers.end(); ++itr){
         server = *itr;
         mservers.erase(itr);
         delete server;
         break;
        }
     } while( mservers.begin() != mservers.end() );
}

////////////////////////////////////////////////////////////////

void
StDbManager::deleteDomains(){

  dbDomains::iterator itr;
  dbDomain* domain;

  do {
    for(itr = mDomains.begin(); itr!=mDomains.end(); ++itr){
        domain=*itr;
        mDomains.erase(itr);
        delete domain;
        break;
    }
  } while (mDomains.begin() != mDomains.end());

}

////////////////////////////////////////////////////////////////

void
StDbManager::deleteTypes(){

  dbTypes::iterator itr;
  dbType* type;

  do {
    for(itr = mTypes.begin(); itr!=mTypes.end(); ++itr){
        type=*itr;
        mTypes.erase(itr);
        delete type;
        break;
    }
  } while (mTypes.begin() != mTypes.end());

}

////////////////////////////////////////////////////////////////

char*
StDbManager::getDbTypeName(StDbType type){

char* name;
  for(dbTypes::iterator itr=mTypes.begin();
      itr != mTypes.end(); ++itr){
    if((*itr)->type == type){
      name = (*itr)->name;
      break;
    }
  }

return strdup(name);
}     

////////////////////////////////////////////////////////////////

char*
StDbManager::getDbDomainName(StDbDomain domain){

char* name;
  for(dbDomains::iterator itr=mDomains.begin();
      itr != mDomains.end(); ++itr){
    if((*itr)->domain == domain){
      name = (*itr)->name;
      break;
    }
  }

char* retName=strdup(name);
if(strcmp(retName,"unknown")==0)retName=0;

return strdup(retName);
}     


////////////////////////////////////////////////////////////////

StDbType
StDbManager::getDbType(const char* typeName){

StDbType retType;
  for(dbTypes::iterator itr=mTypes.begin();
      itr != mTypes.end(); ++itr){
    if(strcmp((*itr)->name,typeName)==0){
      retType = (*itr)->type;
      break;
    }
  }

return retType;
}     


////////////////////////////////////////////////////////////////

StDbDomain
StDbManager::getDbDomain(const char* domainName){

StDbDomain retType;
  for(dbDomains::iterator itr=mDomains.begin();
      itr != mDomains.end(); ++itr){
    if(strcmp((*itr)->name,domainName) ==0){
      retType = (*itr)->domain;
      break;
    }
  }


return retType;
}     

////////////////////////////////////////////////////////////////

void
StDbManager::initServers(const char* refFile){

  //
  // No longer called. servers are created as needed 
  //

StDbServer* mserver = 0;

/* just now only one server
 if(refFile){  // get from a XML reference file
    StDbServer::instance()->queryDbServers(refFile);
 } else {     // get from the mysql DataBase
    StDbServer::instance()->queryDbServers();
 }
*/

  cout << "StDbManager:: Creating Server List " << endl;
    mservers.push_back(new StDbServer(StarDb,Star,"StarDb","Star"));
    mservers.push_back(new StDbServer(Conditions,Star,"Conditions","Star"));
    mservers.push_back(new StDbServer(Calibrations,Star,"Calibrations","Star"));
    mservers.push_back(new StDbServer(Geometry,Star,"Geometry","Star"));
    mservers.push_back(new StDbServer(Conditions,Tpc,"Conditions","tpc"));
    mservers.push_back(new StDbServer(Calibrations,Tpc,"Calibrations","tpc"));
    mservers.push_back(new StDbServer(Geometry,Tpc,"Geometry","tpc"));
      /*
    while( (mserver=StDbServer::instance()->nextServer())){
         mservers.push_back(mserver);
    }
      */
}

////////////////////////////////////////////////////////////////

StDbServer*
StDbManager::findServer(StDbType type, StDbDomain domain){

 StDbServer* server = 0;

 for(ServerList::iterator itr = mservers.begin();
     itr != mservers.end(); ++itr){
   if((*itr)->getDbDomain()==domain && (*itr)->getDbType()==type){
     server = *itr;
     break;
   }
 }

 if(!server){
    char * typeName = getDbTypeName(type);
    char * domainName = getDbDomainName(domain);
    if(typeName && domainName) {
      server = new StDbServer(type,domain,typeName,domainName);
      if(server){
        mservers.push_back(server);
        if(!server->isconnected())server->init();
      }
    }
 }

if(!server) {
   cout << "No Such Server " << endl;
   cout << "Type = " << type << " & name= " << getDbTypeName(type) << endl;
   cout << "Domain = "<< domain << " & name = " << getDbDomainName(domain) << endl;
}
return server;
}

////////////////////////////////////////////////////////////////

StDbConfigNode*
StDbManager::initConfig(const char* configName){

StDbConfigNode* configNode = new StDbConfigNode(StarDb,Star,"StarDb",configName);
configNode->buildTree();

return configNode;
}

////////////////////////////////////////////////////////////////
StDbConfigNode*
StDbManager::initConfig(StDbType type, StDbDomain domain, const char* configName){

StDbConfigNode * configNode = 0;

char* name;
 if(domain == Star){
  name = getDbTypeName(type);
 } else {
  name = getDbDomainName(domain);
 }

 configNode = new StDbConfigNode(type,domain,name,configName);
 configNode->buildTree();
 return configNode;

}

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

bool
StDbManager::IsValid(StDbTableI* table, int time){

 bool retVal = false;
 if(!table) return retVal;
 if(time >= table->getBeginTime() && time < table->getEndTime()) retVal = true;
return retVal;

}

void
StDbManager::fetchDbTable(StDbTableI* table, int time){

  if(!table){
    cout << "Cannot Update StDbTable=0" << endl;
  } else {

  table->setRequestTime(time);

  StDbServer* server = findServer(table->getDbType(),table->getDbDomain());
  server->QueryDb((StDbTable*)table);  // table is filled 

  }
}

void
StDbManager::storeDbTable(StDbTableI* table){

  if(!table){
    cout << "Cannot Update StDbTable=0" << endl;
  } else {

    //  table->setRequestTime(time);

  StDbServer* server = findServer(table->getDbType(),table->getDbDomain());
  server->WriteDb((StDbTable*)table);  // table is filled 

  }
}

////////////////////////////////////////////////////////////////

void
StDbManager::initTypes(){

mTypes.push_back(new dbType(StarDb,"StarDb")); 
mTypes.push_back(new dbType(RunLog,"RunLog")); 
mTypes.push_back(new dbType(DbServer,"StDbServer")); 
mTypes.push_back(new dbType(Conditions,"Conditions")); 
mTypes.push_back(new dbType(Calibrations,"Calibrations")); 
mTypes.push_back(new dbType(Geometry,"Geometry")); 
mTypes.push_back(new dbType(RunCatalog,"RunCatalog")); 
mTypes.push_back(new dbType(Configurations,"Configurations")); 

}

////////////////////////////////////////////////////////////////

void
StDbManager::initDomains(){

mDomains.push_back(new dbDomain(Star,"Star")); 
mDomains.push_back(new dbDomain(Tpc,"tpc")); 
mDomains.push_back(new dbDomain(Ftpc,"ftpc")); 
mDomains.push_back(new dbDomain(Emc,"emc")); 
mDomains.push_back(new dbDomain(Svt,"svt")); 
mDomains.push_back(new dbDomain(Ctb,"ctb")); 
mDomains.push_back(new dbDomain(Trg,"trg")); 
mDomains.push_back(new dbDomain(Daq,"daq")); 
mDomains.push_back(new dbDomain(Scaler,"scaler")); 
mDomains.push_back(new dbDomain(Global,"global")); 
mDomains.push_back(new dbDomain(L3,"l3")); 

}

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////


















