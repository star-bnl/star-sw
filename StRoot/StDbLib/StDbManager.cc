/***************************************************************************
 *   
 * $Id: StDbManager.cc,v 1.14 2000/01/19 20:20:05 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Manages access to Servers and passes Query-by-Table to db
 *
 ***************************************************************************
 *
 * $Log: StDbManager.cc,v $
 * Revision 1.14  2000/01/19 20:20:05  porter
 * - finished transaction model needed by online
 * - fixed CC5 compile problem in StDbNodeInfo.cc
 * - replace TableIter class by StDbTableIter to prevent name problems
 *
 * Revision 1.13  2000/01/14 14:50:52  porter
 * expanded use of verbose mode & fixed inconsistency in
 * StDbNodeInfo::getElementID
 *
 * Revision 1.12  2000/01/10 20:37:54  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.11  1999/12/28 21:31:42  porter
 * added 'using std::vector' and 'using std::list' for Solaris CC5 compilation.
 * Also fixed some warnings arising from the CC5 compiles
 *
 * Revision 1.10  1999/12/03 22:24:01  porter
 * expanded functionality used by online, fixed bug in
 * mysqlAccessor::getElementID(char*), & update StDbDataSet to
 * conform to changes in Xml reader & writer
 *
 * Revision 1.9  1999/10/19 14:30:39  porter
 * modifications relevant to use with StDbBroker and future merging with
 * "params" database structure + some docs + suppressing diagnostics messages
 *
 * Revision 1.8  1999/09/30 02:06:07  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "StDbManager.hh"
#include "StDbLists.hh"
#include "StDbConfigNode.hh"
#include "StDbServer.hh"
#include "StDbTable.h"
#include "StDbTime.h"
#include "StDbTableIter.hh"
#include <iostream.h>
#include <strstream.h>
#include <strings.h>

StDbManager* StDbManager::mInstance=0;

////////////////////////////////////////////////////////////////

StDbManager::~StDbManager(){
 
  deleteServers();
  deleteDomains();
  deleteTypes();

}

////////////////////////////////////////////////////////////////
//
//
// protected methods
//
//
////////////////////////////////////////////////////////////////

void
StDbManager::initTypes(){


addDbType(dbStDb,"StarDb"); 
addDbType(dbRunLog,"RunLog"); 
addDbType(dbServer,"StDbServer"); 
addDbType(dbConditions,"Conditions"); 
addDbType(dbCalibrations,"Calibrations"); 
addDbType(dbGeometry,"Geometry"); 
addDbType(dbRunCatalog,"RunCatalog"); 
addDbType(dbConfigurations,"Configurations"); 
addDbType(dbRunParams,"RunParams"); 
addDbType(dbTestScheme,"TestScheme"); 

}

////////////////////////////////////////////////////////////////

void
StDbManager::addDbType(StDbType type, const char* typeName){
  mTypes.push_back(new dbType(type,typeName));
}

////////////////////////////////////////////////////////////////

void
StDbManager::initDomains(){

mDomains.push_back(new dbDomain(dbStar,"Star")); 
mDomains.push_back(new dbDomain(dbTpc,"tpc")); 
mDomains.push_back(new dbDomain(dbFtpc,"ftpc")); 
mDomains.push_back(new dbDomain(dbEmc,"emc")); 
mDomains.push_back(new dbDomain(dbSvt,"svt")); 
mDomains.push_back(new dbDomain(dbCtb,"ctb")); 
mDomains.push_back(new dbDomain(dbTrg,"trg")); 
mDomains.push_back(new dbDomain(dbDaq,"daq")); 
mDomains.push_back(new dbDomain(dbScaler,"scaler")); 
mDomains.push_back(new dbDomain(dbGlobal,"global")); 
mDomains.push_back(new dbDomain(dbL3,"l3")); 

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

void
StDbManager::lookUpServers(){

int it=1;
char* xmlfile1 = getFileName("HOME");
char* xmlfile2 = getFileName("STDB_SERVERS");
char* xmlfile3 = getFileName("STAR","/StDb/servers");

 if(misVerbose)cout<<endl<<"**** Order of Files searched for Servers **** "<<endl;
if(xmlfile1){
 ifstream is1(xmlfile1);

 if(is1){
   if(misVerbose)cout<<"  "<<it<<". "<< xmlfile1 <<endl;
   findServersXml(is1);
   it++;
 }
 delete [] xmlfile1;
}

if(xmlfile2){
 ifstream is2(xmlfile2);
 if(is2){
   if(misVerbose)cout<<"  "<<it<<". "<<xmlfile2 <<endl;
   findServersXml(is2);
   it++;
 }
 delete [] xmlfile2;
}

if(xmlfile3){
 ifstream is3(xmlfile3);
 if(is3){
   if(misVerbose)cout<<"  "<<it<<". "<<xmlfile3 <<endl;
   findServersXml(is3);
 }
 delete [] xmlfile3;
}

 if(misVerbose) cout <<"*********************************************" << endl<<endl;;
mhasServerList = true;

}

////////////////////////////////////////////////////////////////

char*
StDbManager::getFileName(const char* envName, const char* subDirName){

char* xmlFile = 0;
char* dirname=getenv(envName);

 if(dirname){

  xmlFile = new char[1024];
  ostrstream os(xmlFile,1024);

  char serverFile[32]="/dbServers.xml";

  if(subDirName){
     os<<dirname<<subDirName<<(char*)serverFile<<ends;
   } else {
     os<<dirname<<serverFile<<ends;
   }

 }


return xmlFile;
}


////////////////////////////////////////////////////////////////

void
StDbManager::findServersXml(ifstream& is){

  char* stardatabase;

  while(!is.eof()){

  stardatabase = findServerString(is); 
  if(!stardatabase) continue;

  // local DTD ...
  char bserver[32]="<server>"; char eserver[32]="</server>";
  char bhost[32]="<host>"; char ehost[32]="</host>";
  char bsock[32]="<socket>"; char esock[32]="</socket>";
  char bport[32]="<port>"; char eport[32]="</port>";
  char bdb[32]="<databases>"; char edb[32]="</databases>";
  int portNum = 0;

  char* servName = mparser.getString(stardatabase,(char*)bserver,(char*)eserver);
  char* hostName = mparser.getString(stardatabase,(char*)bhost,(char*)ehost);
  char* uSocket = mparser.getString(stardatabase,(char*)bsock,(char*)esock);
  char* portNumber = mparser.getString(stardatabase,(char*)bport,(char*)eport);

  if(portNumber){
    portNum   = atoi(portNumber);
    delete [] portNumber;
  }

  char* dbNames = mparser.getString(stardatabase,(char*)bdb,(char*)edb);

   StDbServer* server = new StDbServer();
   server->setServerName((const char*)servName); delete [] servName;
   server->setHostName((const char*)hostName); delete [] hostName;
   server->setUnixSocket((const char*)uSocket); delete [] uSocket;
   server->setPortNumber(portNum); 

  if( !dbNames && !mhasDefaultServer ){

    server->setIsDefaultServer();
    mservers.push_back(server);
    mhasDefaultServer = true;

  } else {

    StDbServer* aserver;
    char* p1 = &dbNames[0];
    char* aname;

    while(p1){
      aname = getNextName(p1); // p1 is reset to after next "," 
      aserver = new StDbServer(*server);
      aserver->setDbName(aname);
      mservers.push_back(aserver);
      if(aname)delete [] aname;
    }
    
   if(dbNames)delete [] dbNames;
  }
  if(stardatabase)delete [] stardatabase;
  }

}

////////////////////////////////////////////////////////////////

char*
StDbManager::findServerString(ifstream& is){

char* line = new char[10240];
char tmpline[256];
bool done = false;
bool started = false;
char* id;

ostrstream os(line,10240);

 while(!done){

   if(is.eof()){
     done = true; 
     delete [] line;
     line = 0; // and no new complete string was found
   } else {
     is.getline(tmpline,255);

     if((id=strstr(tmpline,"//")))continue;

     if(!started){

        id= strstr(tmpline,"<StDbServer>");
        if(id){
          os<<tmpline;
          started = true;
        }

     } else {

        os<<tmpline;

        id=strstr(tmpline,"</StDbServer>");
        if(id){
          os<<ends;
          done=true;
        }

     } // started check          
   } // eof check 
 } // while loop

return line;
}

////////////////////////////////////////////////////////////////

char*
StDbManager::getNextName(char*& names){

char* nextName = 0;
if(!names)return nextName;

char* id = strstr(names,",");

if(!id) {
  // nextName = mstringDup(names);
 nextName = mparser.removeBlankEnds(names);
 //delete [] names;
 names = 0;
} else {
  int iloc = id-names;
  char* saveName = new char[iloc+1];
  strncpy(saveName,names,iloc);
  saveName[iloc]='\0';
  nextName=mparser.removeBlankEnds(saveName);
  delete [] saveName;
  names = id; names++;
}

return nextName;
}


////////////////////////////////////////////////////////////////

char*
StDbManager::mstringDup(const char* str){

char* retString=0;
if(!str)return retString;
retString = new char[strlen(str)+1];
strcpy(retString,str);

return retString;
}


////////////////////////////////////////////////////////////////
//
//
//  public methods
//
//
///////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////

StDbServer*
StDbManager::findServer(StDbType type, StDbDomain domain){

 if(!mhasServerList)lookUpServers();
 StDbServer* server = 0;

 // first check if it exists in list
 for(ServerList::iterator itr = mservers.begin();
     itr != mservers.end(); ++itr){
   if((*itr)->getDbDomain()==domain && (*itr)->getDbType()==type){
     server = *itr;
     break;
   }
 }

 // if not build from default server
 if(!server){ 

    char * typeName = getDbTypeName(type);
    char * domainName = getDbDomainName(domain);
    StDbServer* defaultServer = findDefaultServer();

    if(defaultServer){
     server = new StDbServer(*defaultServer);
     server->setDataBase(type,domain,typeName,domainName);
     mservers.push_back(server);
    }

    delete [] typeName; delete [] domainName;

 }

 // connect to database if needed

 if(server && !server->isconnected()){
   server->init();
 }

 // report failure
 if(!server) {
   cerr << "ERROR:::   No Such Server  " << endl;
   cerr << "DataBase Type Name= " << getDbTypeName(type) << endl;
   cerr << "DataBase Domain Name = " << getDbDomainName(domain) << endl;
   cerr << endl;
}

return server;
}


////////////////////////////////////////////////////////////////

StDbServer*
StDbManager::findServer(StDbNode* node){

return findServer(node->getDbType(),node->getDbDomain());
}


////////////////////////////////////////////////////////////////

StDbServer*
StDbManager::findServer(const char* typeName, const char* domainName){

return findServer(getDbType(typeName),getDbDomain(domainName));
}

////////////////////////////////////////////////////////////////

StDbServer*
StDbManager::findServer(const char* databaseName){

  // if databaseName contains "_" then = 'dbTypeName_dbDomainName'
  // else = 'dbTypeName' and dbDomainName="Star"

char* dbName = new char[strlen(databaseName)+1];
strcpy(dbName,databaseName);
char* domainName;

char* id=strstr(dbName,"_");
if(id){
   *id='\0';
    id++;
    domainName=new char[strlen(id)+1];
    strcpy(domainName,id);
} else {
    domainName=new char[strlen("Star")+1];
    strcpy(domainName,"Star");
}

StDbServer* server=findServer(dbName,(const char*) domainName);
delete [] domainName;
delete [] dbName;

return server;
}


////////////////////////////////////////////////////////////////

StDbServer*
StDbManager::findDefaultServer(){

 StDbServer* server = 0;

 for(ServerList::iterator itr = mservers.begin();
     itr != mservers.end(); ++itr){
   if((*itr)->isDefault()){
     server = *itr;
     break;
   }
 }

return server;
}

////////////////////////////////////////////////////////////////

char*
StDbManager::getDbTypeName(StDbType type){

char* name=0;
  for(dbTypes::iterator itr=mTypes.begin();
      itr != mTypes.end(); ++itr){
    if((*itr)->type == type){
      name = (*itr)->name;
      break;
    }
  }

if(!name) {
  if(misVerbose)cerr<<" Type Name not found for type enum="<<(int)type<<endl;

  return name;
}

if(misVerbose)cout<<" Type Name "<<name<<" found for type enum="<<(int)type<<endl;

return mstringDup(name);
}     

////////////////////////////////////////////////////////////////

char*
StDbManager::getDbDomainName(StDbDomain domain){

if(domain==dbDomainUnknown)return mstringDup("Star");

char* name=0;
  for(dbDomains::iterator itr=mDomains.begin();
      itr != mDomains.end(); ++itr){
    if((*itr)->domain == domain){
      name = (*itr)->name;
      break;
    }
  }

if(!name) {
  if(misVerbose)cerr<<" Domain Name not found for domain enum="<<(int)domain<<endl;
  return name;
}
 if(misVerbose)cout<<" Domain Name "<<name<<" found for domain enum="<<(int)domain<<endl;

return mstringDup(name);
}     


////////////////////////////////////////////////////////////////

StDbType
StDbManager::getDbType(const char* typeName){

  StDbType retType=dbUser;// assume unknown = dbUser
  if(misVerbose)cout << "Finding dbType for typeName= "<<typeName<<endl;
  int ifound = 0;
  for(dbTypes::iterator itr=mTypes.begin();
      itr != mTypes.end(); ++itr){
    if(strcmp((*itr)->name,typeName)==0){
      retType = (*itr)->type;
      ifound = 1;
      break;
    }
  }

if(!ifound){
  if(misVerbose)cout << "Adding dbUser Type "<<typeName<<endl;
  addDbType(retType,typeName); //This'll overwrite User db definition
}
return retType;
}     


////////////////////////////////////////////////////////////////

StDbDomain
StDbManager::getDbDomain(const char* domainName){

StDbDomain retType=dbDomainUnknown;
  if(misVerbose)cout << "Finding dbDomain for domainName= "<<domainName<<endl;
  for(dbDomains::iterator itr=mDomains.begin();
      itr != mDomains.end(); ++itr){
    if(strcmp((*itr)->name,domainName) ==0){
      retType = (*itr)->domain;
      break;
    }
  }

  if(retType==dbDomainUnknown && misVerbose){
    cout << "No Domain found "<<domainName<<endl;
  }

return retType;
}     


////////////////////////////////////////////////////////////////

StDbConfigNode*
StDbManager::initConfig(const char* dbName){

char* type; 
char* domain;

getDataBaseInfo(dbName,type,domain);

StDbType dbtype = getDbType((const char*) type);
StDbDomain dbdomain = getDbDomain((const char*) domain);

delete [] type;
delete [] domain;

return initConfig(dbtype,dbdomain);
}

////////////////////////////////////////////////////////////////

StDbConfigNode*
StDbManager::initConfig(const char* dbName, const char* configName){

char* type; 
char* domain;

getDataBaseInfo(dbName,type,domain);

StDbType dbtype = getDbType((const char*) type);
StDbDomain dbdomain = getDbDomain((const char*) domain);

delete [] type;
delete [] domain;

return initConfig(dbtype,dbdomain,configName);
}

////////////////////////////////////////////////////////////////

StDbConfigNode*
StDbManager::initConfig(StDbType type, StDbDomain domain){

StDbConfigNode * configNode = 0;

char* name;
 if(domain == dbStar){
  name = getDbTypeName(type);
 } else {
  name = getDbDomainName(domain);
 }

 configNode = new StDbConfigNode(type,domain,name);
 configNode->buildTree();
 return configNode;
}

////////////////////////////////////////////////////////////////

StDbConfigNode*
StDbManager::initConfig(StDbType type, StDbDomain domain, const char* configName){

StDbConfigNode * configNode = 0;

char* name;
 if(domain == dbStar){
  name = getDbTypeName(type);
 } else {
  name = getDbDomainName(domain);
 }

 configNode = new StDbConfigNode(type,domain,name,configName);
 configNode->buildTree();
 return configNode;

}

////////////////////////////////////////////////////////////////
void
StDbManager::setRequestTime(unsigned int time){

mcheckTime.munixTime = time;
StDbServer* server = findServer(dbStDb,dbStar);
if(server)mcheckTime.mdateTime = server->getDateTime(time);

}

////////////////////////////////////////////////////////////////

void
StDbManager::setRequestTime(const char* time){

mcheckTime.setDateTime(time);
StDbServer* server = findServer(dbStDb,dbStar);
if(server)mcheckTime.munixTime = server->getUnixTime(time);

}


////////////////////////////////////////////////////////////////
void
StDbManager::setStoreTime(unsigned int time){

mstoreTime.munixTime = time;
StDbServer* server = findServer(dbStDb,dbStar);
if(server){
 mstoreTime.mdateTime = server->getDateTime(time);
 if(misVerbose) cout <<" StoreDateTime = " << mstoreTime.mdateTime << endl;
}


}

////////////////////////////////////////////////////////////////

void
StDbManager::setStoreTime(const char* time){

mstoreTime.setDateTime(time);
StDbServer* server = findServer(dbStDb,dbStar);
if(server){
  mstoreTime.munixTime = server->getUnixTime(time);
 if(misVerbose) cout <<" StoreUnixTime = " << mstoreTime.munixTime << endl;
}

}

////////////////////////////////////////////////////////////////

unsigned int
StDbManager::getUnixCheckTime(){ return mcheckTime.munixTime; }

////////////////////////////////////////////////////////////////

char* 
StDbManager::getDateCheckTime(){ return mcheckTime.mdateTime; }

////////////////////////////////////////////////////////////////

unsigned int
StDbManager::getUnixStoreTime(){ return mstoreTime.munixTime; }

////////////////////////////////////////////////////////////////

char* 
StDbManager::getDateStoreTime(){ return mstoreTime.mdateTime; }

////////////////////////////////////////////////////////////////

bool
StDbManager::IsValid(StDbTable* table){

 unsigned int time = mcheckTime.munixTime;

 bool retVal = false;
 if(!table) return retVal;
 if(time >= table->getBeginTime() && time < table->getEndTime()) retVal = true;

return retVal;

}

////////////////////////////////////////////////////////////////

bool
StDbManager::fetchDbTable(StDbTable* table){
bool retVal = false;

  if(!table){
    cout << "Cannot Update Null pointer to StDbTable" << endl;
    return retVal;
  } else {

    if(!mcheckTime.munixTime && !mcheckTime.mdateTime)return retVal;

    StDbServer* server = findServer(table->getDbType(),table->getDbDomain());
    if(!server)return retVal;
    if(!server->QueryDb(table,mcheckTime.munixTime)){
     if(!server->reConnect())return retVal;
     if(!server->QueryDb(table,mcheckTime.munixTime))return retVal;
    }
     
     // table is filled 
  }

return true;
}

bool
StDbManager::fetchDbTable(StDbTable* table, char* whereClause){
bool retVal = false;

  if(!table){
    cout << "Cannot Update Null pointer to StDbTable" << endl;
    return retVal;
  } else {


    StDbServer* server = findServer(table->getDbType(),table->getDbDomain());
    if(!server)return retVal;
    if(!server->QueryDb(table,whereClause)){
     if(!server->reConnect())return retVal;
     if(!server->QueryDb(table,whereClause))return retVal;
    }
     
     // table is filled 
  }

return true;
}
     


////////////////////////////////////////////////////////////////

bool
StDbManager::fetchAllTables(StDbConfigNode* node){

bool retVal=false;

  if(!mcheckTime.munixTime && !mcheckTime.mdateTime){
    cerr<< "No storage time set";
    return retVal;
  }

  if(node->hasData()){
    StDbTableIter* itr = node->getStDbTableIter();
    StDbTable* table = 0;
    retVal = true;
    while(!itr->done()){
      table = itr->next();
      retVal = (retVal && fetchDbTable(table));
    }
  delete itr;
  }

bool children = true;
bool siblings = true;

  if(node->hasChildren())children = fetchAllTables(node->getFirstChildNode());
  StDbConfigNode* nextNode = 0;
  if((nextNode=node->getNextNode()))siblings = fetchAllTables(nextNode);

return (retVal && children && siblings);
}   

////////////////////////////////////////////////////////////////

bool
StDbManager::storeDbTable(StDbTable* table){
bool retVal = false;

  if(!table){
    cout << "Cannot Update StDbTable=0" << endl;
    return retVal;
  } else {

    //  table->setRequestTime(time);
    if(!mstoreTime.munixTime && !mstoreTime.mdateTime){
      cerr<< "No storage time set"; return retVal;   
    }

    StDbServer* server = findServer(table->getDbType(),table->getDbDomain());
    if(!server)return retVal;

    if(!server->WriteDb(table,mstoreTime.munixTime)){
     if(!server->reConnect())return retVal;  // table is filled 
     if(!server->WriteDb(table,mstoreTime.munixTime))return retVal; 
   }    

  }

return true;
}

////////////////////////////////////////////////////////////////

bool
StDbManager::storeAllTables(StDbConfigNode* node){

bool retVal = false;

  if(!mstoreTime.munixTime && !mstoreTime.mdateTime){
    cerr<< "No storage time set"; return retVal;
  }

  if(node->hasData()){
    StDbTableIter* itr = node->getStDbTableIter();
    StDbTable* table = 0;
    retVal = true;
    while(!itr->done()){
      table = itr->next();
      retVal = (retVal && storeDbTable(table));
      if(!retVal){
        rollBackAllTables(node);
        break;
      }
    }
  delete itr;
  }

if(!retVal) return retVal;
bool children = true;
bool siblings = true;

  if(node->hasChildren())children = storeAllTables(node->getFirstChildNode());
  if(!children){
    rollBackAllTables(node);
    return false;
  }
  StDbConfigNode* nextNode = 0;
  if((nextNode=node->getNextNode()))siblings = storeAllTables(nextNode);
  if(!siblings){
    rollBackAllTables(node);
    return false;
  }

return true;
}

///////////////////////////////////////////////////////////////

int
StDbManager::storeConfig(StDbConfigNode* node, int currentID){

StDbServer* server=findServer(node->getDbType(),node->getDbDomain());
bool children = true;
bool siblings = true;
bool retVal = false;
int nodeID=server->WriteDb(node,currentID);
if(nodeID){
  retVal = true;
  node->addWrittenNode(nodeID);
  if(node->hasChildren())children=storeConfig(node->getFirstChildNode(),nodeID);
  if(node->getNextNode())siblings=storeConfig(node->getNextNode(),currentID);
 
}
 if(! (retVal && children && siblings) ){
      if(retVal){
        rollBack(node);
      } else  {
        if(node->hasChildren() && children)rollBack(node->getFirstChildNode());
        if((node->getNextNode()) && siblings)rollBack(node->getNextNode());
      }
     retVal = false;
 }

return retVal;
}

////////////////////////////////////////////////////////////////

bool
StDbManager::rollBackAllTables(StDbConfigNode* node){

bool retVal=false;

  if(node->hasData()){
    StDbTableIter* itr = node->getStDbTableIter();
    StDbTable* table = 0;
    retVal = true;
    while(!itr->done()){
      table = itr->next();
      rollBack(table);
    }
  delete itr;
  }

bool children = true;
bool siblings = true;

  if(node->hasChildren())children = rollBackAllTables(node->getFirstChildNode());
  StDbConfigNode* nextNode = 0;
  if((nextNode=node->getNextNode()))siblings = rollBackAllTables(nextNode);

return (retVal && children && siblings);
}

////////////////////////////////////////////////////////////////

bool
StDbManager::rollBackAllNodes(StDbConfigNode* node){

bool retVal=false;

  if(node->hasData()){
    StDbTableIter* itr = node->getStDbTableIter();
    StDbNode* tnode = 0;
    retVal = true;
    while(!itr->done()){
      tnode = (StDbNode*)itr->next();
      rollBack(tnode);
    }
  delete itr;
  }

bool children = true;
bool siblings = true;

if(node->hasChildren())children = rollBackAllNodes(node->getFirstChildNode());
StDbConfigNode* nextNode = 0;
if((nextNode=node->getNextNode()))siblings = rollBackAllNodes(nextNode);


return rollBack(node);
}

////////////////////////////////////////////////////////////////

bool
StDbManager::rollBack(StDbNode* node){

  if(!node->canRollBack()){
    cerr<<" Cannot Roll Back Write of Node="<<node->getName()<<endl;
    return false;
  }

StDbServer* server=findServer(node->getDbType(),node->getDbDomain());
return server->rollBack(node);
}

////////////////////////////////////////////////////////////////////////

bool
StDbManager::rollBack(StDbTable* table){

  int* dataRows=0;
  int numRows;
  bool retVal = false;
  if((dataRows=table->getWrittenRows(&numRows))){
     StDbServer* server=findServer(table->getDbType(),table->getDbDomain());
     retVal = server->rollBack(table);
  }
     
return retVal;
}


////////////////////////////////////////////////////////////////////////

bool
StDbManager::commitAllTables(StDbConfigNode* node){

bool retVal=true;

  if(node->hasData()){
    StDbTableIter* itr = node->getStDbTableIter();
    StDbTable* table = 0;
    retVal = true;
    while(!itr->done()){
      table = itr->next();
      if(table) table->commitData();
    }
  delete itr;
  }

bool children = true;
bool siblings = true;

  if(node->hasChildren())children = commitAllTables(node->getFirstChildNode());
  StDbConfigNode* nextNode = 0;
  if((nextNode=node->getNextNode()))siblings = commitAllTables(nextNode);

return (retVal && children && siblings);
}

////////////////////////////////////////////////////////////////////////

bool
StDbManager::commitAllNodes(StDbConfigNode* node){

bool retVal=true;

  if(node->hasData()){
    StDbTableIter* itr = node->getStDbTableIter();
    StDbNode* tnode = 0;
    retVal = true;
    while(!itr->done()){
      tnode = (StDbNode*)itr->next();
      if(tnode) tnode->commit();
    }
  delete itr;
  }

bool children = true;
bool siblings = true;

if(node->hasChildren())children = commitAllNodes(node->getFirstChildNode());
StDbConfigNode* nextNode = 0;
if((nextNode=node->getNextNode()))siblings = commitAllNodes(nextNode);

node->commit();

return true;
}




////////////////////////////////////////////////////////////////

bool
StDbManager::getDataBaseInfo(const char* dbName, char*& type, char*& domain){

bool retVal = false;
char* tmpName=new char[strlen(dbName)+1];
strcpy(tmpName,dbName);


char* id = strstr(tmpName,"_");
if(!id) {
type = new char[strlen(tmpName)+1];
strcpy(type,tmpName);
domain = new char[strlen("Star")+1];
strcpy(domain,"Star");
delete [] tmpName;
return true;
}

*id='\0';
type = new char[strlen(tmpName)+1];
strcpy(type,tmpName);
id++;
domain = new char[strlen(id)+1];
strcpy(domain,id);
retVal = true;

delete [] tmpName;
 if(misVerbose){
   cout << "From database name = "<<dbName<<endl;
   cout << "Returning DbType = "<<type<<" & DbDomain= "<<domain<< endl;
 }
return retVal;
}

