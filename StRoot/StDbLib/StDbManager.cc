/***************************************************************************
 *   
 * $Id: StDbManager.cc,v 1.24 2000/06/02 13:37:36 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Manages access to Servers and passes Query-by-Table to db
 *
 ***************************************************************************
 *
 * $Log: StDbManager.cc,v $
 * Revision 1.24  2000/06/02 13:37:36  porter
 * built up list of minor changes:
 *  - made buffer more robust for certain null inputs
 *  - fixed small leak in StDbTables & restructure call to createMemory
 *  - added dbRhic as a database domain in StDbDefs
 *  - added setUser() in StDbManager
 *  - added more diagnostic printouts in mysqlAccessor.cc
 *
 * Revision 1.23  2000/05/04 15:13:11  porter
 * added dbOnl, dbRich, dbMwc domains as standards
 *
 * Revision 1.22  2000/04/25 18:26:03  porter
 * added flavor & production time as settable query fields in
 * table &/or node. Associated SQL updated in mysqlAccessor.
 * Flavor key supports "+" as an OR symbol.
 *
 * Revision 1.21  2000/03/28 17:03:18  porter
 * Several upgrades:
 * 1. configuration by timestamp for Conditions
 * 2. query by whereClause made more systematic
 * 3. conflict between db-stored comments & number lists resolved
 * 4. ensure endtime is correct for certain query falures
 * 5. dbstl.h->handles ObjectSpace & RogueWave difference (Online vs Offline)
 *
 * Revision 1.20  2000/03/01 20:56:16  porter
 * 3 items:
 *    1. activated reConnect for server timeouts
 *    2. activated connection sharing; better resource utilization but poorer
 *       logging
 *    3. made rollback method in mysqlAccessor more robust (affects writes only)
 *
 * Revision 1.19  2000/02/24 20:30:45  porter
 * fixed padding for uchar; beginTime in mysqlAccessor;
 * added rollback safety checkes in StDbManger
 *
 * Revision 1.18  2000/02/18 16:58:09  porter
 * optimization of table-query, + whereClause gets timeStamp if indexed
 *  + fix to write multiple rows algorithm
 *
 * Revision 1.17  2000/02/15 20:27:44  porter
 * Some updates to writing to the database(s) via an ensemble (should
 * not affect read methods & haven't in my tests.
 *  - closeAllConnections(node) & closeConnection(table) method to mgr.
 *  - 'NullEntry' version to write, with setStoreMode in table;
 *  -  updated both StDbTable's & StDbTableDescriptor's copy-constructor
 *
 * Revision 1.16  2000/01/27 20:27:17  porter
 * fixed error logic for table, config, or table-list not-found
 *
 * Revision 1.15  2000/01/27 05:54:34  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
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
#include "mysqlAccessor.hh"
#include "dbCollection.h"
#include <iostream.h>
#include <strstream.h>
#include <strings.h>

StDbManager* StDbManager::mInstance=0;
StDbDefaults* StDbDefaults::mInstance=0;

////////////////////////////////////////////////////////////////

StDbManager::~StDbManager(){
 
  deleteServers();
  deleteQueryObjects();
  deleteDomains();
  deleteTypes();
  mInstance=0;

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
mDomains.push_back(new dbDomain(dbOnl,"onl")); 
mDomains.push_back(new dbDomain(dbRich,"rich")); 
mDomains.push_back(new dbDomain(dbMwc,"mwc")); 

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
StDbManager::deleteQueryObjects() {  

  QueryObjects::iterator itr;
  tableQuery* qo;

  do {
      for(itr = mqobjects.begin(); itr != mqobjects.end(); ++itr){
         qo = *itr;
         mqobjects.erase(itr);
         delete qo;
         break;
        }
     } while( mqobjects.begin() != mqobjects.end() );

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
   server->setServerName((const char*)servName); 
   server->setHostName((const char*)hostName); delete [] hostName;
   server->setUnixSocket((const char*)uSocket); delete [] uSocket;
   server->setPortNumber(portNum); 

   tableQuery* qo=new mysqlAccessor(servName,portNum); delete [] servName;
   mqobjects.push_back(qo);
   server->setQueryObject(qo);


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
    
   if(server) delete server;
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


 // report failure
 if(!server) {
   cerr << "ERROR:::   No Such Server  " << endl;
   cerr << "DataBase Type Name= " << getDbTypeName(type) << endl;
   cerr << "DataBase Domain Name = " << getDbDomainName(domain) << endl;
   cerr << endl;
 } else {
   server->selectDb();
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
StDbManager::initConfig(const char* dbName, const char* configName, int opt){

char* type; 
char* domain;

getDataBaseInfo(dbName,type,domain);

StDbType dbtype = getDbType((const char*) type);
StDbDomain dbdomain = getDbDomain((const char*) domain);

delete [] type;
delete [] domain;

return initConfig(dbtype,dbdomain,configName,opt);
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
 // configNode->buildTree(opt);

 delete [] name;
 return configNode;
}

////////////////////////////////////////////////////////////////

StDbConfigNode*
StDbManager::initConfig(StDbType type, StDbDomain domain, const char* configName, int opt){

StDbConfigNode * configNode = 0;

char* name = 0;
 if(domain == dbStar){
  name = getDbTypeName(type);
 } else {
  name = getDbDomainName(domain);
 }

 configNode = new StDbConfigNode(type,domain,name,configName);
 configNode->buildTree(opt);
 if(name) delete [] name;
 return configNode;
}

////////////////////////////////////////////////////////////////

StDbConfigNode*
StDbManager::initConfig(StDbType type, StDbDomain domain, unsigned int requestTime, int opt){

StDbConfigNode * configNode = 0;
char* configName = 0;

char* name;
 if(domain == dbStar){
  name = getDbTypeName(type);
 } else {
  name = getDbDomainName(domain);
 }

 StDbTable* table = new StDbTable("dbCollection");
 table->setVersion("default");
 StDbServer* server = findServer(type,domain);
 if(server->QueryDb(table,requestTime)){
    dbCollection* collection = (dbCollection*)table->GetTable();
    if(collection){
      configName = new char[strlen(collection->name)+1];
      strcpy(configName,collection->name);
    }
 }

 // just now I am deleting the table... I could add it to node
 // so that the validity period is kept with the node.

 delete table;

 if(!configName){
   cerr<<"initConfig:: No named collection for found"<<endl;
   return configNode;
 }

 configNode = new StDbConfigNode(type,domain,name,configName);
 configNode->buildTree(opt);
 return configNode;
}

////////////////////////////////////////////////////////////////

StDbConfigNode*
StDbManager::initConfig(const char* dbName, unsigned int requestTime, int opt){

char* type; 
char* domain;

getDataBaseInfo(dbName,type,domain);

StDbType dbtype = getDbType((const char*) type);
StDbDomain dbdomain = getDbDomain((const char*) domain);

delete [] type;
delete [] domain;

return initConfig(dbtype,dbdomain,requestTime,opt);
}


////////////////////////////////////////////////////////////////
void
StDbManager::setRequestTime(unsigned int time){

mcheckTime.munixTime = time;
StDbServer* server = findServer(dbStDb,dbStar);
if(server){
  if(!server->isConnected())server->init();
  if(mcheckTime.mdateTime) delete [] mcheckTime.mdateTime;
  mcheckTime.mdateTime = server->getDateTime(time);
}

}

////////////////////////////////////////////////////////////////

void
StDbManager::setRequestTime(const char* time){

mcheckTime.setDateTime(time);
StDbServer* server = findServer(dbStDb,dbStar);
if(server){
  if(!server->isConnected())server->init();
  mcheckTime.munixTime = server->getUnixTime(time);
}

}


////////////////////////////////////////////////////////////////
void
StDbManager::setStoreTime(unsigned int time){

mstoreTime.munixTime = time;
StDbServer* server = findServer(dbStDb,dbStar);
if(server){
 if(!server->isConnected())server->init();
 if(mstoreTime.mdateTime) delete [] mstoreTime.mdateTime;
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
  if(!server->isConnected())server->init();
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
    if(!server->isConnected())server->init();
    if(!server->QueryDb(table,mcheckTime.munixTime))return retVal;
    
     
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
    if(!server->isConnected())server->init();
    if(!server->QueryDb(table,whereClause))return retVal;

  }

return true;
}
     


////////////////////////////////////////////////////////////////

bool
StDbManager::fetchAllTables(StDbConfigNode* node){

bool retVal=false;

 if(misVerbose){
   cout << "Retrieving Data from node = " << node->getName()<<endl;
 }

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
StDbManager::storeDbTable(StDbTable* table, bool commitWhenDone){
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
    if(!server->isConnected())server->init();
    if(!server->WriteDb(table,mstoreTime.munixTime))return retVal;

  }

if(commitWhenDone)table->commitData();
return true;
}

////////////////////////////////////////////////////////////////

bool
StDbManager::storeAllTables(StDbConfigNode* node, bool commitWhenDone){

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
      if(retVal){
        retVal = (retVal && storeDbTable(table, false));
      } else {
        if(table)table->commitData(); // prevent rollback of non-stored tables
      }
    }
  delete itr;
  }

if(!retVal) {
  rollBackAllTables(node);
  return retVal;
}

// do children & siblings
bool children = true;
bool siblings = true;

  if(node->hasChildren())children = storeAllTables(node->getFirstChildNode(),false);
  if(!children){
    rollBackAllTables(node);
    return false;
  }
 
  StDbConfigNode* nextNode = 0;
  if((nextNode=node->getNextNode()))siblings = storeAllTables(nextNode,false);
  if(!siblings){
    rollBackAllTables(node);
    return false;
  }

if(commitWhenDone)commitAllTables(node);

return true;
}

///////////////////////////////////////////////////////////////

int
StDbManager::storeConfig(StDbConfigNode* node, int currentID, bool commitWhenDone){

StDbServer* server=findServer(node->getDbType(),node->getDbDomain());
bool children = true;
bool siblings = true;
bool retVal = false;
if(!server->isConnected())server->init();
int nodeID=server->WriteDb(node,currentID);

if(nodeID){
  retVal = true;
  node->addWrittenNode(nodeID);
  if(node->hasChildren())children=storeConfig(node->getFirstChildNode(),nodeID,false);
  if(node->getNextNode())siblings=storeConfig(node->getNextNode(),currentID,false);
 
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

if(commitWhenDone) commitAllNodes(node);
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

  //bool retVal=false;

  if(node->hasData()){
    StDbTableIter* itr = node->getStDbTableIter();
    StDbNode* tnode = 0;
    //    retVal = true;
    while(!itr->done()){
      tnode = (StDbNode*)itr->next();
      rollBack(tnode);
    }
  delete itr;
  }

if(node->hasChildren()) rollBackAllNodes(node->getFirstChildNode());
StDbConfigNode* nextNode = 0;
if((nextNode=node->getNextNode())) rollBackAllNodes(nextNode);

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
if(!server)return false;
if(!server->isConnected())server->init();

return server->rollBack(node);
}

////////////////////////////////////////////////////////////////////////

bool
StDbManager::rollBack(StDbTable* table){

  int* dataRows;
  int numRows;
  bool retVal = false;
  if((dataRows=table->getWrittenRows(&numRows))){
     StDbServer* server=findServer(table->getDbType(),table->getDbDomain());
     if(!server) return false;
     if(!server->isConnected()) server->init();
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

  //bool retVal=true;

  if(node->hasData()){
    StDbTableIter* itr = node->getStDbTableIter();
    StDbNode* tnode = 0;
    //    retVal = true;
    while(!itr->done()){
      tnode = (StDbNode*)itr->next();
      if(tnode) tnode->commit();
    }
  delete itr;
  }

  //bool children = true;
  //bool siblings = true;

if(node->hasChildren())commitAllNodes(node->getFirstChildNode());
StDbConfigNode* nextNode = 0;
if((nextNode=node->getNextNode()))commitAllNodes(nextNode);

node->commit();

return true;
}

////////////////////////////////////////////////////////////////
void
StDbManager::closeAllConnections(){

  ServerList::iterator itr;
  StDbServer* server;

      for(itr = mservers.begin(); itr != mservers.end(); ++itr){
         server = *itr;
         if(server && (server->isConnected())) server->closeConnection();
        }
return;
}

////////////////////////////////////////////////////////////////
void
StDbManager::closeAllConnections(StDbConfigNode* node){

if(!node) return;

 closeConnection(node);
 if(node->hasChildren())closeAllConnections(node->getFirstChildNode());
 StDbConfigNode* next = node->getNextNode();
 if(next)closeAllConnections(next);
  
} 

////////////////////////////////////////////////////////////////
void
StDbManager::closeConnection(StDbNode* node){

if(!node) return;

StDbServer* server=findServer(node->getDbType(),node->getDbDomain());
if(server && (server->isConnected())) server->closeConnection();

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

//////////////////////////////////////////////////////////////////

void
StDbManager::setUser(const char* userName, const char* pWord){

  if(userName){
    if(muserName) delete [] muserName;
    muserName = new char[strlen(userName)+1];
    strcpy(muserName, userName);
  }

  if(pWord){
   if(mpWord) delete [] mpWord;
   mpWord = new char[strlen(pWord)+1];
   strcpy(mpWord,pWord);
  }

}
