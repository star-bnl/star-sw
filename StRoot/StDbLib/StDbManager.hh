/***************************************************************************
 *
 * $Id: StDbManager.hh,v 1.7 1999/10/19 14:30:39 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Manages access to Servers and passes Query-by-Table to db
 *
 ***************************************************************************
 *
 * $Log: StDbManager.hh,v $
 * Revision 1.7  1999/10/19 14:30:39  porter
 * modifications relevant to use with StDbBroker and future merging with
 * "params" database structure + some docs + suppressing diagnostics messages
 *
 * Revision 1.6  1999/09/30 02:06:07  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBMANAGER_HH
#define STDBMANAGER_HH

#include "StDbDefs.hh"
#include "parseXmlString.hh"
#include <fstream.h>
#include "StDbTime.h"

class dbType;
class dbDomain;
class StDbServer;
class StDbTable;
class StDbTableI;
class StDbConfigNode;
//class StDbTime;

#ifndef __CINT__
#include <list>
#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<dbType*,allocator<dbType*> > dbTypes;
typedef list<dbDomain*,allocator<dbDomain*> > dbDomains;
typedef list<StDbServer*,allocator<StDbServer*> > ServerList;
#else
typedef list<dbType*> dbTypes;
typedef list<dbDomain*> dbDomains;
typedef list<StDbServer*> ServerList;
#endif
#endif
#ifdef __CINT__
class dbTypes;
class dbDomains;
class ServerList;
#endif


class StDbManager {

private:

  dbTypes mTypes;  // enum mapping shortcut
  dbDomains mDomains; // enum mapping shortcut
  ServerList mservers;  // servers to handle the Query 
  parseXmlString mparser; // parses strings in XML

  StDbManager(): mhasServerList(false), mhasDefaultServer(false) { initTypes(); initDomains();};

  static StDbManager* mInstance;

  bool mhasServerList;
  bool mhasDefaultServer;
  StDbTime mcheckTime;  
  StDbTime mstoreTime;

protected:

  virtual void initServers(const char* refFile = 0);
  virtual void initTypes();
  virtual void initDomains();
  virtual void deleteServers();
  virtual void deleteTypes();
  virtual void deleteDomains();

  // These lookup up ServerInfo from XML files

  virtual void lookUpServers();
  virtual char* getFileName(const char* envName, const char* subDirName=0);
  virtual void findServersXml(ifstream& is);  
  virtual char* findServerString(ifstream& is);
  virtual char* getNextName(char*& name);

  char* mstringDup(const char* str);

public:

  static StDbManager* Instance(){
    if(!mInstance){
      mInstance = new StDbManager;
    }
   return mInstance;
  }

  virtual ~StDbManager();

  virtual StDbConfigNode* initConfig(const char* configName);
  virtual StDbConfigNode* initConfig(StDbType type, StDbDomain domain, const char* configName=0);

  virtual StDbServer* findServer(StDbType type, StDbDomain domain);
  virtual StDbServer* findDefaultServer();

  // helper functions to map enumeration to type

  virtual char* getDbTypeName(StDbType type);
  virtual char* getDbDomainName(StDbDomain domain);
  virtual StDbType getDbType(const char* typeName);
  virtual StDbDomain getDbDomain(const char* domainName);

  virtual void setRequestTime(unsigned int time);
  virtual void setRequestTime(const char* time);
  virtual unsigned int getUnixCheckTime();
  virtual char* getDateCheckTime();
  virtual void setStoreTime(unsigned int time);
  virtual void setStoreTime(const char* time);
  virtual unsigned int getUnixStoreTime();
  virtual char* getDateStoreTime();
  virtual bool getDataBaseInfo(const char* dbname, char*& type, char*& domain);
 
  //  virtual bool IsValid(StDbTableI* table, int time);
  //  virtual void fetchDbTable(StDbTableI* table, int time);

  virtual bool IsValid(StDbTableI* table);
  virtual void fetchDbTable(StDbTableI* table);
  virtual void storeDbTable(StDbTableI* table);
  virtual void storeAllTables(StDbConfigNode* node);

  // ClassDef(StDbManager,0)

};


#endif







