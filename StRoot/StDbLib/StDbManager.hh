#ifndef STDBMANAGER_HH
#define STDBMANAGER_HH

#include "StDbDefs.hh"

class dbType;
class dbDomain;
class StDbServer;
class StDbTable;
class StDbTableI;
class StDbConfigNode;

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

  ServerList mservers;  // servers handle the Query 

  StDbManager() { initTypes(); initDomains(); };
  static StDbManager* mInstance;
  
protected:

  virtual void initServers(const char* refFile = 0);
  virtual void initTypes();
  virtual void initDomains();
  virtual void deleteServers();
  virtual void deleteTypes();
  virtual void deleteDomains();

public:

  static StDbManager* Instance(){
    if(!mInstance){
      mInstance = new StDbManager;
    }
   return mInstance;
  }

  virtual ~StDbManager();

  virtual StDbConfigNode* initConfig(const char* configName);
  virtual StDbConfigNode* initConfig(StDbType type, StDbDomain domain, const char* configName);

  virtual StDbServer* findServer(StDbType type, StDbDomain domain);

  // helper functions to map enumeration to type

  virtual char* getDbTypeName(StDbType type);
  virtual char* getDbDomainName(StDbDomain domain);
  virtual StDbType getDbType(const char* typeName);
  virtual StDbDomain getDbDomain(const char* domainName);

  virtual bool IsValid(StDbTableI* table, int time);
  virtual void fetchDbTable(StDbTableI* table, int time);

};


#endif







