#ifndef STDBMANAGER_HH
#define STDBMANAGER_HH

#include "StDbDefs.hh"
//#include "StDbLists.hh"
//#include "StDbConfigNode.hh"
//#include "StDbServer.hh"

class dbType;
class dbDomain;
class StDbServer;
class StDbTableComponent;
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
  StDbConfigNode* m_configNode;  // handles the StDbTables & Named Configurations

  StDbManager(){ initTypes(); initDomains(); initServers();};
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

  virtual StDbConfigNode* getConfig() { return m_configNode; };
  virtual StDbConfigNode* initConfig(const char* configName);
  //  virtual StDbConfigNode* initConfig();
  virtual StDbConfigNode* resetConfig(StDbType type, StDbDomain domain, const char* configName);

  virtual StDbServer* findServer(StDbType type, StDbDomain domain);

  // helper functions to map enumeration to type
  virtual char* getDbTypeName(StDbType type);
  virtual char* getDbDomainName(StDbDomain domain);
  virtual StDbType getDbType(const char* typeName);
  virtual StDbDomain getDbDomain(const char* domainName);

  virtual bool IsValid(StDbTableComponent* table, int time);
  virtual void fetchDbTable(StDbTableComponent* table, int time);


};


#endif







