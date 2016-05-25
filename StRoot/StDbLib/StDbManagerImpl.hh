/***************************************************************************
 *
 * $Id: StDbManagerImpl.hh,v 1.10 2016/05/25 20:17:51 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Manages access to Servers and passes Query-by-Table to db
 *               --> Now the Implementation of the virtual StDbManager class
 *
 ***************************************************************************
 *
 * $Log: StDbManagerImpl.hh,v $
 * Revision 1.10  2016/05/25 20:17:51  dmitry
 * coverity - uninit ctor
 *
 * Revision 1.9  2012/06/11 14:33:47  fisyak
 * std namespace
 *
 * Revision 1.8  2011/02/10 17:30:42  dmitry
 * added an option to blacklist domains
 *
 * Revision 1.7  2007/08/20 18:21:29  deph
 * New Version of Load Balancer
 *
 * Revision 1.6  2007/01/09 16:27:40  deph
 * Updates for load balancing "added 1)write privilege 2)xml comments 3)camelCase notation
 *
 * Revision 1.5  2006/11/16 21:50:40  deph
 * additional files needed for db load balancing
 *
 * Revision 1.4  2006/08/17 02:58:57  deph
 * updated load balancer - removing hard-coded nodes from API to xml
 *
 * Revision 1.3  2003/09/16 22:44:17  porter
 * got rid of all ostrstream objects; replaced with ostringstream+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.2  2003/09/02 17:57:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2001/01/22 18:37:56  porter
 * Update of code needed in next year running. This update has little
 * effect on the interface (only 1 method has been changed in the interface).
 * Code also preserves backwards compatibility so that old versions of
 * StDbLib can read new table structures.
 *  -Important features:
 *    a. more efficient low-level table structure (see StDbSql.cc)
 *    b. more flexible indexing for new systems (see StDbElememtIndex.cc)
 *    c. environment variable override KEYS for each database
 *    d. StMessage support & clock-time logging diagnostics
 *  -Cosmetic features
 *    e. hid stl behind interfaces (see new *Impl.* files) to again allow rootcint access
 *    f. removed codes that have been obsolete for awhile (e.g. db factories)
 *       & renamed some classes for clarity (e.g. tableQuery became StDataBaseI
 *       and mysqlAccessor became StDbSql)
 *
 **************************************************************************/
#ifndef STDBMANAGERIMPL_HH
#define STDBMANAGERIMPL_HH

#include "StDbManager.hh"
#include "parseXmlString.hh"
#include "stdb_streams.h"
#include "StDbTime.h"
#include "StDataBaseI.hh"
#include "StDbTableFactory.hh"
#include "StDbMessService.hh"
#include "StDbConfigNode.hh"
#include "StDbTable.h"
#include "StDbLogger.hh"
#include "StDbServiceBroker.h"

/********* helper classes (c-structs) *************/
class dbType{
public:
  StDbType type;
  char name[64];
  dbType(StDbType atype,const char* aname): type(atype) { 
                                           if(aname)strcpy(name,aname);};
};
class dbDomain{
public:
  StDbDomain domain;
  char name[64];
  dbDomain(StDbDomain adom,const char* aname): domain(adom) {
                                               if(aname)strcpy(name,aname);};
};

#define MAX_ENV_VARS 50
class dbEnvList {
public:
  dbEnvList(): num(0) {
	for ( int i = 0; i < MAX_ENV_VARS; i++) {
		envVar[i] = 0; envDef[i] = 0;
	}
  }; 
  ~dbEnvList() {
    for(int i=0;i<num;i++){
      delete [] envVar[i];
      delete [] envDef[i];
    }
  }
  int num;
  char* envVar[MAX_ENV_VARS];
  char* envDef[MAX_ENV_VARS];
};
/*************** end helper class definitions ****************/

#include <list>
#include <set>
#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<dbType*,allocator<dbType*> > dbTypes;
typedef list<dbDomain*,allocator<dbDomain*> > dbDomains;
typedef list<StDbServer*,allocator<StDbServer*> > ServerList;
typedef set<std::string,allocator<std::string> > dbDomainBlacklist;
#else
#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif
typedef list<dbType*> dbTypes;
typedef list<dbDomain*> dbDomains;
typedef list<StDbServer*> ServerList;
typedef std::set<std::string> dbDomainBlacklist;
#endif



class StDbManagerImpl : public StDbManager {

  friend class StDbManager; //! in order to create "singleton" with inheritance

private:

  dbTypes        mTypes;          //! enum list->mapping shortcut
  dbDomains      mDomains;        //! enum list->mapping shortcut
  StDbType       dbTypeFree;      //! enum of last unspecified type 
  StDbDomain     dbDomainFree;    //! enum of last unspecified domain
  ServerList     mservers;        //! servers to handle the Query 
  dbDomainBlacklist mBlacklist;   //! set of blacklisted domains
  parseXmlString mparser;         //! parses strings in XML

  bool mhasServerList;
  bool mhasDefaultServer;
  StDbTime mcheckTime;  
  StDbTime mstoreTime;  
  StDbTableFactory* mfactory;
  StDbLogger mnodeLog;
  StDbLogger mdataLog;
  
protected:

  // protected constructor
  StDbManagerImpl();

public:

  virtual ~StDbManagerImpl();

  virtual void turnOffTimeLogging();
  // Factory class for Container of StDbTables..
  virtual StDbConfigNode* initConfig(const char* databaseName);
  virtual StDbConfigNode* initConfig(const char* databaseName, 
                                     const char* configName, int opt=0);
  virtual StDbConfigNode* initConfig(StDbType type, StDbDomain domain);
  virtual StDbConfigNode* initConfig(StDbType type, StDbDomain domain, 
                                     const char* configName, int opt=0);
  virtual StDbConfigNode* initConfig(StDbType type, StDbDomain domain, 
                                     unsigned int requestTime, int opt=0);
  virtual StDbConfigNode* initConfig(const char* databaseName, 
                                     unsigned int requestTime, int opt=0);

  virtual char*           getConfigNodeName(StDbType type, StDbDomain domain);
  virtual char*           getExternalVersion(StDbType type,StDbDomain domain);
  virtual dbEnvList*      getEnvList(const char* name);

  virtual StDbTable*      newDbTable(const char* dbName,const char* tabName);
  virtual StDbTable*      newDbTable(StDbNode* node);

  // find databases & servers by various methods
  virtual StDataBaseI*    findDb(StDbType type, StDbDomain domain);
  virtual StDataBaseI*    findDb(const char* dbType, const char* dbDomain);
  virtual StDataBaseI*    findDb(const char* databaseName);

  virtual StDbServer*     findServer(StDbType type, StDbDomain domain);
  virtual StDbServer*     findServer(StDbNode* node);
  virtual StDbServer*     findServer(const char* dbType, const char* dbDomain);
  virtual StDbServer*     findServer(const char* databaseName);
  virtual StDbServer*     findDefaultServer();

  // helper functions to map enumeration to type
  virtual char*           getDbTypeName(StDbType type);
  virtual char*           getDbDomainName(StDbDomain domain);
  virtual StDbType        getDbType(const char* typeName);
  virtual StDbDomain      getDbDomain(const char* domainName);
  virtual char*           getDbName(const char* typeName, 
                                    const char* domainName);
  virtual char*           printDbName(StDbType type, StDbDomain domain);

  virtual void            blacklistDbDomain(const char* domainName); 

  // time stamp methods
  virtual void            setRequestTime(unsigned int time);
  virtual void            setRequestTime(const char* time);
  virtual unsigned int    getUnixRequestTime();
  virtual char*           getDateRequestTime();
  virtual unsigned int    getUnixCheckTime();
  virtual char*           getDateCheckTime();
  virtual void            setStoreTime(unsigned int time);
  virtual void            setStoreTime(const char* time);
  virtual unsigned int    getUnixStoreTime();
  virtual char*           getDateStoreTime();

  // find the dbType & dbDomain for a database=>dbname
  virtual bool            getDataBaseInfo(const char* dbname, 
                                          char*& type, char*& domain);
  virtual bool            getDataBaseInfo(const char* dbname,
                                          StDbType& type, StDbDomain& domain);

  // fetch & store methods for data
  virtual bool IsValid(StDbTable* table);
  virtual bool fetchDbTable(StDbTable* table);
  virtual bool fetchDbTable(StDbTable* table, char* whereClause);
  virtual bool fetchAllTables(StDbConfigNode* node);
  virtual bool storeDbTable(StDbTable* table, bool commitWhenDone=true);
  virtual bool storeAllTables(StDbConfigNode* node, bool commitWhenDone=true);
  virtual int  storeConfig(StDbConfigNode* node, int currentID, 
                           int& configID, bool commitWhenDone=true);

  // transaction methods 
  //  nodes are node entries in the "Nodes" table
  //  tables are real data entries in a data+dataIndex table
  virtual bool rollBackAllTables(StDbConfigNode* node); //! for table data
  virtual bool rollBackAllNodes(StDbConfigNode* node); //!  for node data
  virtual bool rollBack(StDbNode* node);   //! for node data
  virtual bool rollBack(StDbTable* table); //! for table data
  virtual bool commitAllTables(StDbConfigNode* node); //! table commits
  virtual bool commitAllNodes(StDbConfigNode* node);  //! node  commits 

  virtual void closeAllConnections(); 
  virtual void closeAllConnections(StDbConfigNode* node); 
  virtual void closeConnection(StDbNode* node);

  virtual void printTimeStats();


  short xmlInputSource;

  // MLK added for load balacing:
#ifndef NoXmlTreeReader
  StDbServiceBroker* myServiceBroker;
  
#endif

protected:
  // initializers for standard types & domains
  virtual void initTypes();
  virtual void initDomains();
  virtual void addDbType(StDbType type, const char* typeName);
  virtual void addDbDomain(StDbDomain domain, const char* domainName);

  // assigns an available user type or domain to a user supplied name
  virtual StDbType newDbType(const char* typeName);
  virtual StDbDomain newDbDomain(const char* domainName);

  virtual void deleteServers();
  virtual void deleteTypes();
  virtual void deleteDomains();

  // These lookup up ServerInfo from XML files
  virtual void  lookUpServers();
  virtual void  findServersXml(ifstream& is);  
  virtual char* findServerString(ifstream& is);
  virtual char* getNextName(char*& name);

  // ensure both unix & date timestamp are in-sinc.
  void          updateDateTime(StDbTime& t);
  void          updateUnixTime(StDbTime& t);


};

#endif









