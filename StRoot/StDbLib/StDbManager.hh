/***************************************************************************
 *
 * $Id: StDbManager.hh,v 1.10 2000/01/10 20:37:54 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Manages access to Servers and passes Query-by-Table to db
 *
 ***************************************************************************
 *
 * $Log: StDbManager.hh,v $
 * Revision 1.10  2000/01/10 20:37:54  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.9  1999/12/28 21:31:42  porter
 * added 'using std::vector' and 'using std::list' for Solaris CC5 compilation.
 * Also fixed some warnings arising from the CC5 compiles
 *
 * Revision 1.8  1999/12/03 22:24:01  porter
 * expanded functionality used by online, fixed bug in
 * mysqlAccessor::getElementID(char*), & update StDbDataSet to
 * conform to changes in Xml reader & writer
 *
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
class StDbConfigNode;
class StDbNode;

#ifndef __CINT__
#include <list>


#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<dbType*,allocator<dbType*> > dbTypes;
typedef list<dbDomain*,allocator<dbDomain*> > dbDomains;
typedef list<StDbServer*,allocator<StDbServer*> > ServerList;
#else
using std::list;
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

  bool misVerbose; // these give 3 levels of verbosity: verbose, normal, quiet.
  bool misQuiet;   // normal=!misVerbose && !misQuiet
  dbTypes mTypes;  // enum mapping shortcut
  dbDomains mDomains; // enum mapping shortcut
  ServerList mservers;  // servers to handle the Query 
  parseXmlString mparser; // parses strings in XML

  StDbManager(): misVerbose(false), misQuiet(false), mhasServerList(false), mhasDefaultServer(false)             { 
                                        initTypes(); 
                                        initDomains(); 
                                   };

  // singleton pointer
  static StDbManager* mInstance;

  bool mhasServerList;
  bool mhasDefaultServer;
  StDbTime mcheckTime;  
  StDbTime mstoreTime;

protected:

  // server methods needed internally
  //  virtual void initServers(const char* refFile = 0);
  virtual void initTypes();
  virtual void addDbType(StDbType type, const char* typeName);
  virtual void initDomains();
  virtual void deleteServers();
  virtual void deleteTypes();
  virtual void deleteDomains();

  // These lookup up ServerInfo from XML files

  virtual void  lookUpServers();
  virtual char* getFileName(const char* envName, const char* subDirName=0);
  virtual void  findServersXml(ifstream& is);  
  virtual char* findServerString(ifstream& is);
  virtual char* getNextName(char*& name);

  char* mstringDup(const char* str); //  strdup(..) is not ANSI

public:

  // access to this singleton object

  static StDbManager* Instance(){
    if(!mInstance){
      mInstance = new StDbManager;
    }
   return mInstance;
  }

  virtual ~StDbManager();

  // get Container for StDbTables..
  virtual StDbConfigNode* initConfig(const char* databaseName);
  virtual StDbConfigNode* initConfig(const char* databaseName, const char* configName);
  virtual StDbConfigNode* initConfig(StDbType type, StDbDomain domain);
  virtual StDbConfigNode* initConfig(StDbType type, StDbDomain domain, const char* configName);

  virtual bool IsVerbose() const { return misVerbose;};
  virtual void setVerbose(bool isVerbose){ misVerbose=isVerbose;
                                           if(isVerbose) misQuiet=false;};
  virtual bool IsQuiet() const { return misQuiet;};
  virtual void setQuiet(bool isQuiet){ misQuiet=isQuiet;
                                       if(isQuiet)misVerbose=false;};

  // find servers by various methods
  virtual StDbServer* findServer(StDbType type, StDbDomain domain);
  virtual StDbServer* findServer(StDbNode* node);
  virtual StDbServer* findServer(const char* dbType, const char* dbDomain);
  virtual StDbServer* findServer(const char* databaseName);
  virtual StDbServer* findDefaultServer();

  // helper functions to map enumeration to type
  virtual char*      getDbTypeName(StDbType type);
  virtual char*      getDbDomainName(StDbDomain domain);
  virtual StDbType   getDbType(const char* typeName);
  virtual StDbDomain getDbDomain(const char* domainName);

  // time stamp methods
  virtual void         setRequestTime(unsigned int time);
  virtual void         setRequestTime(const char* time);
  virtual unsigned int getUnixCheckTime();
  virtual char*        getDateCheckTime();
  virtual void         setStoreTime(unsigned int time);
  virtual void         setStoreTime(const char* time);
  virtual unsigned int getUnixStoreTime();
  virtual char*        getDateStoreTime();

  // find the dbType & dbDomain for a database=>dbname
  virtual bool getDataBaseInfo(const char* dbname, char*& type, char*& domain);
 

  // fetch & store methods for data
  virtual bool IsValid(StDbTable* table);
  virtual bool fetchDbTable(StDbTable* table);
  virtual bool fetchDbTable(StDbTable* table, char* whereClause);
  virtual bool fetchAllTables(StDbConfigNode* node);
  virtual bool storeDbTable(StDbTable* table);
  virtual bool storeAllTables(StDbConfigNode* node);
  virtual int  storeConfig(StDbConfigNode* node, int currentID=0);

  // make ROOT-CLI via star-ofl "makefiles"
  // ClassDef(StDbManager,0)

};


#endif







