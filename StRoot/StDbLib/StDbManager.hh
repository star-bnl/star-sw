/***************************************************************************
 *
 * $Id: StDbManager.hh,v 1.18 2000/06/02 13:37:37 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Manages access to Servers and passes Query-by-Table to db
 *
 ***************************************************************************
 *
 * $Log: StDbManager.hh,v $
 * Revision 1.18  2000/06/02 13:37:37  porter
 * built up list of minor changes:
 *  - made buffer more robust for certain null inputs
 *  - fixed small leak in StDbTables & restructure call to createMemory
 *  - added dbRhic as a database domain in StDbDefs
 *  - added setUser() in StDbManager
 *  - added more diagnostic printouts in mysqlAccessor.cc
 *
 * Revision 1.17  2000/03/28 17:03:19  porter
 * Several upgrades:
 * 1. configuration by timestamp for Conditions
 * 2. query by whereClause made more systematic
 * 3. conflict between db-stored comments & number lists resolved
 * 4. ensure endtime is correct for certain query falures
 * 5. dbstl.h->handles ObjectSpace & RogueWave difference (Online vs Offline)
 *
 * Revision 1.16  2000/03/01 20:56:16  porter
 * 3 items:
 *    1. activated reConnect for server timeouts
 *    2. activated connection sharing; better resource utilization but poorer
 *       logging
 *    3. made rollback method in mysqlAccessor more robust (affects writes only)
 *
 * Revision 1.15  2000/02/24 20:30:46  porter
 * fixed padding for uchar; beginTime in mysqlAccessor;
 * added rollback safety checkes in StDbManger
 *
 * Revision 1.14  2000/02/18 16:58:09  porter
 * optimization of table-query, + whereClause gets timeStamp if indexed
 *  + fix to write multiple rows algorithm
 *
 * Revision 1.13  2000/02/15 20:27:44  porter
 * Some updates to writing to the database(s) via an ensemble (should
 * not affect read methods & haven't in my tests.
 *  - closeAllConnections(node) & closeConnection(table) method to mgr.
 *  - 'NullEntry' version to write, with setStoreMode in table;
 *  -  updated both StDbTable's & StDbTableDescriptor's copy-constructor
 *
 * Revision 1.12  2000/01/27 05:54:34  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
 * Revision 1.11  2000/01/19 20:20:05  porter
 * - finished transaction model needed by online
 * - fixed CC5 compile problem in StDbNodeInfo.cc
 * - replace TableIter class by StDbTableIter to prevent name problems
 *
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
#include "tableQuery.hh"

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
typedef list<tableQuery*,allocator<tableQuery*> > QueryObjects;
#else
#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif
typedef list<dbType*> dbTypes;
typedef list<dbDomain*> dbDomains;
typedef list<StDbServer*> ServerList;
typedef list<tableQuery*> QueryObjects;
#endif
#endif
#ifdef __CINT__
class dbTypes;
class dbDomains;
class ServerList;
class QueryObjects;
#endif


class StDbManager {

private:

  bool misVerbose; // these give 3 levels of verbosity: verbose, normal, quiet.
  bool misQuiet;   // normal=!misVerbose && !misQuiet
  dbTypes mTypes;  // enum mapping shortcut
  dbDomains mDomains; // enum mapping shortcut
  ServerList mservers;  // servers to handle the Query 
  QueryObjects mqobjects;
  parseXmlString mparser; // parses strings in XML

  StDbManager(): misVerbose(false), misQuiet(false), mhasServerList(false), mhasDefaultServer(false), muserName(0), mpWord(0)  { 
                                                   initTypes(); 
                                                   initDomains(); 
                                                };

  // singleton pointer
  static StDbManager* mInstance;

  bool mhasServerList;
  bool mhasDefaultServer;
  StDbTime mcheckTime;  
  StDbTime mstoreTime;
  
  char* muserName;
  char* mpWord;
  
protected:

  // server methods needed internally
  //  virtual void initServers(const char* refFile = 0);
  virtual void initTypes();
  virtual void addDbType(StDbType type, const char* typeName);
  virtual void initDomains();
  virtual void deleteServers();
  virtual void deleteTypes();
  virtual void deleteDomains();
  virtual void deleteQueryObjects();

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
  virtual StDbConfigNode* initConfig(const char* databaseName, const char* configName, int opt=0);
  virtual StDbConfigNode* initConfig(StDbType type, StDbDomain domain);
  virtual StDbConfigNode* initConfig(StDbType type, StDbDomain domain, const char* configName, int opt=0);
  virtual StDbConfigNode* initConfig(StDbType type, StDbDomain domain, unsigned int requestTime, int opt=0);
  virtual StDbConfigNode* initConfig(const char* databaseName, unsigned int requestTime, int opt=0);

  // message verbosity level
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
  virtual bool storeDbTable(StDbTable* table, bool commitWhenDone=true);
  virtual bool storeAllTables(StDbConfigNode* node, bool commitWhenDone=true);
  virtual int  storeConfig(StDbConfigNode* node, int currentID=0, bool commitWhenDone=true);

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

  virtual char* userName();
  virtual char* pWord();
  virtual void  setUser(const char* userName, const char* pWord);

  // make ROOT-CLI via star-ofl "makefiles"
  // ClassDef(StDbManager,0)

};

inline char* StDbManager::userName() { return muserName; }
inline char* StDbManager::pWord() { return mpWord; }

#endif







