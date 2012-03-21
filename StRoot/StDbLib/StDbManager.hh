/***************************************************************************
 *
 * $Id: StDbManager.hh,v 1.24 2011/02/10 17:30:42 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Manages access to Servers and passes Query-by-Table to db
 *               --> Now a pure virtual interface
 *
 ***************************************************************************
 *
 * $Log: StDbManager.hh,v $
 * Revision 1.24  2011/02/10 17:30:42  dmitry
 * added an option to blacklist domains
 *
 * Revision 1.23  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.22  2003/09/16 22:44:17  porter
 * got rid of all ostrstream objects; replaced with ostringstream+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.21  2003/09/02 17:57:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.20  2001/02/09 23:06:24  porter
 * replaced ostrstream into a buffer with ostrstream creating the
 * buffer. The former somehow clashed on Solaris with CC5 iostream (current .dev)
 *
 * Revision 1.19  2001/01/22 18:37:55  porter
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

#include <string.h>
#include "StString.h"

#include "stdb_streams.h"

// real basic definitions
#include "StDbDefs.hh"
// pure virtual classes
#include "StDbMessService.hh"

// other classes
class StDbServer;
class StDataBaseI;
class dbEnvList;
class StDbNode;
class StDbConfigNode;
class StDbTable;

#ifdef __ROOT__
#include "TROOT.h"
#endif

class StDbManager {

protected:

  bool misVerbose;         //! 3 levels of verbosity: verbose, normal, quiet.
  bool misQuiet;           //!   where normal=!misVerbose && !misQuiet
  StDbMessService* Messenger; //! 
  char* muserName;
  char* mpWord;

  bool   misTimeLogged;  //! flag for logging timing

  StDbManager();
  static StDbManager* mInstance;

  char*         mstringDup(const char* str); //!  strdup(..) is not ANSI

public:

  // access to this singleton object
  static StDbManager* Instance();
  virtual ~StDbManager();

  // Act as Factory object for StDbConfigNodess
  // --> get dbHandle & Container of StDbTables..
  virtual StDbConfigNode* initConfig(const char* dbName)                  =0;
  virtual StDbConfigNode* initConfig(const char* dbName, 
                                     const char* configName, int opt=0)   =0;
  virtual StDbConfigNode* initConfig(StDbType type, StDbDomain domain)    =0;
  virtual StDbConfigNode* initConfig(StDbType type, StDbDomain domain, 
                                     const char* configName, int opt=0)   =0;
  virtual StDbConfigNode* initConfig(StDbType type, StDbDomain domain, 
                                     unsigned int requestTime, int opt=0) =0;
  virtual StDbConfigNode* initConfig(const char* dbName, 
                                     unsigned int requestTime, int opt=0) =0;

  virtual char*           getConfigNodeName(StDbType t, StDbDomain d)     =0;
  virtual char*           getExternalVersion(StDbType t,StDbDomain d)     =0;
  virtual dbEnvList*      getEnvList(const char* name)                    =0;

  // act as a Factory object for StDbTables
  virtual StDbTable*      newDbTable(const char* dbName,const char* tName)=0;
  virtual StDbTable*      newDbTable(StDbNode* node)                      =0;

  // Retreive DbLib owned database & server poointers by various methods
  virtual StDataBaseI* findDb(StDbType type, StDbDomain domain)          =0;
  virtual StDataBaseI* findDb(const char* dbType, const char* dbDomain)  =0;
  virtual StDataBaseI* findDb(const char* databaseName)                  =0;
  virtual StDbServer*  findServer(StDbType type, StDbDomain domain)      =0;
  virtual StDbServer*  findServer(StDbNode* node)                        =0;
  virtual StDbServer*  findServer(const char* type, const char* domain)  =0;
  virtual StDbServer*  findServer(const char* databaseName)              =0;
  virtual StDbServer*  findDefaultServer()                               =0;

  // helper functions using mapped enumeration to dbType & dbDomain
  virtual char*      getDbTypeName(StDbType type)                        =0;
  virtual char*      getDbDomainName(StDbDomain domain)                  =0;
  virtual StDbType   getDbType(const char* typeName)                     =0;
  virtual StDbDomain getDbDomain(const char* domainName)                 =0;
  virtual char*      getDbName(const char* typeName, const char* domName)=0;
  virtual char*      printDbName(StDbType type, StDbDomain domain)       =0;

  virtual void       blacklistDbDomain(const char* domainName)           =0;

  // time stamp methods
  virtual void         setRequestTime(unsigned int time)                 =0;
  virtual void         setRequestTime(const char* time)                  =0;
  virtual unsigned int getUnixRequestTime()                              =0;
  virtual char*        getDateRequestTime()                              =0;
  virtual unsigned int getUnixCheckTime()                                =0;
  virtual char*        getDateCheckTime()                                =0;
  virtual void         setStoreTime(unsigned int time)                   =0;
  virtual void         setStoreTime(const char* time)                    =0;
  virtual unsigned int getUnixStoreTime()                                =0;
  virtual char*        getDateStoreTime()                                =0;

  // find the dbType & dbDomain for a database=>dbname
  virtual bool getDataBaseInfo(const char* dbname, 
                               char*& type, char*& domain)               =0;
  virtual bool getDataBaseInfo(const char* dbname,
                               StDbType& type, StDbDomain& domain)       =0;
  
  // fetch & store methods for data
  virtual bool IsValid(StDbTable* table)                                 =0;
  virtual bool fetchDbTable(StDbTable* table)                            =0;
  virtual bool fetchDbTable(StDbTable* table, char* whereClause)         =0;
  virtual bool fetchAllTables(StDbConfigNode* node)                      =0;
  virtual bool storeDbTable(StDbTable* table, bool commitWhenDone=true)  =0;
  virtual bool storeAllTables(StDbConfigNode* node, 
                              bool commitWhenDone=true)                  =0;
  virtual int  storeConfig(StDbConfigNode* node, int currentID, 
                           int& configID, bool commitWhenDone=true)      =0;

  // transaction methods 
  virtual bool rollBackAllTables(StDbConfigNode* node)                   =0;
  virtual bool rollBackAllNodes(StDbConfigNode* node)                    =0; 
  virtual bool rollBack(StDbNode* node)                                  =0;   
  virtual bool rollBack(StDbTable* table)                                =0; 
  virtual bool commitAllTables(StDbConfigNode* node)                     =0; 
  virtual bool commitAllNodes(StDbConfigNode* node)                      =0;  

  // close connection methods
  virtual void closeAllConnections()                                     =0; 
  virtual void closeAllConnections(StDbConfigNode* node)                 =0; 
  virtual void closeConnection(StDbNode* node)                           =0;

  virtual void printTimeStats()                                          =0;

  // messaging service directions *** These are real functions *** 
  virtual bool    IsVerbose() const;
  virtual void    setVerbose(bool isVerbose);
  virtual bool    IsQuiet() const ;
  virtual void    setQuiet(bool isQuiet);
  virtual void    turnOffTimeLogging();
  virtual void    updateMessLevel();
  virtual void    setMessenger(StDbMessService* service);
  virtual StDbMessService* getMessenger();
  virtual int  printInfo(const char* m1, StDbMessLevel ml, 
                         int lineNumber=0, const char* className=" ",
                         const char* methodName=" ") ;
  virtual int  printInfo(const char* m1,  const char* m2,
                         StDbMessLevel ml, int lineNumber=0,
                         const char* className=" ",
                         const char* methodName=" ");

  virtual char* userName();
  virtual char* pWord();
  virtual void  setUser(const char* userName, const char* pWord);

  // make ROOT-CLI via star-ofl "makefiles"
#ifdef __ROOT__
  ClassDef(StDbManager,0)
#endif

};

inline bool StDbManager::IsVerbose() const { return misVerbose;};
inline void StDbManager::setVerbose(bool isVerbose){ 
 misVerbose=isVerbose;
 if(isVerbose)misQuiet=false;
 updateMessLevel();   
};
inline bool StDbManager::IsQuiet() const { return misQuiet;};
inline void StDbManager::setQuiet(bool isQuiet){ 
   misQuiet=isQuiet;
   if(isQuiet) misVerbose=false;
   updateMessLevel();
}
inline void StDbManager::updateMessLevel(){
 Messenger->setMessLevel(dbMWarn);
 if(misQuiet)Messenger->setMessLevel(dbMErr);
 if(misVerbose)Messenger->setMessLevel(dbMDebug);
}
inline void StDbManager::setMessenger(StDbMessService* service){
  delete Messenger;
  Messenger=service;
  updateMessLevel();
}
inline StDbMessService* StDbManager::getMessenger(){ return Messenger; }
inline char* StDbManager::userName() { return muserName; }
inline char* StDbManager::pWord()    { return mpWord; }

#endif









