/***************************************************************************
 *
 * $Id: StDbServer.hh,v 1.7 2000/01/19 20:20:07 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Server class for DB-access
 *
 ***************************************************************************
 *
 * $Log: StDbServer.hh,v $
 * Revision 1.7  2000/01/19 20:20:07  porter
 * - finished transaction model needed by online
 * - fixed CC5 compile problem in StDbNodeInfo.cc
 * - replace TableIter class by StDbTableIter to prevent name problems
 *
 * Revision 1.6  2000/01/10 20:37:54  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.5  1999/09/30 02:06:09  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBSERVER_HH
#define STDBSERVER_HH

#include "StDbDefs.hh"    // enumeration of type & domain

class StDbNode;
class StDbTable;
class StDbConfigNode;

#include "tableQuery.hh" // includes the query

class StDbServer {

private:
  
  char* mserverName;
  char* mhostName;
  char* munixSocket;
  int   mportNumber;
  char* mdbName;

  StDbType mdbType;
  StDbDomain mdbDomain;
  char* mdomainName;
  char* mtypeName;
  bool  mconnectState;
  bool  misDefault;

 
protected:

  virtual bool initServer();
  char* mstringDup(const char * str) const;

public:

  tableQuery* mdatabase; // low level query access of DataBase

  // constructors & dtor
  StDbServer(): mserverName(0), mhostName(0), munixSocket(0), mdbName(0),  
                mdbType(dbStDb), mdbDomain(dbDomainUnknown), mdomainName(0), 
                mtypeName(0), mconnectState(false), 
                misDefault(false), mdatabase(0) {};

  StDbServer(StDbServer& server);
  StDbServer(StDbType type, StDbDomain domain);
  StDbServer(StDbType type, StDbDomain domain, 
             const char* typeName, const char* domainName);

  virtual ~StDbServer();


  // set methods for identifiers
  virtual void setDbName(const char* dbName);
  virtual void setDataBase(StDbType type, StDbDomain domain, 
                           const char* typeName, const char* domainName);
  virtual void setHostName(const char* name);
  virtual void setUnixSocket(const char* name);
  virtual void setDomainName(const char* name);
  virtual void setTypeName(const char* name);
  virtual void setPortNumber(int port){ mportNumber = port;}
  virtual void setDbType(StDbType type) { mdbType = type;}
  virtual void setDbDomain(StDbDomain domain) { mdbDomain=domain;}
  virtual void setServerName(const char* name);
  virtual void setIsDefaultServer() {misDefault=true;};

  // get methods for identifiers
  virtual char*      getDbName() const;
  virtual StDbType   getDbType() const { return mdbType; };
  virtual StDbDomain getDbDomain() const { return mdbDomain; };
  virtual char*      getServerName() const ;
  virtual char*      getHostName() const ;
  virtual char*      getUnixSocket() const;
  virtual int        getPortNumber() const { return mportNumber;} ;
  virtual char*      getTypeName() const ;
  virtual char*      getDomainName() const ;

  // connection & check connections
  virtual void  init() { initServer(); };
  virtual bool  isconnected() const { return mconnectState;};
  virtual bool  isDefault() const { return misDefault; };
  virtual bool  reConnect();

  // timestamp translation
  virtual unsigned int getUnixTime(const char* dateTime);
  virtual char*        getDateTime(unsigned int time);
  
  // Query access to the DataBase
  virtual bool QueryDb(StDbTable* table, unsigned int reqTime);
  virtual bool QueryDb(StDbTable* table, const char* whereClause);
  virtual int  WriteDb(StDbConfigNode* node, int currentID);

  virtual bool QueryDb(StDbConfigNode* node);
  virtual bool WriteDb(StDbTable* table, unsigned int storeTime);

  virtual bool rollBack(StDbNode* node);
  virtual bool rollBack(StDbTable* table);
  virtual bool QueryDescriptor(StDbTable* table);

  // provide direct SQL access
  virtual tableQuery* getQueryObject();
 
};

inline
tableQuery* 
StDbServer::getQueryObject(){ return mdatabase;}


#endif
















