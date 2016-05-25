/***************************************************************************
 *
 * $Id: StDbServer.hh,v 1.13 2016/05/25 20:17:51 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Interface Base class of Server for DB-access
 *
 *              A Server is specified in this base class by :
 *                name, host, unix-socket, port-number
 *              When the default server flag is set, all databases that
 *                are not mapped to a specific server are accessed via
 *                the default server. This is implemented in the manager.
 *
 *
 ***************************************************************************
 *
 * $Log: StDbServer.hh,v $
 * Revision 1.13  2016/05/25 20:17:51  dmitry
 * coverity - uninit ctor
 *
 * Revision 1.12  2001/01/22 18:37:59  porter
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
 * Revision 1.11  2000/08/15 22:51:52  porter
 * Added Root2DB class from Masashi Kaneta
 * + made code more robust against requesting data from non-existent databases
 *
 * Revision 1.10  2000/03/01 20:56:16  porter
 * 3 items:
 *    1. activated reConnect for server timeouts
 *    2. activated connection sharing; better resource utilization but poorer
 *       logging
 *    3. made rollback method in mysqlAccessor more robust (affects writes only)
 *
 * Revision 1.9  2000/02/15 20:27:44  porter
 * Some updates to writing to the database(s) via an ensemble (should
 * not affect read methods & haven't in my tests.
 *  - closeAllConnections(node) & closeConnection(table) method to mgr.
 *  - 'NullEntry' version to write, with setStoreMode in table;
 *  -  updated both StDbTable's & StDbTableDescriptor's copy-constructor
 *
 * Revision 1.8  2000/01/27 05:54:34  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
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
#include "StDataBaseI.hh"

class StDbServer {

protected:
  
  char* mserverName = 0;
  char* mhostName = 0;
  char* munixSocket = 0;
  char* muserName = 0;
  char* mpword = 0;
  int   mportNumber = 0;
  bool  misDefault = false;

  char* mstringDup(const char * str) const;

public:

  // constructors & dtor
  StDbServer();
  StDbServer(const char* name, const char* host, const char* sock, int port);
  StDbServer(StDbServer& server);
  virtual ~StDbServer();

  virtual void setHostName(const char* name);
  virtual void setUnixSocket(const char* name);
  virtual void setPortNumber(int port);
  virtual void setServerName(const char* name);
  virtual void setUser(const char* name, const char* pword=0);

  // get methods for identifiers
  virtual char*      getServerName() const;
  virtual char*      getHostName()   const;
  virtual char*      getUnixSocket() const;
  virtual int        getPortNumber() const;
  virtual char*      printServerName();
  virtual char*      printHostName();
  virtual char*      printUnixSocket();
  virtual char*      printUser();
  virtual char*      printPword();

  virtual void       setIsDefaultServer();
  virtual bool       isDefault() const ;

  // set methods for identifiers
  virtual void addDataBase(StDbType type, StDbDomain domain)             =0; 
  virtual void addDataBase(const char* typeName, const char* domName)    =0;
  virtual StDataBaseI* useDb(StDbType type, StDbDomain domain)           =0;
  virtual StDataBaseI* useDb(const char* typeName, const char* domName)  =0;
  virtual StDataBaseI* useDb()                                           =0;
  // connection & check connections
  virtual bool  isConnected()                                            =0;
  virtual void  closeConnection()                                        =0;
  virtual void  setTimeLogging(bool isTimeLogged)                        =0;
  virtual double getQueryTimes()                                         =0;
  virtual double getSocketTimes()                                        =0;
  virtual double getConnectTimes()                                        =0;

};

inline void  StDbServer::setPortNumber(int port){ mportNumber = port;}
inline int   StDbServer::getPortNumber() const  { return mportNumber; }
inline char* StDbServer::printServerName()      { return mserverName; }
inline char* StDbServer::printHostName()        { return mhostName; }
inline char* StDbServer::printUnixSocket()      { return munixSocket; }
inline char* StDbServer::printUser()            { return muserName; }
inline char* StDbServer::printPword()           { return mpword; }
inline void  StDbServer::setIsDefaultServer()   { misDefault=true; }
inline bool  StDbServer::isDefault() const      { return misDefault; }

#endif
