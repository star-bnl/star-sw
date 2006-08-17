/***************************************************************************
 *
 * $Id: StDbServerImpl.hh,v 1.2 2006/08/17 02:58:58 deph Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Implements Server Interface class for DB-access
 *     
 *              A Server is specified in StDbServer base class by :
 *                name, host, unix-socket, port-number
 *              A Server contains a list of databases distinquishable by names.
 *              The currentDb us specified by "useDb" methods
 *              The Server knows only the db-list built via "add" methods.
 *
 ***************************************************************************
 *
 * $Log: StDbServerImpl.hh,v $
 * Revision 1.2  2006/08/17 02:58:58  deph
 * updated load balancer - removing hard-coded nodes from API to xml
 *
 * Revision 1.1  2001/01/22 18:37:59  porter
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
 *
 **************************************************************************/
#ifndef STDBSERVERIMPL_HH
#define STDBSERVERIMPL_HH

#include "StDbServer.hh"
#include "MysqlDb.h"
#include "StDbBuffer.h"

#include <list>
#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StDataBaseI*,allocator<StDataBaseI*> > DbList;
#else
#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif
typedef list<StDataBaseI*> DbList;
#endif

class StDbServerImpl : public StDbServer {

protected:
  
  MysqlDb      Db;
  StDbBuffer   buff;

  StDataBaseI* mcurrentDb;
  DbList       mdataBases;
 
  void  deleteDataBases();


public:

  StDbServerImpl();
  StDbServerImpl(const char* name, const char* host, 
                 const char* socket, int port);
  StDbServerImpl(StDbServerImpl& server);
  virtual ~StDbServerImpl();

  virtual void         addDataBase(StDbType type, StDbDomain domain); 
  virtual void         addDataBase(const char* typeName, const char* domName);
  virtual StDataBaseI* useDb(StDbType type, StDbDomain domain);
  virtual StDataBaseI* useDb(const char* typeName, const char* domName);
  virtual StDataBaseI* useDb();
  virtual bool         isConnected();
  virtual void         closeConnection();
  virtual void         setTimeLogging(bool isTimeLogged);
  virtual double       getQueryTimes();
  virtual double       getSocketTimes();
  virtual double       getConnectTimes();

  void PointMysqlDb(StDbManagerImpl* m) {Db.my_manager = m;}

};

#endif
