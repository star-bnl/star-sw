/***************************************************************************
 *
 * $Id: StDbServerImpl.cc,v 1.4 2016/05/24 20:26:48 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Server class for DB-access
 *
 ***************************************************************************
 *
 * $Log: StDbServerImpl.cc,v $
 * Revision 1.4  2016/05/24 20:26:48  dmitry
 * coverity - unreachable delete loop suppression
 *
 * Revision 1.3  2002/01/30 15:40:48  porter
 * changed limits on flavor tag & made defaults retrieving more readable
 *
 * Revision 1.2  2001/02/08 23:23:56  porter
 * fixed initialization of schemaID in table & fixed some warnings when
 * compiled with NODEBUG
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
 **************************************************************************/
#include "StDbServerImpl.hh"
#include "StDbSql.hh"
#define __CLASS__ "StDbServerImpl"

////////////////////////////////////////////////////////////////

StDbServerImpl::StDbServerImpl(): StDbServer(), mcurrentDb(0){}

StDbServerImpl::StDbServerImpl(const char* name, const char* host, const char* sock, int port): StDbServer(name,host,sock,port), mcurrentDb(0) {};

StDbServerImpl::StDbServerImpl(StDbServerImpl& server) :  StDbServer(server), mcurrentDb(0){}

StDbServerImpl::~StDbServerImpl() {   deleteDataBases(); }

////////////////////////////////////////////////////

void
StDbServerImpl::addDataBase(StDbType type, StDbDomain domain) {

  StDataBaseI* db=useDb(type,domain);
  if(!db){
    mcurrentDb = new StDbSql(Db,buff,type,domain);
    if(!Db.IsConnected())Db.Connect(mhostName,muserName,mpword,mcurrentDb->printDbName(),mportNumber);
    mcurrentDb->use();
    mdataBases.push_back(mcurrentDb);
  }
}
////////////////////////////////////////////////////

void
StDbServerImpl::addDataBase(const char* type, const char* domain) {

  if(!useDb(type,domain)){
    mcurrentDb = new StDbSql(Db,buff,type,domain);
    if(!Db.IsConnected())Db.Connect(mhostName,muserName,mpword,mcurrentDb->printDbName(),mportNumber);
    mcurrentDb->use();
    mdataBases.push_back(mcurrentDb);
  }
}
////////////////////////////////////////////////////////////////

StDataBaseI*
StDbServerImpl::useDb(StDbType type, StDbDomain domain) {

if(mcurrentDb && 
   mcurrentDb->getDbType()==type && 
   mcurrentDb->getDbDomain()==domain) return mcurrentDb;

  StDataBaseI* db=0; // return null if not found
  for(DbList::iterator itr = mdataBases.begin();
      itr != mdataBases.end(); ++itr){
    if(type==(*itr)->getDbType() && domain==(*itr)->getDbDomain()){  
       mcurrentDb=*itr;
       mcurrentDb->use();
       db=mcurrentDb;
       break;
    }
  }
  return db;
}
////////////////////////////////////////////////////////////////

StDataBaseI*
StDbServerImpl::useDb(const char* type, const char* domain) {

if( mcurrentDb && 
    mcurrentDb->checkDbType(type) && 
    mcurrentDb->checkDbDomain(domain)) return mcurrentDb;

  StDataBaseI* db=0;
  for(DbList::iterator itr = mdataBases.begin();
      itr != mdataBases.end(); ++itr){
    if((*itr)->checkDbType(type) && (*itr)->checkDbDomain(domain)){  
       mcurrentDb=*itr;
       mcurrentDb->use();
       db=mcurrentDb;
       break;
    }
  }
  return db;
}

//////////////////////////////////////////////////////
StDataBaseI* 
StDbServerImpl::useDb(){  

  if(!mcurrentDb){
    mcurrentDb= new StDbSql(Db,buff,dbServer,dbStar);
    mcurrentDb->use();
    mdataBases.push_back(mcurrentDb);
  }    

 if(Db.IsConnected() || 
    Db.Connect(mhostName,muserName,mpword,mcurrentDb->printDbName(),mportNumber)) return mcurrentDb; 

 return (StDataBaseI*) NULL;
}
/////////////////////////////////////////////////////////////////////

void
StDbServerImpl::deleteDataBases() {

  for( auto &it : mdataBases ) delete it;
  mdataBases.clear();

}
/////////////////////////////////////////////////////////////////////

bool StDbServerImpl::isConnected()    {  return Db.IsConnected(); }
void StDbServerImpl::closeConnection(){  Db.Close();  }

void   StDbServerImpl::setTimeLogging(bool isTLog){ Db.mlogTime=isTLog; }
double StDbServerImpl::getQueryTimes() {return Db.mqueryLog.getTotalTimes();};
double StDbServerImpl::getSocketTimes(){return Db.msocketLog.getTotalTimes();};
double StDbServerImpl::getConnectTimes(){return Db.mconnectLog.getTotalTimes();};

#undef __CLASS__
