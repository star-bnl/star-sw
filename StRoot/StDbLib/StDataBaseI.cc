/***************************************************************************
 *
 * $Id: StDataBaseI.cc,v 1.2 2001/10/26 20:59:45 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Abstract class for Star specific SQL queries
 *
 ***************************************************************************
 *
 * $Log: StDataBaseI.cc,v $
 * Revision 1.2  2001/10/26 20:59:45  porter
 * fixed new endtime flag from previous checkin. made StDataBaseI available
 * at root-cli.
 *
 * Revision 1.1  2001/01/22 18:37:50  porter
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
#include "StDataBaseI.hh"
#include "StDbManager.hh"
#include <string.h>

#ifdef __ROOT__
ClassImp(StDataBaseI)
#endif
//////////////////////////////////////////////////////////////////

StDataBaseI::StDataBaseI() : mdbType(dbStDb), mdbDomain(dbDomainUnknown), mdbName(0), mtypeName(0), mdomainName(0), mdbStore(dbV00) { };

StDataBaseI::StDataBaseI(StDbType type, StDbDomain domain) : mdbName(0), mtypeName(0), mdomainName(0), mdbStore(dbV00) { setDataBase(type,domain); }

StDataBaseI::StDataBaseI(const char* typeName, const char* domainName) : mdbName(0), mtypeName(0), mdomainName(0), mdbStore(dbV00) { setDataBase(typeName,domainName); }

//////////////////////////////////////////////////////////////////
StDataBaseI::~StDataBaseI(){
  if(mdbName)     delete [] mdbName;
  if(mtypeName)   delete [] mtypeName;
  if(mdomainName) delete [] mdomainName;
}

//////////////////////////////////////////////////////////////////
void
StDataBaseI::setDataBase(StDbType type, StDbDomain domain){
   setDbType(type);
   setDbDomain(domain);
   char* name= StDbManager::Instance()->getDbTypeName(type);
   setTypeName(name); if(name) delete [] name;
   name= StDbManager::Instance()->getDbDomainName(domain);
   setDomainName(name); if(name) delete [] name;
   name=StDbManager::Instance()->getDbName(mtypeName,mdomainName);
   setDbName(name); if(name) delete [] name;
}

//////////////////////////////////////////////////////////////////
void
StDataBaseI::setDataBase(const char* typeName, const char* domainName){
   setTypeName(typeName);
   setDomainName(domainName);
   char* name = StDbManager::Instance()->getDbName(mtypeName,mdomainName);
   setDbName(name); if(name) delete [] name;
   setDbType(StDbManager::Instance()->getDbType(typeName));
   setDbDomain(StDbManager::Instance()->getDbDomain(domainName));
}

//////////////////////////////////////////////////////////////////
void
StDataBaseI::setDataBase(const char* dbName){
   setDbName(dbName);
   StDbManager::Instance()->getDataBaseInfo(dbName,mtypeName,mdomainName);
   setDbType(StDbManager::Instance()->getDbType(mtypeName));
   setDbDomain(StDbManager::Instance()->getDbDomain(mdomainName));
}

//////////////////////////////////////////////////////////////////
void
StDataBaseI::setDbName(const char* dbName) {

  if(dbName){
    if(mdbName) delete [] mdbName;
    mdbName = new char[strlen(dbName)+1];
    strcpy(mdbName,dbName);
  }
}

//////////////////////////////////////////////////////////////////
void
StDataBaseI::setTypeName(const char* typeName) {

  if(typeName){
    if(mtypeName) delete [] mtypeName;
    mtypeName = new char[strlen(typeName)+1];
    strcpy(mtypeName,typeName);
  }
}

//////////////////////////////////////////////////////////////////
void
StDataBaseI::setDomainName(const char* domainName) {

  if(domainName){
    if(mdomainName) delete [] mdomainName;
    mdomainName = new char[strlen(domainName)+1];
    strcpy(mdomainName,domainName);
  }
}

//////////////////////////////////////////////////////////////////
char*
StDataBaseI::getDbName() const {

 char* retVal=0;
 if(mdbName) {
    retVal = new char[strlen(mdbName)+1];
    strcpy(retVal,mdbName);
 }
return retVal;
}

//////////////////////////////////////////////////////////////////
char*
StDataBaseI::getTypeName() const {

 char* retVal=0;
 if(mtypeName) {
    retVal = new char[strlen(mtypeName)+1];
    strcpy(retVal,mtypeName);
 }
return retVal;
}

//////////////////////////////////////////////////////////////////
char*
StDataBaseI::getDomainName() const {

 char* retVal=0;
 if(mdomainName) {
    retVal = new char[strlen(mdomainName)+1];
    strcpy(retVal,mdomainName);
 }
return retVal;
}

//////////////////////////////////////////////////////////////////

char* StDataBaseI::printDbName() { return mdbName; }
char* StDataBaseI::printTypeName() { return mtypeName; }
char* StDataBaseI::printDomainName() { return mdomainName; }
bool StDataBaseI::checkDbName(const char* name) { 
  return (strcmp(name,mdbName)==0) ? true : false;
}
bool StDataBaseI::checkDbType(const char* name) { 
  return (strcmp(name,mtypeName)==0) ? true : false;
}
bool StDataBaseI::checkDbDomain(const char* name) {
  return (strcmp(name,mdomainName)==0) ? true : false;
}
