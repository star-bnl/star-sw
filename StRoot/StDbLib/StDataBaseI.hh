/***************************************************************************
 *
 * $Id: StDataBaseI.hh,v 1.3 2003/01/10 04:19:19 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Abstract class for Star specific SQL queries
 *
 ***************************************************************************
 *
 * $Log: StDataBaseI.hh,v $
 * Revision 1.3  2003/01/10 04:19:19  porter
 * added feature of getting timestamp list (but no data) for a table.
 * fixed 2 features sometimes used in online in query-by-whereclause.
 * removed a stray 'cout' in a routine that is rarely accessed
 *
 * Revision 1.2  2001/10/26 20:59:46  porter
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
#ifndef STDATABASEI_HH
#define STDATABASEI_HH

class StDbNode;
class StDbTable;
class StDbConfigNode;
class StDbElementIndex;

#include "StDbDefs.hh"

#ifdef __ROOT__
#include "TROOT.h"
#endif

class StDataBaseI {

protected:
  // enumerated database specification
  StDbType mdbType;
  StDbDomain mdbDomain;

  //
  // dbName = "typeName_domainName" or "typeName" if domainName=Star 
  // typeName => mapped from dbType
  // domainName => mapped from dbDomain
  //

  char* mdbName;
  char* mtypeName;
  char* mdomainName;

  StDbStoreType mdbStore; // for future evolution of changes in internal 
                          // storage structure & different SQL
public:

  StDataBaseI();
  StDataBaseI(StDbType type, StDbDomain domain);
  StDataBaseI(const char* typeName, const char* domainName);

  virtual ~StDataBaseI();

  virtual void setDataBase(StDbType type, StDbDomain domain); 
  virtual void setDataBase(const char* typeName, const char* domainName);
  virtual void setDataBase(const char* dbName);

  virtual void       setDbType(StDbType type);
  virtual void       setDbDomain(StDbDomain domain);
  virtual StDbType   getDbType()   const ;
  virtual StDbDomain getDbDomain() const ;

  virtual void       setDbName(const char* dbName);
  virtual void       setTypeName(const char* typeName);
  virtual void       setDomainName(const char* domainName);
  virtual char*      getDbName()     const ;
  virtual char*      getTypeName()   const ;
  virtual char*      getDomainName() const ;
  virtual char*      printDbName();
  virtual char*      printTypeName();
  virtual char*      printDomainName();
  virtual bool       checkDbName(const char* name);
  virtual bool       checkDbType(const char* type);
  virtual bool       checkDbDomain(const char* domain);

  virtual StDbStoreType getDbStoreType() const;
  virtual void setDbStoreType(StDbStoreType type);

  // DB-Implementation Specific methods
  virtual void use()                                                      = 0; 
  virtual void close()                                                    = 0;
  virtual int  QueryDb(StDbConfigNode* node)                              = 0;
  virtual int  QueryDb(StDbNode* node)                                    = 0;
  virtual int  QueryDb(StDbTable* table, unsigned int reqTime)            = 0;
  virtual int  QueryDb(StDbTable* table, const char* whereClause)         = 0;
  virtual unsigned int* QueryDbTimes(StDbTable* table, 
                                     const char* whereClause, int opt=0)  = 0;
  virtual int  QueryDbFunction(StDbTable* table, 
                              const char* whereClause, char* funcName)    = 0;
  virtual int  QueryDescriptor(StDbTable* table)                          = 0;
  virtual int  WriteDb(StDbTable* table, unsigned int storeTime)          = 0;
  virtual int  WriteDb(StDbConfigNode* node, int parentID, int& configID) = 0;

  virtual int  storeConfigNode(StDbConfigNode* node)                      = 0;
  virtual int  storeTableNode(StDbTable* table)                           = 0;
  virtual bool insertNodeRelation(int configID, int parent, int child)    = 0;
  virtual bool rollBack(StDbNode* node)                                   = 0;
  virtual bool rollBack(StDbTable* table)                                 = 0;

  virtual unsigned int getUnixTime(const char* time)                      = 0;
  virtual char*        getDateTime(unsigned int time)                     = 0;
  virtual int*         selectElements(const char* elementName, 
                                      StDbElementIndex* inval, 
                                      int& numElements)                   = 0;
#ifdef __ROOT__
  ClassDef(StDataBaseI,0)
#endif

};

inline void StDataBaseI::setDbType(StDbType type)       {mdbType = type; };
inline void StDataBaseI::setDbDomain(StDbDomain domain) {mdbDomain = domain; }
inline StDbType StDataBaseI::getDbType() const     { return mdbType; };
inline StDbDomain StDataBaseI::getDbDomain() const { return mdbDomain; };
inline StDbStoreType StDataBaseI::getDbStoreType() const    {return mdbStore;};
inline void StDataBaseI::setDbStoreType(StDbStoreType type) {mdbStore=type; };

#endif
