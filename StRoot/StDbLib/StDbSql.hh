/***************************************************************************
 *
 * $Id: StDbSql.hh,v 1.1 2001/01/22 18:37:59 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Implementation class of StDataBaseI in (My)SQL
 *
 ***************************************************************************
 *
 * $Log: StDbSql.hh,v $
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
#ifndef STDATABASE_HH
#define STDATABASE_HH

#include "StDbTableDescriptor.h"
#include "StDbManager.hh"
#include "StDataBaseI.hh"
#include "MysqlDb.h"
#include "StDbBuffer.h"

#include <list>
#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StDbTableDescriptor*,allocator<StDbTableDescriptor*> > DescList;
#else
#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif
typedef list<StDbTableDescriptor*> DescList;
#endif

#define MAX_EID_INDECES 10

class StDbSql : public StDataBaseI {

protected:

  StDbManager* mgr;

  // A found descriptor list -> can be faster than asking DB if already found 
  DescList mdescriptors;

  char* mretString; // memory holder for internal string passing
  int   mtableCatalog; // 0==hasn't checked, 1==doesn't have it, 2==has it
  char* mdefaultEndDateTime;
  unsigned int mdefaultEndTime;

  // descriptor tracking methods
  StDbTableDescriptor* findDescriptor(int structID, int schemaID);
  StDbTableDescriptor* getDescriptor(int structID, int schemaID);
  void                 addDescriptor(StDbTableDescriptor* td);  
  void                 deleteDescriptors();
  void                 deleteDescriptor(int structID, int schemaID);

  int   prepareNode(StDbNode* node);
  int   queryNode(StDbNode* node);
  bool  readNodeInfo(StDbNode* node);
  bool  readConfigNodeInfo(StDbConfigNode* node);
  bool  readTableInfo(StDbTable* table);
  bool  checkValue(const char* colName, const char* colValue);
  bool  checkForNull(const char* src);
  char* insertNodeString(StDbNode* node);
  char* getFlavorQuery(const char* flavor);
  char* getProdTimeQuery(unsigned int prodTime);
  char* getElementList(int* elements, int num);
  char* getColumnList(StDbTable* table,char* funcName=0);
  bool  hasInstance(StDbTable* table);
  void  checkTableCatalog();
  char* checkTablePrepForQuery(StDbTable* table, bool checkIndexed=false);

  bool  writeOldIndex(int nodeID, int schemaID, const char* sTime, 
                      int elementID,const char* flavor, int dataID);
  void  deleteOldIndex(int* dataIDs, int numRows, int nodeID);
  void  deleteRows(const char* tableName, int* rowID, int nrows);
  void  initEndTime();

public:

  StDbSql(MysqlDb &db, StDbBuffer& buffer);
  StDbSql(MysqlDb &db, StDbBuffer& buffer, 
          StDbType type, StDbDomain domain);
  StDbSql(MysqlDb &db, StDbBuffer& buffer, 
          const char* typeName, const char* domainName);

  virtual ~StDbSql();

  virtual void  use();
  virtual void  close();
  virtual void  clear();  

  virtual int   QueryDb(StDbConfigNode* node);
  virtual int   QueryDb(StDbNode* node);
  virtual int   QueryDb(StDbTable* table, unsigned int reqTime);
  virtual int   QueryDb(StDbTable* table, const char* whereClause);
  virtual unsigned int* QueryDbTimes(StDbTable* table, const char* whereClause);
  virtual int   QueryDbFunction(StDbTable* table, const char* whereClause, char* funcName);
  virtual int   QueryDescriptor(StDbTable* table);
  virtual int   WriteDb(StDbTable* table, unsigned int storeTime);
  virtual int   WriteDb(StDbConfigNode* node, int parentID, int& configID);

  virtual int   storeConfigNode(StDbConfigNode* node);
  virtual int   storeTableNode(StDbTable* table);
  virtual bool  insertNodeRelation(int configID, int parent, int child);
  virtual bool  rollBack(StDbNode* node);
  virtual bool  rollBack(StDbTable* table);  

  virtual unsigned int getUnixTime(const char* time)  ;
  virtual char*        getDateTime(unsigned int time) ;

  virtual  int*        selectElements(const char* elementName, 
                                      StDbElementIndex* inval, 
                                      int& numElements);

        char*  getDataTable(StDbTable* table, unsigned int time);
        char** getDataTables(StDbTable* table,int& numTables);
        void   setDbUtils(MysqlDb& db, StDbBuffer& buffer);
        void   setDefaultReturnValues(StDbTable* table, unsigned int reqTime);
        void   setDefaultBeginTime(StDbTable* table,unsigned int reqTime);
        void   setDefaultEndTime(StDbTable* table );
        char** getIndexNames( const char* elementName, int& numIndexes);

MysqlDb& Db;
StDbBuffer& buff;

};

inline void StDbSql::use()   { Db.setDefaultDb(mdbName); };
inline void StDbSql::close() { Db.Close(); };
inline void StDbSql::clear() { Db.Release(); buff.Raz(); };

#endif
