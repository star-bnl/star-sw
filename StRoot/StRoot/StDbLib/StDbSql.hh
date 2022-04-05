/***************************************************************************
 *
 * $Id: StDbSql.hh,v 1.11 2016/05/25 20:17:51 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Implementation class of StDataBaseI in (My)SQL
 *
 ***************************************************************************
 *
 * $Log: StDbSql.hh,v $
 * Revision 1.11  2016/05/25 20:17:51  dmitry
 * coverity - uninit ctor
 *
 * Revision 1.10  2005/11/07 14:46:44  deph
 * added protoype for function that accepts IN for non contiguous elementIDs
 *
 * Revision 1.9  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.8  2003/09/16 22:44:17  porter
 * got rid of all ostrstream objects; replaced with StString+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.7  2003/04/11 22:47:36  porter
 * Added a fast multi-row write model specifically needed by the daqEventTag
 * writer. Speed increased from about 100Hz to ~3000Hz.  It is only invoked if
 * the table is marked as Non-Indexed (daqTags & scalers). For non-indexed tables
 * which include binary stored data (we don't have any yet), the fast writer  has
 * to invoke a slower buffer so that the rates are a bit slower (~500Hz at 50 rows/insert).
 *
 * Revision 1.6  2003/01/10 04:19:20  porter
 * added feature of getting timestamp list (but no data) for a table.
 * fixed 2 features sometimes used in online in query-by-whereclause.
 * removed a stray 'cout' in a routine that is rarely accessed
 *
 * Revision 1.5  2002/01/30 15:40:48  porter
 * changed limits on flavor tag & made defaults retrieving more readable
 *
 * Revision 1.4  2001/10/24 04:05:20  porter
 * added long long type to I/O and got rid of obsolete dataIndex table
 *
 * Revision 1.3  2001/04/25 17:19:53  perev
 * HPcorrs
 *
 * Revision 1.2  2001/03/30 18:48:26  porter
 * modified code to keep Insure from wigging-out on ostrstream functions.
 * moved some messaging into a StDbSql method.
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

#ifdef HPUX
#define freeze(i) str()
#endif

typedef list<StDbTableDescriptor*> DescList;
#endif

#define MAX_EID_INDECES 10

class StDbSql : public StDataBaseI {

protected:

  StDbManager* mgr = 0;

  // A found descriptor list -> can be faster than asking DB if already found 
  DescList mdescriptors;

  char* mretString = 0; // memory holder for internal string passing
  int   mtableCatalog = 0; // 0==hasn't checked, 1==doesn't have it, 2==has it
  char* mdefaultEndDateTime = 0;
  unsigned int mdefaultEndTime = 0;

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

  // use local 'mretString' for building and tracking common query entities
  char* insertNodeString(StDbNode* node);
  char* getFlavorQuery(const char* flavor);
  char* getProdTimeQuery(unsigned int prodTime);
  char* getElementList(int* elements, int num);
  char* getElementListIN(int* elements, int num);
  char* getColumnList(StDbTable* table,char* tableName=0, char* funcName=0);
  char* getEmptyString();

  bool  hasInstance(StDbTable* table);
  void  checkTableCatalog();
  char* checkTablePrepForQuery(StDbTable* table, bool checkIndexed=false);

  void  deleteRows(const char* tableName, int* rowID, int nrows);
  void  initEndTime();

  char* mRetString(StString& rs);
  int   sendMess(const char* a, const char* b, StDbMessLevel m, 
                 int lineNum=0, const char* className=" ",
                 const char* methName=" ");

  /* new */
  bool  checkColumn(const char* tableName, const char* columnName);
  bool  updateEndTime(StDbTable* table, const char* dataTable, unsigned int reqTime);
  void  init();

  // specific meth for fast multi-row writes of non-indexed tables
  virtual int   WriteDbNoIndex(StDbTable* table, unsigned int storeTime);


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
  virtual unsigned int* QueryDbTimes(StDbTable* table, const char* whereClause,int opt=0);
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

inline char* StDbSql::mRetString(StString& rs){
   if(mretString)delete [] mretString;
   string srs=rs.str();
   mretString = new char[srs.length()+1];
   strcpy(mretString,srs.c_str());
   return mretString;
}

inline int StDbSql::sendMess(const char* a, const char* b, StDbMessLevel m, int lineNum, const char* className, const char* methName){
  if(m==dbMDebug && !(mgr->IsVerbose()))return 0;
  return mgr->printInfo(a,b,m,lineNum,className,methName);
}

inline void StDbSql::init() { 
    mretString = 0;
    mtableCatalog=0;
    mdefaultEndDateTime = 0;
}

inline void StDbSql::use()   { Db.setDefaultDb(mdbName); };
inline void StDbSql::close() { Db.Close(); };
inline void StDbSql::clear() { Db.Release(); buff.Raz(); };

#endif





