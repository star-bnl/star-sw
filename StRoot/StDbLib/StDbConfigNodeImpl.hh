/***************************************************************************
 *
 * $Id: StDbConfigNodeImpl.hh,v 1.2 2011/11/28 17:03:08 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Node (directory) to hold list of dbtables
 *
 ***************************************************************************
 *
 * $Log: StDbConfigNodeImpl.hh,v $
 * Revision 1.2  2011/11/28 17:03:08  dmitry
 * dbv override support in StDbLib,StDbBroker,St_db_Maker
 *
 * Revision 1.1  2001/01/22 18:37:52  porter
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
#ifndef STDBCONFIGNODEIMPL_HH
#define STDBCONFIGNODEIMPL_HH

#include "StDbConfigNode.hh"
#include "StDbElementIndex.hh"

#include <list>
#ifdef ST_NO_TEMPLATE_DEF_ARGS
 typedef list<StDbTable*, allocator<StDbTable*> > TableList;
#else
 #if !defined(ST_NO_NAMESPACES)
    using std::list;
 #endif
 typedef list<StDbTable*> TableList;
#endif

class StDbConfigNodeImpl : public StDbConfigNode {

 friend class StDbTableIterImpl;

protected:
  
  StDbElementIndex*  melementIndex;
  int                mindexRef;    
  TableList          mTables;

  void deleteTables();
  void updateDbInfo();
  void updateDbTables(int opt=0);
  void updateDbTable(StDbTable* table,int opt=0);

public:

  StDbConfigNodeImpl( StDbConfigNode* parent, 
                      const char* nodeName, const char* configName);
  StDbConfigNodeImpl( StDbConfigNode* parent, StDbNode& node); 
  StDbConfigNodeImpl( StDbType type,  StDbDomain domain,  
                      const char* nodeName, const char* configName="none");
  virtual ~StDbConfigNodeImpl(); 

  virtual void addChildren(dbEnvList* elist);

  // DB & Table Index operations 
   virtual void   resetConfig(const char* config, int opt=0); 
   virtual int    buildTree(int opt=0);//0=get tableDescriptors from db
   virtual StDbElementIndex* getElementIndex();
   virtual void   setElementIndexInfo(const char* indexName, int indexID);
   virtual void   getElementIndexInfo(char*& indexname, int& indexID);
   virtual int    getNumIndeces() const;

  // Table operations 
   virtual StDbTable*     addDbTable(const char* tableName, 
                                     const char* version="default");
   virtual StDbTable*     addTable(const char* tableName, 
                                   const char* version="default");
   virtual StDbTable*     addTable(StDbNode* node);
   virtual StDbTable*     findTable(const char* name, const char* subPath="/");
   virtual StDbTable*     findLocalTable(const char* name);
   virtual void           removeTable(StDbTable* table);

   virtual StDbTableIter* getStDbTableIter();
   virtual bool           compareTables(StDbTable* tab1, StDbTable* tab2);
   virtual void           printTables(int depth);
   virtual void           printNumberStats();

   virtual void getNumberStats(unsigned int& nNodes, 
                               unsigned int& ntables, 
                               unsigned int& numBytes);

  // set the table flavors in full sub-tree or local list
   virtual void  setTablesFlavor(const char* flavor);
   virtual void  setTablesProdTime(unsigned int ptime);
   virtual void  setTablesProdTimeOverride(unsigned int ptime, char* dbType = 0, char* dbDomain = 0); // DBV override
}; 

#endif





