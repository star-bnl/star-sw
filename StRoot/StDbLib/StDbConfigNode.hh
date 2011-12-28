/***************************************************************************
 *
 * $Id: StDbConfigNode.hh,v 1.17 2011/11/28 17:03:08 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Node (directory) & db handle to hold list of dbtables
 *               Now (Dec2000) pure-virtual for hiding db & table (stl) part
 *              
 ***************************************************************************
 *
 * $Log: StDbConfigNode.hh,v $
 * Revision 1.17  2011/11/28 17:03:08  dmitry
 * dbv override support in StDbLib,StDbBroker,St_db_Maker
 *
 * Revision 1.16  2001/01/22 18:37:52  porter
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
 * Revision 1.15  2000/04/25 18:26:02  porter
 * added flavor & production time as settable query fields in
 * table &/or node. Associated SQL updated in mysqlAccessor.
 * Flavor key supports "+" as an OR symbol.
 *
 * Revision 1.14  2000/03/28 17:03:18  porter
 * Several upgrades:
 * 1. configuration by timestamp for Conditions
 * 2. query by whereClause made more systematic
 * 3. conflict between db-stored comments & number lists resolved
 * 4. ensure endtime is correct for certain query falures
 * 5. dbstl.h->handles ObjectSpace & RogueWave difference (Online vs Offline)
 *
 * Revision 1.13  2000/01/27 05:54:33  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
 * Revision 1.12  2000/01/19 20:20:05  porter
 * - finished transaction model needed by online
 * - fixed CC5 compile problem in StDbNodeInfo.cc
 * - replace TableIter class by StDbTableIter to prevent name problems
 *
 * Revision 1.11  2000/01/14 14:50:52  porter
 * expanded use of verbose mode & fixed inconsistency in
 * StDbNodeInfo::getElementID
 *
 * Revision 1.10  2000/01/10 20:37:53  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.9  1999/12/28 21:31:41  porter
 * added 'using std::vector' and 'using std::list' for Solaris CC5 compilation.
 * Also fixed some warnings arising from the CC5 compiles
 *
 * Revision 1.8  1999/12/03 22:24:01  porter
 * expanded functionality used by online, fixed bug in
 * mysqlAccessor::getElementID(char*), & update StDbDataSet to
 * conform to changes in Xml reader & writer
 *
 * Revision 1.7  1999/10/19 14:30:38  porter
 * modifications relevant to use with StDbBroker and future merging with
 * "params" database structure + some docs + suppressing diagnostics messages
 *
 * Revision 1.6  1999/09/30 02:06:03  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBCONFIGNODE_HH
#define STDBCONFIGNODE_HH

#include "StDbNode.hh"
#include "StDbTableIter.hh"

class dbEnvList;
class StDbElementIndex;
class StDbTable;

#ifdef __ROOT_
#include "TROOT.h"
#endif

class StDbConfigNode : public StDbNode {

protected:

 StDbConfigNode* mfirstChildNode;
 StDbConfigNode* mnextNode;
 StDbConfigNode* mparentNode;
  
 bool            mhasData;
 int             mbranchID;
 bool            misDbNode;
 void            zeroNodes();

public:

   StDbConfigNode( StDbConfigNode* parent, 
                   const char* nodeName, const char* configName);
   StDbConfigNode( StDbConfigNode* parent, StDbNode& node); 
   StDbConfigNode( StDbType type,  StDbDomain domain,  
                   const char* nodeName, const char* configName="none");
   virtual ~StDbConfigNode(); 
  
  // node & tree operations
   void            setNextNode(StDbConfigNode* node); 
   void            setParentNode(StDbConfigNode* node); 
   void            setFirstChildNode(StDbConfigNode* node); 
   virtual void    setChildNode(StDbConfigNode* node); 
   void            appendNode(StDbConfigNode* node); 
   StDbConfigNode* getNextNode();
   StDbConfigNode* getParentNode(); 
   StDbConfigNode* getFirstChildNode(); 
   void            deleteTree();
   virtual void    addChildren(dbEnvList* elist) = 0;
   void            deleteChildren();

  // check container status
   bool   hasChildren();
   bool   hasData();
   int    getBranchID();
   void   setBranchID(int branchID);
   void   setIsDbNode(bool isDbNode);
   bool   isDbNode();
   void   printTree(int depth);

  // DB & Table Index operations
   virtual void  resetConfig(const char* config, int opt=0)                 =0;
   virtual int   buildTree(int opt=0)                                       =0;
   virtual StDbElementIndex*  getElementIndex()                             =0;
   virtual void  setElementIndexInfo(const char* indexName, int indexID)    =0;
   virtual void  getElementIndexInfo(char*& indexname, int& indexID)        =0;
   virtual int   getNumIndeces() const                                      =0;

  // Table operations --> pure virtual for db & stl dependencies 
   virtual StDbTable*     addDbTable(const char* tableName, 
                                     const char* version="default")         =0;
   virtual StDbTable*     addTable  (const char* tableName, 
                                     const char* version="default")         =0;
   virtual StDbTable*     addTable(StDbNode* node)                          =0;
   virtual StDbTable*     findTable(const char* name, const char* sPath="/")=0;
   virtual StDbTable*     findLocalTable(const char* name)                  =0;
   virtual void           removeTable(StDbTable* table)                     =0;
   virtual StDbTableIter* getStDbTableIter()                                =0;
   virtual bool           compareTables(StDbTable* tab1, StDbTable* tab2)   =0;
   virtual void           printTables(int depth)                            =0;
   virtual void           printNumberStats()                                =0;
   virtual void getNumberStats(unsigned int& nNodes, 
                               unsigned int& ntables, 
                               unsigned int& numBytes)                      =0;

  // set the table flavors in full sub-tree or local list
   virtual void           setTablesFlavor(const char* flavor)               =0;
   virtual void           setTablesProdTime(unsigned int ptime)             =0;
   virtual void           setTablesProdTimeOverride(unsigned int ptime, char* dbType = 0, char* dbDomain = 0)             =0;
   void  setFlavor(const char* flavor);
   void  setProdTime(unsigned int ptime);
   void  setProdTimeOverride(unsigned int ptime, char* dbType = 0, char* dbDomain = 0);

  // More node operations
   StDbConfigNode* findConfigNode(StDbType t, StDbDomain d, const char* sPath);
   StDbConfigNode* findConfigNode(StDbType t, StDbDomain d);
   StDbConfigNode* findConfigNode(const char* sPath);
   StDbConfigNode* findChildConfigNode(const char* nodeName);
#ifdef __ROOT__
  ClassDef(StDbConfigNode,0)
#endif

}; 

inline void StDbConfigNode::setNextNode(StDbConfigNode* node){ mnextNode=node;}
inline StDbConfigNode* StDbConfigNode::getNextNode()   { return mnextNode; };
inline StDbConfigNode* StDbConfigNode::getParentNode() { return mparentNode; };
inline StDbConfigNode* StDbConfigNode::getFirstChildNode(){ return mfirstChildNode; };
inline bool StDbConfigNode::hasData(){ return mhasData;};
inline int  StDbConfigNode::getBranchID() { return mbranchID; };
inline void StDbConfigNode::setBranchID(int branchID) { mbranchID=branchID; };
inline void StDbConfigNode::setIsDbNode(bool isDbNode){misDbNode=isDbNode; };
inline bool StDbConfigNode::isDbNode() { return misDbNode; };
inline bool StDbConfigNode::hasChildren(){return (mfirstChildNode) ? true : false; };

#endif





