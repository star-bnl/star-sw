/***************************************************************************
 *
 * $Id: StDbNode.hh,v 1.7 2016/05/25 20:17:51 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Base-class database entities
 *
 ***************************************************************************
 *
 * $Log: StDbNode.hh,v $
 * Revision 1.7  2016/05/25 20:17:51  dmitry
 * coverity - uninit ctor
 *
 * Revision 1.6  2001/02/08 23:23:56  porter
 * fixed initialization of schemaID in table & fixed some warnings when
 * compiled with NODEBUG
 *
 * Revision 1.5  2001/01/22 18:37:58  porter
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
 * Revision 1.4  2000/04/25 18:26:03  porter
 * added flavor & production time as settable query fields in
 * table &/or node. Associated SQL updated in mysqlAccessor.
 * Flavor key supports "+" as an OR symbol.
 *
 * Revision 1.3  2000/02/15 20:27:44  porter
 * Some updates to writing to the database(s) via an ensemble (should
 * not affect read methods & haven't in my tests.
 *  - closeAllConnections(node) & closeConnection(table) method to mgr.
 *  - 'NullEntry' version to write, with setStoreMode in table;
 *  -  updated both StDbTable's & StDbTableDescriptor's copy-constructor
 *
 * Revision 1.2  2000/01/19 20:20:06  porter
 * - finished transaction model needed by online
 * - fixed CC5 compile problem in StDbNodeInfo.cc
 * - replace TableIter class by StDbTableIter to prevent name problems
 *
 * Revision 1.1  2000/01/10 20:37:54  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 *
 ***************************************************************************/
#ifndef STDBNODE_HH
#define STDBNODE_HH

#include "StDbDefs.hh"
#include <string.h>
#ifdef __ROOT__
#include "TROOT.h"
#endif

class StDbNode {

protected:

  // unique node identfier
  char * mname;
  char * mversion;
  char * mdbName;
  StDbType mdbType = dbStDb;
  StDbDomain mdbDomain = dbDomainUnknown;

  // from db
  int   mnodeID;
  char* mnodeType;

  bool misConfigured;
  bool mcanRollBack;

public:

   StDbNode() : mname(0), mversion(0), mdbName(0), mnodeID(0), mnodeType(0), misConfigured(false), mcanRollBack(false) {};
   StDbNode(const char* name, const char* versionKey);
   StDbNode(const char* name);
   StDbNode(StDbNode& node);

   virtual ~StDbNode();

   char* getName() ;
   char* printName() ;
   char* getMyName() ;
   char* getVersion() ;
   char* printVersion() ;
   char* getDbName() ;
   char* printDbName();
   StDbType getDbType() const;
   StDbDomain getDbDomain() const;
   int getNodeID() const;
   char* getNodeType() ;
   char* printNodeType();
  
   void  setName(const char* nodeName);
   void  setVersion(const char* nodeVersion);
   void  setDbName(const char* nodeDbName);
   void  setDbType(StDbType type);
   void  setDbDomain(StDbDomain domain);
   void  setNodeID(int id);
   void  setNodeType(const char* nodeType);

  // write transactions
   bool  canRollBack() const;
   void  addWrittenNode(int dataID);
   void  commit();

  // string comparisons
   bool  checkName(const char* nodeName) const;
   bool  checkVersion(const char* nodeVersion) const;
   bool  checkNode(const char* nodeName, const char* nodeVersion) const;
   bool  IsConfigured() const;
   void  setConfigured(bool isConfigured);
   bool  isNode(StDbType type, StDbDomain domain);
   virtual bool IsTable() const;
 
  // helper functions 
   char* mstrDup(const char* s2) ;  // strdup isn't ANSI
   int*  decodeElementID(const char* elementID, int& numRows) ;
   char* getNextID(char*& currentElement) const;
#ifdef __ROOT__
  ClassDef(StDbNode,0)
#endif

};
inline char* StDbNode::printName()     { return mname; };
inline char* StDbNode::printDbName()   { return mdbName; };
inline char* StDbNode::printNodeType() { return mnodeType; };
inline char* StDbNode::printVersion()  { return mversion; };
inline char* StDbNode::getMyName()     { return printName(); }
inline StDbType   StDbNode::getDbType() const   { return mdbType;}
inline StDbDomain StDbNode::getDbDomain() const { return mdbDomain; }
inline void StDbNode::setDbType(StDbType type)       { mdbType=type;}
inline void StDbNode::setDbDomain(StDbDomain domain) { mdbDomain=domain; }
inline int  StDbNode::getNodeID() const  { return mnodeID; }
inline void StDbNode::setNodeID(int id ) {mnodeID = id; }
inline void StDbNode::setConfigured(bool isC){ misConfigured=isC; }
inline bool StDbNode::IsConfigured() const { return misConfigured; }
inline bool StDbNode::isNode(StDbType type, StDbDomain domain){
  return ( (type==mdbType) && (domain==mdbDomain) ) ? true : false;
}
inline bool StDbNode::canRollBack() const { return mcanRollBack; }
inline void StDbNode::addWrittenNode(int dataID){
 mcanRollBack=true;
 mnodeID=dataID;
}
inline void StDbNode::commit() { mcanRollBack = false; }
inline bool StDbNode::checkName(const char* nodeName) const {
return (mname && (strcmp(mname,nodeName)==0)) ? true : false;
}
inline bool StDbNode::checkVersion(const char* nodeVersion) const {
return (mversion && (strcmp(mversion,nodeVersion)==0)) ? true : false;
}
inline bool StDbNode::checkNode(const char* name, const char* version) const {
return (checkName(name) && checkVersion(version)) ? true : false;
}
inline bool StDbNode::IsTable() const { return false; }
#endif
