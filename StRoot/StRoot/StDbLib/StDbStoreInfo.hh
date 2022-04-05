/***************************************************************************
 *
 * $Id: StDbStoreInfo.hh,v 1.1 2001/01/22 18:38:00 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Simple integer array implementation of the database
 *              dataID fields. These are held during writes so that a
 *              many-row table or a multi-table write can be rolled back
 *              completely on failure at some point in the writing
 *
 ***************************************************************************
 *
 * $Log: StDbStoreInfo.hh,v $
 * Revision 1.1  2001/01/22 18:38:00  porter
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
#ifndef STDBSTOREINFO_HH
#define STDBSTOREINFO_HH

class StDbStoreInfo {

protected:

  int * mdataIDs;
  int   mnumRows;
  int   mMaxRows;
  // canRollBack logic:: true during writes, false after writes or with reads 
  bool  mcanRollBack; 

  void  reSizeInternalStore();

public:

  StDbStoreInfo();
  ~StDbStoreInfo();

  int* getDataIDs(int& numRows);
  void addWrittenRow(int dataID);

  void commit();
  bool canRollBack();  
  bool hasData();

  // delete information
  void resetStoreInfo();

};

inline void StDbStoreInfo::commit(){  mcanRollBack=false; }
inline bool StDbStoreInfo::canRollBack(){ return mcanRollBack; }
inline bool StDbStoreInfo::hasData() { return (bool) mnumRows; };
#endif


