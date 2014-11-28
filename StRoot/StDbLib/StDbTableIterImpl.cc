/***************************************************************************
 *
 * $Id: StDbTableIterImpl.cc,v 1.1 2001/01/22 18:38:01 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Implementation of Iterator of table-list in a 
 *               given StDbConfigNode class
 *
 ***************************************************************************
 *
 * $Log: StDbTableIterImpl.cc,v $
 * Revision 1.1  2001/01/22 18:38:01  porter
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

#include "StDbTableIterImpl.hh"

////////////////////////////////////////////////////////

void StDbTableIterImpl::init(StDbConfigNodeImpl* node){ 
   mnode=node; 
   itr = mnode->mTables.begin();
}

////////////////////////////////////////////////////////

StDbTable* StDbTableIterImpl::next(){
 StDbTable* ret = 0;
 if(done()) return ret;
 ret = *itr; itr++;
return ret;
}

////////////////////////////////////////////////////////

StDbTable* StDbTableIterImpl::operator++(){ return next(); }

////////////////////////////////////////////////////////

bool StDbTableIterImpl::done(){return (itr != mnode->mTables.end()) ? false : true ; }










