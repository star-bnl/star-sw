/***************************************************************************
 *
 * $Id: StDbTableIterImpl.hh,v 1.1 2001/01/22 18:38:02 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Iterates of table-list in a given StDbConfigNode class
 *
 ***************************************************************************
 * $Log: StDbTableIterImpl.hh,v $
 * Revision 1.1  2001/01/22 18:38:02  porter
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
#ifndef TABLEITRIMPL_HH
#define TABLEITRIMPL_HH

#include "StDbConfigNodeImpl.hh"
typedef TableList::iterator ListIter;

class StDbTableIterImpl : public StDbTableIter {

  ListIter itr;
  StDbConfigNodeImpl* mnode;

public:

  StDbTableIterImpl() : mnode(0){};
  StDbTableIterImpl(StDbConfigNodeImpl* node){ init(node);};
  virtual ~StDbTableIterImpl(){};

  void init(StDbConfigNodeImpl* node);

  virtual StDbTable* next();
  virtual StDbTable* operator++();
  virtual bool done();

};

#endif






