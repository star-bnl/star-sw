/***************************************************************************
 *
 * $Id: StDbElementIndex.hh,v 1.1 2001/01/22 18:37:53 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Simple name-value pair index set for selecting elementID's
 *              from the database
 *
 ***************************************************************************
 *
 * $Log: StDbElementIndex.hh,v $
 * Revision 1.1  2001/01/22 18:37:53  porter
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
#ifndef STDBELEMENTINDEX_HH
#define STDBELEMENTINDEX_HH

#define N_MAX_INDEXVALS 10
#include <string.h>

struct indexNameVals {

  char iname[64];
  int  ival;

};

class StDbElementIndex {

  int  mnumIndeces;
  indexNameVals mnvals[N_MAX_INDEXVALS];
  int  mcurrent;

 public:

  StDbElementIndex();
  StDbElementIndex(StDbElementIndex& inval);
  virtual ~StDbElementIndex(){};

  virtual void  clearIndex();

  virtual void  addElementIndex(StDbElementIndex* inval);
  virtual int   addNameValuePair(const char* name, int ival);
  virtual int   getNumIndeces() const;
  virtual int   getIndexVal(int indexNumber);
  virtual char* getIndexName(int indexNumber);
  virtual char* printIndexName(int indexNumber);

  virtual void  resetCounter();
  virtual char* getNextIndex(int& indexVal);
  virtual char* printNextIndex(int& indexVal);

};

inline void StDbElementIndex::clearIndex() { 
 mcurrent=0; mnumIndeces=0; 
 memset(&mnvals,0,N_MAX_INDEXVALS*sizeof(indexNameVals));
}
inline int StDbElementIndex::getNumIndeces() const { return mnumIndeces; }

inline int StDbElementIndex::getIndexVal(int indexNum) {
  if(indexNum>=N_MAX_INDEXVALS)return -1;
  return mnvals[indexNum].ival;
}
inline char* StDbElementIndex::printIndexName(int indexNum) {
  if(indexNum>=N_MAX_INDEXVALS)return (char*)0;
  return (char*)mnvals[indexNum].iname;
}
inline void StDbElementIndex::resetCounter() { mcurrent=0; }

#endif


