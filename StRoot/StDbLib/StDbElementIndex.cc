/***************************************************************************
 *
 * $Id: StDbElementIndex.cc,v 1.1 2001/01/22 18:37:53 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Simple name-value pair index set for selecting elementID's
 *              from the database
 *
 ***************************************************************************
 *
 * $Log: StDbElementIndex.cc,v $
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
#include "StDbElementIndex.hh"
#include <string.h>

StDbElementIndex::StDbElementIndex() {  clearIndex(); };

StDbElementIndex::StDbElementIndex(StDbElementIndex& inval) {

  clearIndex();
  mnumIndeces = inval.getNumIndeces();
  inval.resetCounter();
  char* ctest;
  int ival;
  while ( (ctest=inval.printNextIndex(ival)) ){
    strcpy(mnvals[mcurrent].iname,ctest);
    mnvals[mcurrent].ival = ival;
    mcurrent++;
  }
  resetCounter();
  inval.resetCounter();
}

////////////////////////////////////////////////////////////////
void
StDbElementIndex::addElementIndex(StDbElementIndex* inval) {

  if(!inval) return;
  clearIndex();
  mnumIndeces = inval->getNumIndeces();
  inval->resetCounter();
  char* ctest;
  int ival;
  while ( (ctest=inval->printNextIndex(ival))){
    strcpy(mnvals[mcurrent].iname,ctest);
    mnvals[mcurrent].ival = ival;
    mcurrent++;
  }

  resetCounter();
  inval->resetCounter();

}
////////////////////////////////////////////////////////////////

int
StDbElementIndex::addNameValuePair(const char* name, int ival){

  strcpy(mnvals[mnumIndeces].iname,name);
  mnvals[mnumIndeces].ival = ival;
  int retVal=mnumIndeces;
  mnumIndeces++;
  return retVal;
};

////////////////////////////////////////////////////////////////

char*
StDbElementIndex::getIndexName(int indexNumber){
  char* retVal=0;
  if(indexNumber<mnumIndeces){
    retVal = new char[strlen((char*)mnvals[indexNumber].iname)+1];
    strcpy(retVal,(char*)mnvals[indexNumber].iname);
  }
  return retVal;
}

////////////////////////////////////////////////////////////////

char*
StDbElementIndex::getNextIndex(int& indexVal) {
  char* retVal=0;
  if(mcurrent<mnumIndeces){
    retVal = new char[strlen((char*)mnvals[mcurrent].iname)+1];
    strcpy(retVal,(char*)mnvals[mcurrent].iname);
    indexVal=mnvals[mcurrent].ival;
    mcurrent++;
  }
  return retVal;
}

////////////////////////////////////////////////////////////////
char*
StDbElementIndex::printNextIndex(int& indexVal) {
  char* retVal=0;
  if(mcurrent<mnumIndeces){
    retVal = (char*)mnvals[mcurrent].iname;
    indexVal=mnvals[mcurrent].ival;
    mcurrent++;
  }
  return retVal;
}
////////////////////////////////////////////////////////////////




