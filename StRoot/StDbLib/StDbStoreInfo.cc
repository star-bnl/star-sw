/***************************************************************************
 *
 * $Id: StDbStoreInfo.cc,v 1.1 2001/01/22 18:38:00 porter Exp $
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
 * $Log: StDbStoreInfo.cc,v $
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
 **************************************************************************/
#include "StDbStoreInfo.hh"
#include <string.h>

#define N_ROWS_INCREMENT 500

StDbStoreInfo::StDbStoreInfo() : mdataIDs(0), mnumRows(0), mMaxRows(0), mcanRollBack(false) {};

StDbStoreInfo::~StDbStoreInfo() { resetStoreInfo(); }

//////////////////////////////////////////////////////////////////////

void StDbStoreInfo::reSizeInternalStore(){

   int newMaxRows = mMaxRows+N_ROWS_INCREMENT;
   int* newDataIDs = new int[newMaxRows];

   memset(newDataIDs,0,newMaxRows*sizeof(int));
   if(mdataIDs){
     memcpy(newDataIDs,mdataIDs,mnumRows*sizeof(int));
     delete [] mdataIDs;
   }
   mdataIDs = newDataIDs;
   mMaxRows = newMaxRows;

}

//////////////////////////////////////////////////////////////////////

int* StDbStoreInfo::getDataIDs(int& numRows){
  numRows=mnumRows;
  return mdataIDs;
}

//////////////////////////////////////////////////////////////////////

void StDbStoreInfo::addWrittenRow(int dataID){
  
  if(mnumRows==mMaxRows)reSizeInternalStore();
  mdataIDs[mnumRows]=dataID;
  mnumRows++;

};


void StDbStoreInfo::resetStoreInfo(){  
  if(mdataIDs) delete [] mdataIDs;
  mdataIDs=0;
  mnumRows=0;
  mMaxRows=0;
  mcanRollBack=false;
}

