/***************************************************************************
 *
 * $Id: StDbAccessor.h,v 1.4 1999/09/30 02:06:00 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: C++ c-struct to hold table address in database
 *
 ***************************************************************************
 *
 * $Log: StDbAccessor.h,v $
 * Revision 1.4  1999/09/30 02:06:00  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBACCESSOR_H
#define STDBACCESSOR_H

#include "StDbDefs.hh"
#include "StDbTime.h"

// a C++ c-struct containing the "address" information
// of a table row(s) in the database

class StDbAccessor {
public:

  StDbAccessor(): dbType(StarDb), dbDomain(Unknown), version(0), elementID(0){};

  ~StDbAccessor(){
    if(version) delete [] version;
    if(elementID) delete [] elementID;
   ;}

 StDbType dbType;
 StDbDomain dbDomain;
 int schemaID;
 StDbTime beginTime;
 StDbTime endTime;
 char* version;
 int* elementID;
 int requestTime;
};
 

#endif



