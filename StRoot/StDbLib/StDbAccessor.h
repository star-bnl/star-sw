#ifndef STDBACCESSOR_H
#define STDBACCESSOR_H

#include "StDbDefs.hh"
#include "TObject.h"

class StDbAccessor {
public:
  StDbAccessor(){};
  ~StDbAccessor(){};
 StDbType dbType;
 StDbDomain dbDomain;
 int schemaID;
 int beginTime;
 int endTime;
 int version;
 int elementID;
 int requestTime;
 ClassDef(StDbAccessor,0)
};
 


#endif

