#ifndef STDBACCESSOR_H
#define STDBACCESSOR_H

#include "StDbDefs.hh"

class StDbAccessor {
public:
  StDbAccessor(){};
  ~StDbAccessor(){};
 StDbType dbType;
 StDbDomain dbDomain;
 int schemaID;
 int beginTime;
 int endTime;
 char version[20];
 int elementID;
 int requestTime;
};
 


#endif



