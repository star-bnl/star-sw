#ifndef STDBACCESSOR_H
#define STDBACCESSOR_H

struct StDbAccessor {
 StDbType dbType;
 StDbDomain dbDomain;
 int schemaID;
 int beginTime;
 int endTime;
 int version;
 int elementID;
 int requestTime;
};
 


#endif

