#ifndef STDBTABLEI_HH
#define STDBTABLEI_HH


#include "StDbDefs.hh"
#include "StDbAccessor.h"
#include "StTableDescriptorI.h"
#include "typeAcceptor.hh"
#include "StDbBufferI.h"


class StDbTableI {


public:

  virtual ~StDbTableI(){};

  virtual StDbAccessor getAccessor() const  =0;
  virtual void setAccessor(StDbAccessor a)  =0;
  virtual int getTableID() const =0;
  virtual void setTableID(int tableID) =0;
  virtual unsigned int getTableSize() const =0;
  virtual char* getTableName() const =0;
  virtual void setTableName(const char* name) =0;
  virtual StDbType getDbType() const   =0;
  virtual void setDbType(StDbType type)  =0;
  virtual StDbDomain getDbDomain() const  =0;
  virtual void setDbDomain(StDbDomain domain) =0; 
  virtual int getRequestTime() const  =0;
  virtual void setRequestTime(int time)  =0;
  virtual char* getVersion() const  =0;
  virtual void setVersion(char* version)  =0;
  virtual int getEndTime() const  =0;
  virtual void setEndTime(int time)  =0;
  virtual int getBeginTime() const   =0;
  virtual void setBeginTime(int time)  =0;
  virtual int getElementID() const  =0;
  virtual void setElementID(int id)  =0; 
  virtual int getSchemaID() const  =0; 
  virtual void setSchemaID(int id)  =0; 
 
  virtual StTableDescriptorI* getDescriptorCpy() const =0;
  virtual void setDescriptor(StTableDescriptorI* descriptor) =0;
  virtual bool hasDescriptor() const  =0;

  virtual StDbTableI* Clone() =0;
  virtual char* GetTable() =0; 

  virtual void StreamAccessor(typeAcceptor* accept) =0;
  virtual void dbStreamer(typeAcceptor* accept) =0;

  virtual void StreamAccessor(StDbBufferI* buff, bool isReading) =0;
  virtual void dbStreamer(StDbBufferI* buff, bool isReading) =0;

  //ClassDef(StDbTableI,1)

};

#endif







