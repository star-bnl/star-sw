/***************************************************************************
 *
 * $Id: StDbTableI.h,v 1.3 1999/09/30 02:06:10 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  interface to for StDbTable functionality - 
 *               may not be needed later on but keep for now
 *
 ***************************************************************************
 *
 * $Log: StDbTableI.h,v $
 * Revision 1.3  1999/09/30 02:06:10  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
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

  virtual unsigned int getTableSize() const =0;
  virtual char* getTableName() const =0;
  virtual void setTableName(const char* name) =0;
  virtual StDbType getDbType() const   =0;
  virtual void setDbType(StDbType type)  =0;
  virtual StDbDomain getDbDomain() const  =0;
  virtual void setDbDomain(StDbDomain domain) =0; 
  //  virtual int getRequestTime() const  =0;
  //  virtual void setRequestTime(int time)  =0;

  virtual char* getVersion() const  =0;
  virtual void setVersion(char* version)  =0;

  // unixTime
  virtual unsigned int getEndTime() const  =0;
  virtual void setEndTime( unsigned int time)  =0;
  virtual unsigned int getBeginTime() const   =0;
  virtual void setBeginTime( unsigned int time)  =0;

  // dateTime
  virtual char* getEndDateTime()  =0;
  virtual void setEndTime(const char* time)  =0;
  virtual char* getBeginDateTime()   =0;
  virtual void setBeginTime( const char* time)  =0;

  virtual int* getElementID() const  =0;
  virtual int getRowID(int rowNumber) const = 0;
  virtual void setElementID(int* elements, int nrows)  =0;
 
  virtual int getSchemaID() const  =0; 
  virtual void setSchemaID(int id)  =0;  
  virtual bool isBaseLine() const =0;
  virtual void setIsBaseLine(bool baseline) =0; 

  virtual StTableDescriptorI* getDescriptorCpy() const =0;
  virtual void setDescriptor(StTableDescriptorI* descriptor) =0;
  virtual bool hasDescriptor() const  =0;

  virtual StDbTableI* Clone() =0;
  virtual char* GetTable() =0; 
  virtual void SetTable(char* data, int nrows)=0;
  virtual void AddRows(char* data, int nrows)=0;
  virtual int GetNRows() const=0;
  virtual void setRowNumber(int row) =0;

  virtual void StreamAccessor(typeAcceptor* accept) =0;
  virtual void dbStreamer(typeAcceptor* accept) =0;

  virtual void StreamAccessor(StDbBufferI* buff, bool isReading) =0;
  virtual void dbStreamer(StDbBufferI* buff, bool isReading) =0;

  //ClassDef(StDbTableI,1)

};

#endif







