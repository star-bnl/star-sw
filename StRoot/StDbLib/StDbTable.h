/***************************************************************************
 *
 * $Id: StDbTable.h,v 1.7 1999/12/03 19:02:01 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:   Class that holds data, descriptor, & db-address 
 *                & performs streamer of db-data into data-memory
 *
 ***************************************************************************
 *
 * $Log: StDbTable.h,v $
 * Revision 1.7  1999/12/03 19:02:01  porter
 * modified descriptor to accept tableDescriptor once this St_base object
 * has been updated to have longer name lengths.
 *
 * Revision 1.6  1999/10/19 14:30:39  porter
 * modifications relevant to use with StDbBroker and future merging with
 * "params" database structure + some docs + suppressing diagnostics messages
 *
 * Revision 1.5  1999/09/30 02:06:10  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef StDbTable_HH
#define StDbTable_HH
 
#include "StDbTableI.h"
#include "typeAcceptor.hh"
#include "StTableDescriptorI.h"
#include "StDbAccessor.h"
#include <string.h>

class StDbBuffer;

class StDbTable : public StDbTableI {

protected:

StDbAccessor maccessor;//!

  char* mtableName;//!
  int mstructID;//!  database equivalent to tableName

  bool misBaseLine;
  bool mhasDescriptor;//!
  StTableDescriptorI* mdescriptor;//!

  char* mdata;//!
  int mrows;
  int mrowNumber;

  virtual void ReadElement(char*& ptr, char* name, int length, StTypeE type, StDbBuffer* buff);
  virtual void WriteElement(char* ptr, char* name, int length, StTypeE type, StDbBuffer* buff);
  virtual void PassOutElement(char* ptr, char* name, int length, StTypeE type, typeAcceptor* accept);
  virtual void PassInElement(char* ptr, char* name, int length, StTypeE type, typeAcceptor* accept);
  virtual void getElementSpecs(int elementNum, char*& c, char*& name, unsigned int& length,StTypeE& type);

  bool createMemory();
  bool createMemory(int nrows);
  char* duplicateData();
  void checkDescriptor();


public:

  StDbTable(const char* tableName);
  StDbTable(const char* tableName, int schemaID);
  StDbTable(StDbTable& table);

  virtual ~StDbTable(){if(mtableName)delete [] mtableName;
                                if(mdescriptor)delete mdescriptor; 
                                if(mdata) delete [] mdata; };

  virtual StDbAccessor getAccessor() const ;
  virtual void setAccessor(StDbAccessor a) ;
  virtual unsigned int getTableSize() const;
  virtual char* getTableName() const;
  virtual void setTableName(const char* name);
  virtual bool checkTableName(const char* name);
  virtual StDbType getDbType() const  ;
  virtual void setDbType(StDbType type) ;
  virtual StDbDomain getDbDomain() const ;
  virtual void setDbDomain(StDbDomain domain); 
  virtual char* getVersion() const ;
  virtual void setVersion(char* version) ;

  //  virtual int  getRequestTime() const ;
  //  virtual void setRequestTime(int time) ;

  virtual unsigned int getEndTime() const ;
  virtual char* getEndDateTime();
  virtual void setEndTime(unsigned int time);
  virtual void setEndTime(const char* time);

  virtual char*  getBeginDateTime();
  virtual unsigned int getBeginTime() const  ;
  virtual void setBeginTime(unsigned int time) ;
  virtual void setBeginTime(const char* time);

  virtual int* getElementID() const ;
  virtual int  getRowID(int rowNumber) const ;
  virtual void setElementID(int* elements, int nrows=1) ; 

  virtual int  getSchemaID() const ; 
  virtual void setSchemaID(int id) ; 
  virtual bool isBaseLine() const;
  virtual void setIsBaseLine(bool baseline); 

  // c-struct descriptort & schema 
  // set by 1st call to db

  virtual int getStructID() const;
  virtual void setStructID(int structID);
  virtual StTableDescriptorI* getDescriptorCpy() const;
  virtual void setDescriptor(StTableDescriptorI* descriptor);
  virtual bool hasDescriptor() const {return mhasDescriptor;};

  //
  // access to date via this table or c-struct

  virtual StDbTableI* Clone();
  virtual char* GetTable(); 
  virtual void* GetTableCpy();
  virtual void SetTable(char* data, int nrows);
  virtual void AddRows(char* data, int nrows);
  virtual int  GetNRows() const;
  virtual void SetNRows(int nrows){ mrows = nrows; }; 
  virtual void setRowNumber(int row=0);
  
 
  // methods for reading & writing to Db & to file

  virtual void StreamAccessor(typeAcceptor* accept, bool isReading);
  virtual void dbStreamer(typeAcceptor* accept, bool isReading);

  virtual void StreamAccessor(StDbBufferI* buff, bool isReading);
  virtual void dbStreamer(StDbBufferI* buff, bool isReading);
  virtual void dbTableStreamer(StDbBufferI* buff, const char* name, bool isReading);

  //ClassDef(StDbTable,1)
};


inline 
int StDbTable::getStructID() const {return mstructID;}

inline
void StDbTable::setStructID(int structID) {mstructID=structID;}

inline 
unsigned int
StDbTable::getTableSize() const {
if(mdescriptor) return mdescriptor->getTotalSizeInBytes();
return 0;
}

inline 
StDbAccessor StDbTable::getAccessor() const {return maccessor;}

inline 
void StDbTable::setAccessor(StDbAccessor a) {maccessor = a;}

inline 
bool StDbTable::checkTableName(const char* name) {
if(strcmp(name,mtableName)==0) return true;
return false;
}


inline 
StDbType StDbTable::getDbType() const  { return maccessor.dbType; }

inline 
void StDbTable::setDbType(StDbType type) {maccessor.dbType = type; }

inline 
StDbDomain StDbTable::getDbDomain() const { return maccessor.dbDomain; }

inline 
void StDbTable::setDbDomain(StDbDomain domain) {maccessor.dbDomain = domain; }

//inline 
//int StDbTable::getRequestTime() const { return maccessor.requestTime; }

//inline 
//void StDbTable::setRequestTime(int time) { maccessor.requestTime = time; }

inline 
char* StDbTable::getVersion() const { 

if(!maccessor.version)return maccessor.version;
char* retString=new char[strlen(maccessor.version)+1];
strcpy(retString,maccessor.version);
return retString;

}

inline 
void StDbTable::setVersion(char* version) {

if(maccessor.version)delete [] maccessor.version;
maccessor.version = new char[strlen(version)+1];
strcpy(maccessor.version,version); 

}

inline 
unsigned int StDbTable::getEndTime() const { 
return maccessor.endTime.munixTime; }

inline 
char* StDbTable::getEndDateTime() { 
return maccessor.endTime.mdateTime; }

inline 
void StDbTable::setEndTime(unsigned int time) {
maccessor.endTime.munixTime = time; }

inline 
void StDbTable::setEndTime(const char* time){ 
maccessor.endTime.setDateTime(time); }

inline 
unsigned int StDbTable::getBeginTime() const  { 
return maccessor.beginTime.munixTime; }

inline 
char* StDbTable::getBeginDateTime() { 
return maccessor.beginTime.mdateTime; }

inline 
void StDbTable::setBeginTime(unsigned int time) {
maccessor.beginTime.munixTime = time; }

inline 
void StDbTable::setBeginTime(const char* time){ 
maccessor.beginTime.setDateTime(time); }

inline 
int* StDbTable::getElementID() const { return maccessor.elementID; }

inline
int StDbTable::getRowID(int rowNumber) const { 
  if(rowNumber<mrows)return maccessor.elementID[rowNumber];
return 0;
}

inline 
int StDbTable::getSchemaID() const { return maccessor.schemaID; }

inline 
void StDbTable::setSchemaID(int id) {maccessor.schemaID = id; }

inline
bool StDbTable::isBaseLine() const { return misBaseLine; }

inline
void StDbTable::setIsBaseLine(bool baseline) { misBaseLine = baseline; }


inline 
StDbTableI* StDbTable::Clone(){return (new StDbTable(*this));}

inline 
int StDbTable::GetNRows() const { return mrows; }

inline 
void StDbTable::setRowNumber(int row){
if(row < mrows)mrowNumber = row;
}


#endif











