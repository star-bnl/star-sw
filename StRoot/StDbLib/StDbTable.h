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
  int mtableID;//!

  bool mhasDescriptor;//!
StTableDescriptorI* mdescriptor;//!

  char* mdata;//!

  virtual void ReadElement(char*& ptr, char* name, int length, StTypeE type, StDbBuffer* buff);
  virtual void WriteElement(char* ptr, char* name, int length, StTypeE type, StDbBuffer* buff);
  virtual void PassElement(char* ptr, char* name, int length, StTypeE type, typeAcceptor* accept);
  virtual void getElementSpecs(int elementNum, char*& c, char*& name, unsigned int& length,StTypeE& type);

  bool createMemory();
  void checkDescriptor();

public:

  StDbTable(const char* tableName);
  StDbTable(const char* tableName, int tableID);
  StDbTable(StDbTable& table);

  virtual ~StDbTable(){if(mtableName)delete [] mtableName;
                                if(mdescriptor)delete mdescriptor; 
                                if(mdata) delete [] mdata; };

  virtual StDbAccessor getAccessor() const ;
  virtual void setAccessor(StDbAccessor a) ;
  virtual int getTableID() const;
  virtual void setTableID(int tableID);
  virtual unsigned int getTableSize() const;
  virtual char* getTableName() const;
  virtual void setTableName(const char* name);
  virtual StDbType getDbType() const  ;
  virtual void setDbType(StDbType type) ;
  virtual StDbDomain getDbDomain() const ;
  virtual void setDbDomain(StDbDomain domain); 
  virtual int getRequestTime() const ;
  virtual void setRequestTime(int time) ;
  virtual char* getVersion() const ;
  virtual void setVersion(char* version) ;
  virtual int getEndTime() const ;
  virtual void setEndTime(int time) ;
  virtual int getBeginTime() const  ;
  virtual void setBeginTime(int time) ;
  virtual int getElementID() const ;
  virtual void setElementID(int id) ; 
  virtual int getSchemaID() const ; 
  virtual void setSchemaID(int id) ; 
 

  virtual StTableDescriptorI* getDescriptorCpy() const;
  virtual void setDescriptor(StTableDescriptorI* descriptor);
  virtual bool hasDescriptor() const {return mhasDescriptor;};

  virtual StDbTableI* Clone();
  virtual char* GetTable(); 

  virtual void StreamAccessor(typeAcceptor* accept);
  virtual void dbStreamer(typeAcceptor* accept);

  virtual void StreamAccessor(StDbBufferI* buff, bool isReading);
  virtual void dbStreamer(StDbBufferI* buff, bool isReading);

  //ClassDef(StDbTable,1)


};

inline 
int StDbTable::getTableID() const {return mtableID;}

inline
void StDbTable::setTableID(int tableID) {mtableID=tableID;}

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
StDbType StDbTable::getDbType() const  { return maccessor.dbType; }

inline 
void StDbTable::setDbType(StDbType type) {maccessor.dbType = type; }

inline 
StDbDomain StDbTable::getDbDomain() const { return maccessor.dbDomain; }

inline 
void StDbTable::setDbDomain(StDbDomain domain) {maccessor.dbDomain = domain; }

inline 
int StDbTable::getRequestTime() const { return maccessor.requestTime; }

inline 
void StDbTable::setRequestTime(int time) { maccessor.requestTime = time; }

inline 
char* StDbTable::getVersion() const { 
if(!maccessor.version)return maccessor.version;
char* retString=new char[strlen(maccessor.version)+1];
strcpy(retString,maccessor.version);
return retString;
}

inline 
void StDbTable::setVersion(char* version) {strcpy(maccessor.version,version); }

inline 
int StDbTable::getEndTime() const { return maccessor.endTime; }

inline 
void StDbTable::setEndTime(int time) {maccessor.endTime = time; }

inline 
int StDbTable::getBeginTime() const  { return maccessor.beginTime; }

inline 
void StDbTable::setBeginTime(int time) {maccessor.beginTime = time; }

inline 
int StDbTable::getElementID() const { return maccessor.elementID; }

inline 
void StDbTable::setElementID(int id) {maccessor.elementID = id; }

inline 
int StDbTable::getSchemaID() const { return maccessor.schemaID; }

inline 
void StDbTable::setSchemaID(int id) {maccessor.schemaID = id; }

inline
void StDbTable::setDescriptor(StTableDescriptorI* descriptor){ 
 if(mdescriptor) delete mdescriptor;
 mdescriptor=descriptor;
 mhasDescriptor=true;
 //checkDescriptor();
};

inline StDbTableI* 
StDbTable::Clone(){return (new StDbTable(*this));};


#endif











