/***************************************************************************
 *
 * $Id: StDbTable.h,v 1.10 2000/01/27 05:54:35 porter Exp $
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
 * Revision 1.10  2000/01/27 05:54:35  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
 * Revision 1.9  2000/01/19 20:20:07  porter
 * - finished transaction model needed by online
 * - fixed CC5 compile problem in StDbNodeInfo.cc
 * - replace TableIter class by StDbTableIter to prevent name problems
 *
 * Revision 1.8  2000/01/10 20:37:55  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
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
 
#include "StDbNode.hh"
#include "StDbTime.h"
#include "typeAcceptor.hh"
#include "StTableDescriptorI.h"
#include "StDbBufferI.h"
#include <string.h>

class StDbBuffer;

class StDbTable : public StDbNode {

protected:
//! validity interval
StDbTime mbeginTime;
StDbTime mendTime;

int      mschemaID;
int*     melementID;

//! these are for rolling back stores
int*     mdataIDs;
int      mdataRows;
int      mMaxRows;

//! c-struct descriptor information                      
bool     mhasDescriptor;//!
StTableDescriptorI* mdescriptor;//!

//! data & num of rows
char*    mdata;//!
int      mrows;
int      mrowNumber;

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

  virtual ~StDbTable(){         if(melementID) delete [] melementID;
                                if(mdescriptor)delete mdescriptor; 
                                if(mdata) delete [] mdata; };

  virtual void setNodeInfo(StDbNodeInfo* node);

  virtual unsigned int getTableSize() const;

  virtual unsigned int getEndTime() const ;
  virtual char*        getEndDateTime();
  virtual void         setEndTime(unsigned int time);
  virtual void         setEndTime(const char* time);

  virtual char*        getBeginDateTime();
  virtual unsigned int getBeginTime() const  ;
  virtual void         setBeginTime(unsigned int time) ;
  virtual void         setBeginTime(const char* time);

  virtual int*         getElementID(int& nrows);
  virtual int          getRowID(int rowNumber) const ;
  virtual void         setElementID(int* elements, int nrows=1) ; 

  virtual int          getSchemaID() const ; 
  virtual void         setSchemaID(int id) ; 

  virtual void         addWrittenRow(int dataID);
  virtual int*         getWrittenRows(int* numrows);
  virtual void         commitData();

  // c-struct descriptort & schema 
  // set by 1st call to db

  virtual StTableDescriptorI* getDescriptorCpy() const;
  virtual void                setDescriptor(StTableDescriptorI* descriptor);
  virtual bool                hasDescriptor() const;

  //
  // access to date via this table or c-struct

  virtual StDbTable* Clone();
  virtual char*       GetTable(); 
  virtual void*       GetTableCpy();
  virtual void        SetTable(char* data, int nrows);
  virtual void        AddRows(char* data, int nrows);
  virtual int         GetNRows() const;
  virtual void        SetNRows(int nrows){ mrows = nrows; }; 
  virtual void        setRowNumber(int row=0);
  
 
  // methods for reading & writing to Db & to file

  virtual void StreamAccessor(typeAcceptor* accept, bool isReading);
  virtual void dbStreamer(typeAcceptor* accept, bool isReading);

  virtual void StreamAccessor(StDbBufferI* buff, bool isReading);
  virtual void dbStreamer(StDbBufferI* buff, bool isReading);
  virtual void dbTableStreamer(StDbBufferI* buff, const char* name, bool isReading);

  //ClassDef(StDbTable,1)
};


inline 
unsigned int
StDbTable::getTableSize() const { if(mhasDescriptor) return mdescriptor->getTotalSizeInBytes();
return 0;
}

inline 
unsigned int StDbTable::getEndTime() const { 
return mendTime.munixTime; }

inline 
char* StDbTable::getEndDateTime() { 
return mendTime.mdateTime; }

inline 
void StDbTable::setEndTime(unsigned int time) {
mendTime.munixTime = time; }

inline 
void StDbTable::setEndTime(const char* time){ 
mendTime.setDateTime(time); }

inline 
unsigned int StDbTable::getBeginTime() const  { 
return mbeginTime.munixTime; }

inline 
char* StDbTable::getBeginDateTime() { 
return mbeginTime.mdateTime; }

inline 
void StDbTable::setBeginTime(unsigned int time) {
mbeginTime.munixTime = time; }

inline 
void StDbTable::setBeginTime(const char* time){ 
mbeginTime.setDateTime(time); }

inline 
int* StDbTable::getElementID(int& nrows) { nrows = mrows; 
return melementID; }

inline
int StDbTable::getRowID(int rowNumber) const { 
  if(rowNumber<mrows)return melementID[rowNumber];
return 0;
}

inline 
int StDbTable::getSchemaID() const { return mschemaID; }

inline 
void StDbTable::setSchemaID(int id) {mschemaID = id; }

inline
int* StDbTable::getWrittenRows(int* nrows){
*nrows=mdataRows;
return mdataIDs;
}

inline
void StDbTable::commitData() { 
mdataRows = 0;
mMaxRows  = 500;
if(mdataIDs) delete [] mdataIDs;
mdataIDs = 0;

}

inline
bool StDbTable::hasDescriptor() const { return mhasDescriptor; }

inline 
StDbTable* StDbTable::Clone() {return (new StDbTable(*this));}

inline 
int StDbTable::GetNRows() const { return mrows; }

inline 
void StDbTable::setRowNumber(int row){
if(row < mrows)mrowNumber = row;
}


#endif











