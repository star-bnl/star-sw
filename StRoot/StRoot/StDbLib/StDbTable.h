/***************************************************************************
 *
 * $Id: StDbTable.h,v 1.26 2005/09/07 22:04:02 deph Exp $
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
 * Revision 1.26  2005/09/07 22:04:02  deph
 * update to correct padding issue for packed tables
 *
 * Revision 1.25  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.24  2003/09/16 22:44:18  porter
 * got rid of all ostrstream objects; replaced with StString+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.23  2003/09/02 17:57:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.22  2003/04/11 22:47:36  porter
 * Added a fast multi-row write model specifically needed by the daqEventTag
 * writer. Speed increased from about 100Hz to ~3000Hz.  It is only invoked if
 * the table is marked as Non-Indexed (daqTags & scalers). For non-indexed tables
 * which include binary stored data (we don't have any yet), the fast writer  has
 * to invoke a slower buffer so that the rates are a bit slower (~500Hz at 50 rows/insert).
 *
 * Revision 1.21  2003/02/12 22:12:45  porter
 * moved warning message about null columns (checked in 2 days ago) from the
 * depths of the mysql coding into the StDbTable code. This suppresses confusing
 * warnings from tables that have had elements removed but their storage columns
 * still exist in the database.
 *
 * Revision 1.20  2003/01/10 04:19:21  porter
 * added feature of getting timestamp list (but no data) for a table.
 * fixed 2 features sometimes used in online in query-by-whereclause.
 * removed a stray 'cout' in a routine that is rarely accessed
 *
 * Revision 1.19  2002/01/30 15:40:48  porter
 * changed limits on flavor tag & made defaults retrieving more readable
 *
 * Revision 1.18  2001/12/21 04:54:46  porter
 * sped up table definition for emc and changed some ostrstream usage for
 * insure tests
 *
 * Revision 1.17  2001/10/24 04:05:20  porter
 * added long long type to I/O and got rid of obsolete dataIndex table
 *
 * Revision 1.16  2001/02/09 23:06:25  porter
 * replaced ostrstream into a buffer with ostrstream creating the
 * buffer. The former somehow clashed on Solaris with CC5 iostream (current .dev)
 *
 * Revision 1.15  2001/01/22 18:38:00  porter
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
 * Revision 1.14  2000/06/30 01:57:02  porter
 * fixed a delete bug & small memory leak found by Akio via Insure++ ,
 * updated SetTable() method for containing idList, corrected enumeration
 * map to rhic domain for Conditions_rhic database
 *
 * Revision 1.13  2000/06/02 13:37:37  porter
 * built up list of minor changes:
 *  - made buffer more robust for certain null inputs
 *  - fixed small leak in StDbTables & restructure call to createMemory
 *  - added dbRhic as a database domain in StDbDefs
 *  - added setUser() in StDbManager
 *  - added more diagnostic printouts in mysqlAccessor.cc
 *
 * Revision 1.12  2000/04/25 18:26:03  porter
 * added flavor & production time as settable query fields in
 * table &/or node. Associated SQL updated in mysqlAccessor.
 * Flavor key supports "+" as an OR symbol.
 *
 * Revision 1.11  2000/02/15 20:27:45  porter
 * Some updates to writing to the database(s) via an ensemble (should
 * not affect read methods & haven't in my tests.
 *  - closeAllConnections(node) & closeConnection(table) method to mgr.
 *  - 'NullEntry' version to write, with setStoreMode in table;
 *  -  updated both StDbTable's & StDbTableDescriptor's copy-constructor
 *
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
//#include "typeAcceptor.hh"
#include "StTableDescriptorI.h"
//#include "StDbBufferI.h"
#include "StDbStoreInfo.hh"
#include <string.h>

class StDbBufferI;
class StDbBuffer;
class typeAcceptor;

#ifdef __ROOT__
#include "TROOT.h"
#endif

class StDbTable : public StDbNode {

protected:

char* mflavor;
bool mdefaultFlavor;
unsigned int mprodTime;

//! validity interval
StDbTime mbeginTime;
StDbTime mendTime;
StDbTime mendStoreTime; // for writing null endTimes

// DB storage information
char*    mstructName;
char*    melementName;
bool     mIsBaseLine;
bool     mIsBinary;
bool     mIsIndexed;
char*    mdataTable;

int      mschemaID;
int*     melementID;

//! for rolling back stores
 StDbStoreInfo mstoredData;

//! c-struct descriptor information                      
bool     mhasDescriptor;//!
StTableDescriptorI* mdescriptor;//!

//! data & num of rows
char*    mdata;//!
int      mrows;
int      mrowNumber;
bool     mhasData;
int      mrowsRequested; // for query by where clause
unsigned int* mtimeVals;

  virtual void ReadElement(char*& ptr, char* name, int length, StTypeE type, StDbBuffer* buff);
  virtual void WriteElement(char* ptr, char* name, int length, StTypeE type, StDbBuffer* buff);
  virtual void PassOutElement(char* ptr, char* name, int length, StTypeE type, typeAcceptor* accept);
  virtual void PassInElement(char* ptr, char* name, int length, StTypeE type, typeAcceptor* accept);
  virtual void getElementSpecs(int elementNum, char*& c, char*& name, unsigned int& length,StTypeE& type);

  bool createMemory();
  bool createMemory(int nrows);
  char* duplicateData();
  void  init();
  void printNoDataReturned(const char* elementName);


public:

  StDbTable(const char* tableName);
  StDbTable(const char* tableName, int schemaID);
  StDbTable(StDbTable& table);

  virtual ~StDbTable(){         if(melementID) delete [] melementID;
                                if(mdescriptor)delete mdescriptor; 
                                if(mdata) delete [] mdata; 
                                if(mdataTable) delete [] mdataTable;
                                if(mstructName) delete []  mstructName;
                                if(melementName) delete [] melementName;
                                if(mtimeVals) delete [] mtimeVals;
                                if(mflavor) delete [] mflavor;
                       };

  virtual bool         IsTable() const;
  virtual void         setNodeInfo(StDbNode* node);
  virtual unsigned int getTableSize() const;
  virtual char*        getCstructName();
  virtual char*        printCstructName();
  virtual void         setCstructName(const char* name);
  virtual void         setDataTable(const char* name);
  virtual char*        getDataTable();
  virtual char*        printDataTable();

  // flavor is "ofl", "onl", "sim", .... it is an char[16] array.
  // 16 char is enough to later have "ofl|onl" type syntax

  virtual char*        getFlavor();
  virtual char*        printFlavor();
  virtual void         setFlavor(const char* flavor);
  virtual bool         defaultFlavor() const;
  virtual void         setDefaultFlavor();

  virtual unsigned int getProdTime();
  virtual void         setProdTime(unsigned int ptime);

  virtual unsigned int getEndTime() const ;
  virtual char*        getEndDateTime();
  virtual void         setEndTime(unsigned int time);
  virtual void         setEndTime(const char* time);

  virtual unsigned int getBeginTime() const  ;
  virtual char*        getBeginDateTime();
  virtual void         setBeginTime(unsigned int time) ;
  virtual void         setBeginTime(const char* time);

  virtual unsigned int getEndStoreTime() const;
  virtual char*        getEndStoreDateTime();
  virtual void         setEndStoreTime(unsigned int time);
  virtual void         setEndStoreTime(const char* time);
  
  virtual int*         getElementID(int& nrows);
  virtual int          getRowID(int rowNumber) const ;
  virtual char*        getElementName();
  virtual char*        printElementName();
  virtual void         setElementName(const char* ename);
  virtual void         setElementID(int* elements, int nrows); 
  
  virtual void         setBaseLine(bool baseLine);
  virtual void         setIndexed(bool indexed);
  virtual void         setBinary(bool abinary);
  virtual bool         IsBaseLine() const;
  virtual bool         IsIndexed() const;
  virtual bool         IsBinary() const;

  virtual int          getSchemaID() const ; 
  virtual void         setSchemaID(int id) ; 

  // storage of dataIDs
  virtual void  addWrittenRows(int* dataID,int numRows,bool canRollBack=false);
  virtual int*  getWrittenRows(int& numRows);
  virtual void  commitData();
  virtual void  clearStoreInfo();
  virtual unsigned int* getTimeValues();
  virtual unsigned int  getMaxTime();
  virtual void          setTimeValues(unsigned int* timeValues);

  // c-struct descriptor & schema 
  // set by 1st call to db

  virtual StTableDescriptorI* getDescriptorCpy() const;
  virtual StTableDescriptorI* getDescriptor();
  virtual void                setDescriptor(StTableDescriptorI* descriptor);
  virtual bool                hasDescriptor() const;
  void                        checkDescriptor();

  // access to date via this table or c-struct

  virtual StDbTable*  Clone();
  virtual char*       GetTable(); 
  virtual void*       GetTableCpy(); //! calloc'd version of data for StRoot
  virtual void        SetTable(char* data, int nrows, int* idList=0);
  virtual void        AddRows(char* data, int nrows);
  //  virtual void        AddRowsAt(int rowNumber, char* data, int nrows);
  virtual int         GetNRows() const;
  virtual void        setRowNumber(int row=0);
  virtual bool        hasData() const;
  virtual void*       getDataValue(const char* name,int rowNumber=0);

  // memory management for query by where clause
  virtual int         getRowLimit() const;
  virtual void        setRowLimit(int nrows);
  virtual void        addNRows(int newRows);

  ////--> addNElements must be called after either addNRows or resizeElementID
  virtual void        addNElements(int* elements, int newRows); 
  virtual void        resizeNumRows(int nrows);
  virtual void        resizeElementID(int numRows);

  // methods for reading & writing to Db & to file
  virtual void StreamAccessor(typeAcceptor* accept, bool isReading);
  virtual void dbStreamer(typeAcceptor* accept, bool isReading);
  virtual void StreamAccessor(StDbBufferI* buff, bool isReading);
  virtual void dbStreamer(StDbBufferI* buff, bool isReading);
  virtual void dbStreamerWrite(StDbBufferI* buff);
  virtual void dbTableStreamer(StDbBufferI* buff, const char* name, bool isReading);
#ifdef __ROOT__
 ClassDef(StDbTable,0)
#endif
};

inline bool  StDbTable::IsTable() const { return true; };
inline unsigned int StDbTable::getTableSize() const { 
if(mhasDescriptor && !mdescriptor->getTrowSize()) return mdescriptor->getTotalSizeInBytes();
if(mhasDescriptor) return mdescriptor->getTrowSize();
return 0;
}
inline char* StDbTable::printCstructName() { return mstructName; }
inline char* StDbTable::printDataTable() { return mdataTable; }
inline char* StDbTable::printFlavor() { return mflavor; }
inline bool StDbTable::defaultFlavor() const {return mdefaultFlavor;}
inline void StDbTable::setProdTime(unsigned int ptime) { mprodTime=ptime; }
inline unsigned int StDbTable::getProdTime() { return mprodTime; }
inline unsigned int StDbTable::getEndTime() const {return mendTime.munixTime; }
inline char* StDbTable::getEndDateTime() { return mendTime.mdateTime; }
inline void StDbTable::setEndTime(unsigned int time){ mendTime.munixTime=time;}
inline void StDbTable::setEndTime(const char* time){mendTime.setDateTime(time);}
inline unsigned int StDbTable::getBeginTime() const {return mbeginTime.munixTime; }
inline char* StDbTable::getBeginDateTime() {return mbeginTime.mdateTime; }
inline void StDbTable::setBeginTime(unsigned int time){mbeginTime.munixTime = time; }
inline void StDbTable::setBeginTime(const char* time){ mbeginTime.setDateTime(time); }
inline unsigned int StDbTable::getEndStoreTime() const { return mendStoreTime.munixTime; }
inline char* StDbTable::getEndStoreDateTime() { return mendStoreTime.mdateTime; }
inline void StDbTable::setEndStoreTime(unsigned int time) {mendStoreTime.munixTime = time; }
inline void StDbTable::setEndStoreTime(const char* time){ mendStoreTime.setDateTime(time); }
inline int* StDbTable::getElementID(int& nrows) { nrows = mrows; return melementID; }

inline int StDbTable::getRowID(int rowNumber) const { 
  if(rowNumber<mrows)return melementID[rowNumber];
  return 0;
}
inline unsigned int* StDbTable::getTimeValues(){ return mtimeVals; }
inline void          StDbTable::setTimeValues(unsigned int* timeValues){ 
  if(mtimeVals) delete [] mtimeVals;
  mtimeVals=timeValues;
}
inline unsigned int  StDbTable::getMaxTime() {
  unsigned int retVal=0;
  for(int i=0; i<mrows;i++)if(mtimeVals[i]>retVal)retVal=mtimeVals[i];
  return retVal;
}
inline char* StDbTable::printElementName() { return melementName; }
inline int StDbTable::getSchemaID() const { return mschemaID; }
inline void StDbTable::setSchemaID(int id) {mschemaID = id; }
inline StTableDescriptorI* StDbTable::getDescriptor() { return mdescriptor; }
inline bool StDbTable::hasDescriptor() const { return mhasDescriptor; }
inline StDbTable* StDbTable::Clone() {return (new StDbTable(*this));}
inline int StDbTable::GetNRows() const { return mrows; }
inline void StDbTable::setRowLimit(int nrows) { mrowsRequested=nrows; }; 
inline int  StDbTable::getRowLimit() const { return mrowsRequested; }; 
inline void StDbTable::setRowNumber(int row){if(row < mrows)mrowNumber = row;}
inline bool StDbTable::hasData() const { return mhasData; };
inline void StDbTable::setBinary(bool abinary) { mIsBinary=abinary; }
inline void StDbTable::setBaseLine(bool baseLine)  { mIsBaseLine=baseLine; }
inline void StDbTable::setIndexed(bool indexed)  { mIsIndexed=indexed; }
inline bool StDbTable::IsBinary() const { return mIsBinary; }
inline bool StDbTable::IsBaseLine() const { return mIsBaseLine; }
inline bool StDbTable::IsIndexed() const { return mIsIndexed; }

#endif
