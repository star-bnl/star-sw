/***************************************************************************
 *
 * $Id: StDbTable.cc,v 1.45 2016/05/25 20:40:01 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:   Class that holds data, descriptor, & db-address 
 *                & performs streamer of db-data into data-memory
 *
 ***************************************************************************
 *
 * $Log: StDbTable.cc,v $
 * Revision 1.45  2016/05/25 20:40:01  dmitry
 * coverity - reverse_inull
 *
 * Revision 1.44  2015/06/23 20:21:12  dmitry
 * char type: null-terminator assignment fixed
 *
 * Revision 1.43  2012/12/12 21:59:19  fisyak
 * Add check for HAVE_CLOCK_GETTIME flag and for APPLE
 *
 * Revision 1.42  2009/11/10 20:24:45  fisyak
 * Use SafeDelete
 *
 * Revision 1.41  2009/09/10 18:06:08  dmitry
 * struct alignment fix, does not rely on fixed 4 byte cap anymore - runtime align calculation is now in use
 *
 * Revision 1.40  2007/08/20 18:21:30  deph
 * New Version of Load Balancer
 *
 * Revision 1.39  2007/05/16 22:48:10  deph
 * Replaced cerr with LOG_ERROR <<endm; for logger
 *
 * Revision 1.38  2005/09/07 22:03:04  deph
 * update to correct padding issue for pacted tables
 *
 * Revision 1.37  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.36  2003/09/16 22:44:17  porter
 * got rid of all ostrstream objects; replaced with StString+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.35  2003/09/02 17:57:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.34  2003/04/11 22:47:36  porter
 * Added a fast multi-row write model specifically needed by the daqEventTag
 * writer. Speed increased from about 100Hz to ~3000Hz.  It is only invoked if
 * the table is marked as Non-Indexed (daqTags & scalers). For non-indexed tables
 * which include binary stored data (we don't have any yet), the fast writer  has
 * to invoke a slower buffer so that the rates are a bit slower (~500Hz at 50 rows/insert).
 *
 * Revision 1.33  2003/02/12 22:12:45  porter
 * moved warning message about null columns (checked in 2 days ago) from the
 * depths of the mysql coding into the StDbTable code. This suppresses confusing
 * warnings from tables that have had elements removed but their storage columns
 * still exist in the database.
 *
 * Revision 1.32  2003/01/10 04:19:20  porter
 * added feature of getting timestamp list (but no data) for a table.
 * fixed 2 features sometimes used in online in query-by-whereclause.
 * removed a stray 'cout' in a routine that is rarely accessed
 *
 * Revision 1.31  2002/01/30 15:40:48  porter
 * changed limits on flavor tag & made defaults retrieving more readable
 *
 * Revision 1.30  2001/12/21 04:54:46  porter
 * sped up table definition for emc and changed some ostrstream usage for
 * insure tests
 *
 * Revision 1.29  2001/10/26 20:59:46  porter
 * fixed new endtime flag from previous checkin. made StDataBaseI available
 * at root-cli.
 *
 * Revision 1.28  2001/10/24 04:05:20  porter
 * added long long type to I/O and got rid of obsolete dataIndex table
 *
 * Revision 1.27  2001/08/02 17:37:20  porter
 * fixed problem in fetch by where-clause used in online in StDbSql.cc.
 * also got rid of warning comparing unsigned int to int.
 *
 * Revision 1.26  2001/07/13 22:53:26  porter
 * last night's schema-fix was in a switch-case ... I missed one & put it in today
 *
 * Revision 1.25  2001/07/13 02:28:15  porter
 * fix problem in schema evolution for array size changes
 *
 * Revision 1.24  2001/04/23 19:24:32  porter
 * fixed row limit & initial buffer contents for query by where clause
 *
 * Revision 1.23  2001/02/09 23:06:25  porter
 * replaced ostrstream into a buffer with ostrstream creating the
 * buffer. The former somehow clashed on Solaris with CC5 iostream (current .dev)
 *
 * Revision 1.22  2001/02/08 23:23:56  porter
 * fixed initialization of schemaID in table & fixed some warnings when
 * compiled with NODEBUG
 *
 * Revision 1.21  2001/01/22 18:38:00  porter
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
 * Revision 1.20  2000/08/15 22:51:52  porter
 * Added Root2DB class from Masashi Kaneta
 * + made code more robust against requesting data from non-existent databases
 *
 * Revision 1.19  2000/06/30 01:57:02  porter
 * fixed a delete bug & small memory leak found by Akio via Insure++ ,
 * updated SetTable() method for containing idList, corrected enumeration
 * map to rhic domain for Conditions_rhic database
 *
 * Revision 1.18  2000/06/02 13:37:37  porter
 * built up list of minor changes:
 *  - made buffer more robust for certain null inputs
 *  - fixed small leak in StDbTables & restructure call to createMemory
 *  - added dbRhic as a database domain in StDbDefs
 *  - added setUser() in StDbManager
 *  - added more diagnostic printouts in mysqlAccessor.cc
 *
 * Revision 1.17  2000/05/10 21:39:02  porter
 * fixed delete[] bug in reading from table where input schema includes fields that
 * are not in the database by checking buffer status for reads
 *
 * Revision 1.16  2000/04/25 18:26:03  porter
 * added flavor & production time as settable query fields in
 * table &/or node. Associated SQL updated in mysqlAccessor.
 * Flavor key supports "+" as an OR symbol.
 *
 * Revision 1.15  2000/03/28 17:03:19  porter
 * Several upgrades:
 * 1. configuration by timestamp for Conditions
 * 2. query by whereClause made more systematic
 * 3. conflict between db-stored comments & number lists resolved
 * 4. ensure endtime is correct for certain query falures
 * 5. dbstl.h->handles ObjectSpace & RogueWave difference (Online vs Offline)
 *
 * Revision 1.14  2000/02/15 20:27:44  porter
 * Some updates to writing to the database(s) via an ensemble (should
 * not affect read methods & haven't in my tests.
 *  - closeAllConnections(node) & closeConnection(table) method to mgr.
 *  - 'NullEntry' version to write, with setStoreMode in table;
 *  -  updated both StDbTable's & StDbTableDescriptor's copy-constructor
 *
 * Revision 1.13  2000/01/27 05:54:34  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
 * Revision 1.12  2000/01/19 20:20:07  porter
 * - finished transaction model needed by online
 * - fixed CC5 compile problem in StDbNodeInfo.cc
 * - replace TableIter class by StDbTableIter to prevent name problems
 *
 * Revision 1.11  2000/01/10 20:37:54  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.10  1999/12/07 21:25:25  porter
 * some fixes for linux warnings
 *
 * Revision 1.9  1999/12/03 19:01:59  porter
 * modified descriptor to accept tableDescriptor once this St_base object
 * has been updated to have longer name lengths.
 *
 * Revision 1.8  1999/11/29 21:40:08  fisyak
 * Add cast to HP
 *
 * Revision 1.7  1999/11/19 21:58:06  porter
 * added method to return "malloc'd" version of table instead of new
 * so that delete of St_Table class i done correctly
 *
 * $Log: StDbTable.cc,v $
 * Revision 1.45  2016/05/25 20:40:01  dmitry
 * coverity - reverse_inull
 *
 * Revision 1.44  2015/06/23 20:21:12  dmitry
 * char type: null-terminator assignment fixed
 *
 * Revision 1.43  2012/12/12 21:59:19  fisyak
 * Add check for HAVE_CLOCK_GETTIME flag and for APPLE
 *
 * Revision 1.42  2009/11/10 20:24:45  fisyak
 * Use SafeDelete
 *
 * Revision 1.41  2009/09/10 18:06:08  dmitry
 * struct alignment fix, does not rely on fixed 4 byte cap anymore - runtime align calculation is now in use
 *
 * Revision 1.40  2007/08/20 18:21:30  deph
 * New Version of Load Balancer
 *
 * Revision 1.39  2007/05/16 22:48:10  deph
 * Replaced cerr with LOG_ERROR <<endm; for logger
 *
 * Revision 1.38  2005/09/07 22:03:04  deph
 * update to correct padding issue for pacted tables
 *
 * Revision 1.37  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.36  2003/09/16 22:44:17  porter
 * got rid of all ostrstream objects; replaced with StString+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.35  2003/09/02 17:57:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.34  2003/04/11 22:47:36  porter
 * Added a fast multi-row write model specifically needed by the daqEventTag
 * writer. Speed increased from about 100Hz to ~3000Hz.  It is only invoked if
 * the table is marked as Non-Indexed (daqTags & scalers). For non-indexed tables
 * which include binary stored data (we don't have any yet), the fast writer  has
 * to invoke a slower buffer so that the rates are a bit slower (~500Hz at 50 rows/insert).
 *
 * Revision 1.33  2003/02/12 22:12:45  porter
 * moved warning message about null columns (checked in 2 days ago) from the
 * depths of the mysql coding into the StDbTable code. This suppresses confusing
 * warnings from tables that have had elements removed but their storage columns
 * still exist in the database.
 *
 * Revision 1.32  2003/01/10 04:19:20  porter
 * added feature of getting timestamp list (but no data) for a table.
 * fixed 2 features sometimes used in online in query-by-whereclause.
 * removed a stray 'cout' in a routine that is rarely accessed
 *
 * Revision 1.31  2002/01/30 15:40:48  porter
 * changed limits on flavor tag & made defaults retrieving more readable
 *
 * Revision 1.30  2001/12/21 04:54:46  porter
 * sped up table definition for emc and changed some ostrstream usage for
 * insure tests
 *
 * Revision 1.29  2001/10/26 20:59:46  porter
 * fixed new endtime flag from previous checkin. made StDataBaseI available
 * at root-cli.
 *
 * Revision 1.28  2001/10/24 04:05:20  porter
 * added long long type to I/O and got rid of obsolete dataIndex table
 *
 * Revision 1.27  2001/08/02 17:37:20  porter
 * fixed problem in fetch by where-clause used in online in StDbSql.cc.
 * also got rid of warning comparing unsigned int to int.
 *
 * Revision 1.26  2001/07/13 22:53:26  porter
 * last night's schema-fix was in a switch-case ... I missed one & put it in today
 *
 * Revision 1.25  2001/07/13 02:28:15  porter
 * fix problem in schema evolution for array size changes
 *
 * Revision 1.24  2001/04/23 19:24:32  porter
 * fixed row limit & initial buffer contents for query by where clause
 *
 * Revision 1.23  2001/02/09 23:06:25  porter
 * replaced ostrstream into a buffer with ostrstream creating the
 * buffer. The former somehow clashed on Solaris with CC5 iostream (current .dev)
 *
 * Revision 1.22  2001/02/08 23:23:56  porter
 * fixed initialization of schemaID in table & fixed some warnings when
 * compiled with NODEBUG
 *
 * Revision 1.21  2001/01/22 18:38:00  porter
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
 * Revision 1.20  2000/08/15 22:51:52  porter
 * Added Root2DB class from Masashi Kaneta
 * + made code more robust against requesting data from non-existent databases
 *
 * Revision 1.19  2000/06/30 01:57:02  porter
 * fixed a delete bug & small memory leak found by Akio via Insure++ ,
 * updated SetTable() method for containing idList, corrected enumeration
 * map to rhic domain for Conditions_rhic database
 *
 * Revision 1.18  2000/06/02 13:37:37  porter
 * built up list of minor changes:
 *  - made buffer more robust for certain null inputs
 *  - fixed small leak in StDbTables & restructure call to createMemory
 *  - added dbRhic as a database domain in StDbDefs
 *  - added setUser() in StDbManager
 *  - added more diagnostic printouts in mysqlAccessor.cc
 *
 * Revision 1.17  2000/05/10 21:39:02  porter
 * fixed delete[] bug in reading from table where input schema includes fields that
 * are not in the database by checking buffer status for reads
 *
 * Revision 1.16  2000/04/25 18:26:03  porter
 * added flavor & production time as settable query fields in
 * table &/or node. Associated SQL updated in mysqlAccessor.
 * Flavor key supports "+" as an OR symbol.
 *
 * Revision 1.15  2000/03/28 17:03:19  porter
 * Several upgrades:
 * 1. configuration by timestamp for Conditions
 * 2. query by whereClause made more systematic
 * 3. conflict between db-stored comments & number lists resolved
 * 4. ensure endtime is correct for certain query falures
 * 5. dbstl.h->handles ObjectSpace & RogueWave difference (Online vs Offline)
 *
 * Revision 1.14  2000/02/15 20:27:44  porter
 * Some updates to writing to the database(s) via an ensemble (should
 * not affect read methods & haven't in my tests.
 *  - closeAllConnections(node) & closeConnection(table) method to mgr.
 *  - 'NullEntry' version to write, with setStoreMode in table;
 *  -  updated both StDbTable's & StDbTableDescriptor's copy-constructor
 *
 * Revision 1.13  2000/01/27 05:54:34  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
 * Revision 1.12  2000/01/19 20:20:07  porter
 * - finished transaction model needed by online
 * - fixed CC5 compile problem in StDbNodeInfo.cc
 * - replace TableIter class by StDbTableIter to prevent name problems
 *
 * Revision 1.11  2000/01/10 20:37:54  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.10  1999/12/07 21:25:25  porter
 * some fixes for linux warnings
 *
 * Revision 1.9  1999/12/03 19:01:59  porter
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
#include "StDbTable.h"
#include "StDbBuffer.h"
#include "typeAcceptor.hh"
#include "StTableDescriptorI.h"
#include <stdlib.h>
#ifndef __STDB_STANDALONE__
#include "StMessMgr.h"
#else
#define LOG_DEBUG cout
#define LOG_INFO cout
#define LOG_WARN cout
#define LOG_ERROR cerr
#define LOG_FATAL cerr
#define LOG_QA cout
#define endm "\n"
#endif

#include "StDbDefaults.hh"
#include "StDbManager.hh"
#include <string.h>
#include "stdb_streams.h"
#ifndef __APPLE__
#include <malloc.h>
#endif
#ifdef __ROOT__
ClassImp(StDbTable)
#endif

#define __CLASS__ "StDbTable"

///////////////////////////////////////////////////////////////
StDbTable::StDbTable(const char* tableName): StDbNode(tableName) { init();};

StDbTable::StDbTable(const char* tableName, int schemaID): StDbNode(tableName) {  init(); mschemaID=schemaID; }


StDbTable::StDbTable(StDbTable& table): StDbNode(table) {

 init();
 mflavor=table.getFlavor();
 mdefaultFlavor = table.defaultFlavor();
 mprodTime = table.getProdTime();

 mschemaID = table.getSchemaID();
 mrows = table.GetNRows();
 mhasDescriptor=table.hasDescriptor();
 mdescriptor=table.getDescriptorCpy();
 
 mbeginTime.setDateTime(table.getBeginDateTime());
 mbeginTime.setUnixTime(table.getBeginTime());
 mendTime.setDateTime(table.getEndDateTime());
 mendTime.setUnixTime(table.getEndTime());

 char* tmp = table.GetTable();
 if(mrows==0) mrows = 1; 
 if(tmp) {
   unsigned int size =  mrows*table.getTableSize();
   mdata = new char[size];
   memcpy(mdata,tmp,size);
   mhasData = true;
 }

 setCstructName(table.printCstructName());
 setDataTable(table.printDataTable());

}

void StDbTable::init() {
 mflavor            = 0;
 mstructName        = 0; 
 melementName       = 0;
 mdataTable         = 0; 
 melementID         = 0;
 mhasDescriptor     = false;
 mdescriptor        = 0; 
 mdata              = 0; 
 mhasData           = false; 
 mrowsRequested     = 0;
 mtimeVals          = 0;
 mendTime.munixTime = 0;
 mrows              = 0;
 mrowNumber         = 0;
 mprodTime          = StDbDefaults::Instance()->getProdTime();
 setDefaultFlavor();
 mschemaID = 0;

}

////////////////////////////////////////////////////////////////////
void StDbTable::setNodeInfo(StDbNode* node){

    mdbType=node->getDbType();
    mdbDomain=node->getDbDomain();
    setDbName(node->printDbName());
    setVersion(node->printVersion());
    setNodeType(node->printNodeType());
    mnodeID=node->getNodeID();
};

/////////////////////////////////////////////////////////////////////
void StDbTable::setCstructName(const char* name){ mstructName=mstrDup(name); }

/////////////////////////////////////////////////////////////////////
char* StDbTable::getCstructName() { return mstrDup(mstructName); }

/////////////////////////////////////////////////////////////////////
void StDbTable::setDataTable(const char* name){ mdataTable=mstrDup(name); }

/////////////////////////////////////////////////////////////////////
char* StDbTable::getDataTable() { return mstrDup(mdataTable); }

/////////////////////////////////////////////////////////////////////
void
StDbTable::setDefaultFlavor(){ setFlavor(StDbDefaults::Instance()->printFlavor());
}

/////////////////////////////////////////////////////////////////////
void  StDbTable::setFlavor(const char* flavor) { 
  if(!flavor) return;
  if(mflavor) delete [] mflavor;
  mflavor=new char[strlen(flavor)+1];
  strcpy(mflavor,flavor);  
  mdefaultFlavor = StDbDefaults::Instance()->IsDefaultFlavor(mflavor);
}

char* StDbTable::getFlavor() { return mstrDup(mflavor); }


/////////////////////////////////////////////////////////////////////
void
StDbTable::addWrittenRows(int* dataID, int numRows, bool canRollBack){

  for(int i=0; i<numRows; i++) mstoredData.addWrittenRow(dataID[i]);
  if(canRollBack)mcanRollBack=true;

}

int*  
StDbTable::getWrittenRows(int& nRows){ 
       return mstoredData.getDataIDs(nRows); 
}
void StDbTable::commitData()         { mstoredData.commit(); }
void StDbTable::clearStoreInfo()     { mstoredData.resetStoreInfo(); }

/////////////////////////////////////////////////////////////////////
StTableDescriptorI*
StDbTable::getDescriptorCpy() const { return mdescriptor->getCpy(); }

/////////////////////////////////////////////////////////////////////
void 
StDbTable::setDescriptor(StTableDescriptorI* descriptor){ 

 if(mdescriptor) delete mdescriptor;
 mdescriptor=descriptor;
 mhasDescriptor=true;
 
 //checkDescriptor();

};

/////////////////////////////////////////////////////////////////////
char* StDbTable::GetTable() { if(!mdata)createMemory(); return mdata;};

/////////////////////////////////////////////////////////////////////
void* 
StDbTable::GetTableCpy() { 

if(!mdata)return (void*)GetTable();

 int len =  mrows*getTableSize();
 char* c = (char*)calloc(mrows,getTableSize());
 memcpy(c,mdata,len);

return (void*)c;
};

/////////////////////////////////////////////////////////////////////
void 
StDbTable::SetTable(char* c, int nrows, int* idList) { 

if(mdata){
  delete [] mdata; 
  mdata = 0;
}
 if(!idList){
   createMemory(nrows);
 } else {
   setElementID(idList,nrows); // createMemory is called here
 }
int len = nrows*getTableSize();
memcpy(mdata,c,len);
mhasData=true;

}

/////////////////////////////////////////////////////////////////////
void 
StDbTable::AddRows(char* c, int nrows) { 

char* tmpData = duplicateData();
int len1 = mrows*getTableSize();
int len2 = nrows*getTableSize();

int newRows = nrows+mrows;
if(mdata){
   delete [] mdata;
   mdata = 0;
 }

createMemory(newRows);

char* ptr= &mdata[0];
memcpy(mdata,tmpData,len1);
ptr+=len1;
memcpy(ptr,c,len2);

delete [] tmpData;
mhasData=true;

}

/////////////////////////////////////////////////////////////////////
void*
StDbTable::getDataValue(const char* name, int rowNumber){

  void* retVal=0;
  int saveRowNum=mrowNumber;
  mrowNumber=rowNumber;
  int max = mdescriptor->getNumElements();
  char* ename=0;
  StTypeE type;
  unsigned int length;
  char * ptr;

  for(int i=0;i<max;i++){ 
    getElementSpecs(i,ptr,ename,length,type);
	if ( ename ) {
	    if ( strcmp(name,ename) == 0 ) break;
    	delete [] ename;
 		ename=0;
	}
  }

  mrowNumber=saveRowNum;
  if(!ename) return retVal;

  delete [] ename;
  return (void*)ptr;
}

/////////////////////////////////////////////////////////////////////
char*
StDbTable::duplicateData() { 

 char* dup=0;
 int len1 = mrows*getTableSize();
 if(len1 !=0){
  dup=new char[len1];
  memcpy(dup,mdata,len1);
 }
return dup;
}

/////////////////////////////////////////////////////////////////////

bool
StDbTable::createMemory(int nrows) { 
 mrows = nrows;
 bool retVal = true;
 if(mrows==0) {
   if(mdata)delete [] mdata; 
   mdata=0;
   return retVal;
 }

// mdescriptor->getNumElements();
//if (mdescriptor) {
//cout <<"**************CHECKIT************"<<endl;
//} 
 if(mdescriptor && mdescriptor->getNumElements()>0){
    //     if(mrows==0) mrows = 1;
      //int len = mrows*mdescriptor->getTotalSizeInBytes();
     int len;
     if (!mdescriptor->getTrowSize()){
       len =  mrows*mdescriptor->getTotalSizeInBytes();
     }else{
      len = mrows*mdescriptor->getTrowSize();
     }
      if(len>0){
      if(mdata)delete [] mdata;
      mdata=new char[len];
      memset(mdata,0,len);
      int max = mdescriptor->getNumElements();
      char* name;
      StTypeE type;
      unsigned int length;
      char * ptr;
      for(int i=0; i<max;i++){
        getElementSpecs(i,ptr,name,length,type);
        if (type==Stchar) { *ptr='\0'; }
        delete [] name;
      }
     }
  } else {
    if(!mname){mname=mstrDup("Unknown");}
    retVal = false;
  }

return retVal;
}

/////////////////////////////////////////////////////////////////////
bool
StDbTable::createMemory() {
  if(mdata)return true;
  if(mrows==0) mrows=1;
  return createMemory(mrows);
}

//////////////////////////////////////////////////////////////////////

char*
StDbTable::getElementName() { return mstrDup(melementName); };

void
StDbTable::setElementName(const char* name) { melementName=mstrDup(name);};

void 
StDbTable::setElementID(int* elements, int nrows) { 

   createMemory(nrows);
   // set up & fill char* will element list
   if(melementID) delete [] melementID;
   if(nrows==0){
     melementID=0;
     return;
   }
   melementID = new int[nrows];
   memcpy(melementID, elements, nrows*sizeof(int));
}

//////////////////////////////////////////////////////////////////////
void StDbTable::resizeNumRows(int nrows){
  // if only some rows are returned, this is called to
  // compress memory

  //unsigned int rowsize=mdescriptor->getTotalSizeInBytes();
  unsigned int rowsize;
  if (!mdescriptor->getTrowSize()) {
     rowsize=mdescriptor->getTotalSizeInBytes();
  }else{
      rowsize=mdescriptor->getTrowSize();
   }
  unsigned int len = mrows*rowsize;
  unsigned int newlen = nrows*rowsize;

  if(mdata){
    char* oldData=new char[len];
    memcpy(oldData,mdata,len);
    delete [] mdata;
    mdata = new char[newlen];
    if(newlen<=len){
      memcpy(mdata,oldData,newlen);
    } else {
      memcpy(mdata,oldData,len);
    }
    delete [] oldData;
  }

  mrows=nrows;
  return;
}


//////////////////////////////////////////////////////////////////////
void
StDbTable::addNRows(int numRows){

  if(!mdescriptor) return;

  int newRows = numRows+mrows;
  //unsigned int rowsize=mdescriptor->getTotalSizeInBytes();
  unsigned int rowsize;
  if(!mdescriptor->getTrowSize()) {
     rowsize=mdescriptor->getTotalSizeInBytes();
  }else{
      rowsize=mdescriptor->getTrowSize();
   }
  unsigned int len = newRows*rowsize;
  char* newData = new char[len]; 
  memset(newData,0,len);
  if(mdata)memcpy(newData,mdata,mrows*rowsize);
  char* p1 = newData;
  p1+=mrows*rowsize;
  memset(p1,0,numRows*rowsize);
  if(mdata)delete [] mdata;
  mdata=newData;

  resizeElementID(newRows);
};

//////////////////////////////////////////////////////////////////////
void 
StDbTable::resizeElementID(int numRows){

  int * newElements=new int[numRows];
  if(melementID) {
    memcpy(newElements,melementID,mrows*sizeof(int));
    delete [] melementID;
  }
  melementID=newElements;
  mrows = numRows;

};

//////////////////////////////////////////////////////////////////////
void
StDbTable::addNElements(int* elements, int newRows){


  if(!melementID) return;

  int i,j,k;
  k=mrows-newRows;
  if(k<0)return;
  j=0;
  for(i=k;i<mrows;i++){
    melementID[i]=elements[j]; 
    j++;
  }

}  
  
//////////////////////////////////////////////////////////////////////
void
StDbTable::StreamAccessor(typeAcceptor* accept, bool isReading){

   int len = 1;
   accept->pass((char*)"schemaID",mschemaID,len);

   if(isReading){
     if(mbeginTime.mdateTime) delete [] mbeginTime.mdateTime;
     if(mversion)delete [] mversion;
     if(melementID)delete [] melementID;
   } else {
     if(!melementID){
       melementID = new int[mrows];
       for(int i=0;i<mrows;i++)melementID[i]=i;
     }
   }
   accept->pass((char*)"beginTime",mbeginTime.mdateTime,len);
   accept->pass((char*)"version",mversion,len);
   accept->pass((char*)"elementID",melementID, mrows);
}

//////////////////////////////////////////////////////////////////////
void
StDbTable::StreamAccessor(StDbBufferI* buff, bool isReading){

  bool ClientMode;
  if(!(ClientMode=buff->IsClientMode()))buff->SetClientMode();

  int rowID;
  if(!melementID){
     melementID = new int[mrows];
     for(int i=0;i<mrows;i++)melementID[i]=i;
  }

  if(isReading){
    buff->ReadScalar(rowID,"elementID");
    melementID[mrowNumber]=rowID;

    if(mrowNumber==0){
     buff->ReadScalar(mschemaID,"schemaID");
     if(mversion) delete [] mversion;
     buff->ReadScalar(mversion,"version");
    } else {
      unsigned int bTime;// , eTime;
      buff->ReadScalar(bTime,"beginTime"); 
      if(bTime>mbeginTime.munixTime)mbeginTime.munixTime=bTime;
    }

  } else {

   buff->WriteScalar(mschemaID,"schemaID");
   buff->WriteScalar(mbeginTime.munixTime,"beginTime");
   if(mversion)buff->WriteScalar(mversion,"version");
   rowID = melementID[mrowNumber];   
   buff->WriteScalar(rowID,"elementID");
  }

 if(!ClientMode)buff->SetStorageMode();  // reset to StorageMode

}

///////////////////////////////////////////////////////////////////////
void
StDbTable::getElementSpecs(int elementNum, char*& c, char*& name, unsigned int& length,StTypeE& type){

     int tRow = mdescriptor->getTrowSize();
    unsigned int tSize=mdescriptor->getTotalSizeInBytes();
    unsigned int rowIndex;
    if (!tRow) {
        rowIndex = ((unsigned int)mrowNumber)*tSize;
     }else{
        rowIndex = ((unsigned int)mrowNumber)*tRow;
     }
    int i = elementNum;
    c = &mdata[rowIndex];
    int current = mdescriptor->getElementOffset(i);
    c += current; // for(int k=0;k<current;k++)c++;
    name   = mdescriptor->getElementName(i);
    length = mdescriptor->getElementLength(i);;
    type   = mdescriptor->getElementType(i);

return;
}

///////////////////////////////////////////////////////////////////////
void
StDbTable::dbStreamer(StDbBufferI* buff, bool isReading){

int max = mdescriptor->getNumElements();
char* name;
StTypeE type;
unsigned int length;
char* ptr;

  bool ClientMode;
  if(!(ClientMode=buff->IsClientMode()))buff->SetClientMode();

 if(createMemory() && mrowNumber < mrows){

 for(int i=0; i<max; i++){
    getElementSpecs(i,ptr,name,length,type);
    if(isReading){
     ReadElement(ptr,name,length,type,(StDbBuffer*)buff);
     } else {
     WriteElement(ptr,name,length,type,(StDbBuffer*)buff);
    }       
   delete [] name;
 }

  mrowNumber++;
  if(isReading)mhasData=true;

 } else {
   LOG_ERROR << "dbStreamer:: more rows delivered than allocated " << endm;
 }
 if(!ClientMode)buff->SetStorageMode();  // reset to StorageMode
}


///////////////////////////////////////////////////////////////////////
void
StDbTable::dbStreamerWrite(StDbBufferI* buff){

int max = mdescriptor->getNumElements();
char* name;
StTypeE type;
unsigned int length;
char* ptr;

//  bool ClientMode;
//  if(!(ClientMode=buff->IsClientMode()))buff->SetClientMode();

// if(createMemory() && mrowNumber < mrows){

 if(mrowNumber<mrows){
   for(int i=0; i<max; i++){
     getElementSpecs(i,ptr,name,length,type);
     WriteElement(ptr,name,length,type,(StDbBuffer*)buff);
     delete [] name;       
   }
  mrowNumber++;
 }
}

///////////////////////////////////////////////////////////////////////

void
StDbTable::dbTableStreamer(StDbBufferI* buff, const char* name, bool isReading){

int max = mdescriptor->getNumElements();
//int size = mdescriptor->getTotalSizeInBytes();
StTypeE type = mdescriptor->getElementType(0);
unsigned int length = (unsigned int) mrows*max;

char* ptr;

  bool ClientMode;
  if(!(ClientMode=buff->IsClientMode()))buff->SetClientMode();

 if(createMemory() && mrowNumber < mrows){

   ptr = &mdata[0];
   //    getElementSpecs(i,ptr,name,length,type);
   if(isReading){
     ReadElement(ptr,(char *) name,length,type,(StDbBuffer*)buff);
   } else {
     WriteElement(ptr,(char *) name,length,type,(StDbBuffer*)buff);
   }       
  mrowNumber=mrows;
  if(isReading) mhasData=true;
 }

 if(!ClientMode)buff->SetStorageMode();  // reset to StorageMode
}


///////////////////////////////////////////////////////////////////////

void
StDbTable::dbStreamer(typeAcceptor* accept, bool isReading){

int max = mdescriptor->getNumElements();
char* name;
StTypeE type;
unsigned int length;
char* ptr;

 if(createMemory() && mrowNumber < mrows){

   if(isReading){
     for(int i=0; i<max; i++){
      getElementSpecs(i,ptr,name,length,type);
      PassInElement(ptr,name,length,type,accept);
      delete [] name;
     }
     mhasData=true;
   } else {
     for(int i=0; i<max; i++){
      getElementSpecs(i,ptr,name,length,type);
      PassOutElement(ptr,name,length,type,accept);
      delete [] name;
     }
   }

 mrowNumber++;
 }

}

///////////////////////////////////////////////////////////////////////

void
StDbTable::ReadElement(char*& ptr, char* name, int len, StTypeE type, StDbBuffer* buff){

char* mchar; unsigned char* muchar; short* mshort; unsigned short* mushort; 
int* mint; unsigned int* muint; long* mlong; unsigned long* mulong; 
long long* mlonglong;
float* mfloat; double* mdouble;

 int blen; // length returned from db  ### use lesser of len & blen 
 
  switch (type) {
  case Stchar:
    {
        StString cn;
        cn<<name<<".text"; const char* commentName = (cn.str()).c_str();
        mchar = 0;
        if(!buff->ReadScalar(mchar,commentName))buff->ReadScalar(mchar,name);
        if(mchar){
             int len1=strlen(mchar);
             if(len>len1) len=len1;
             strncpy(ptr,mchar,len);
             delete [] mchar;
        } else {
             *ptr='\0';
             printNoDataReturned(name);
        }
    break;
    }
  case Stuchar:
    {
      if(buff->ReadArray(muchar,blen,name)){
     	if(len>blen)len=blen;
        memcpy(ptr,muchar,len*sizeof(unsigned char));
        delete [] muchar;
      } else { printNoDataReturned(name); }
    break;
    }
  case Stshort:
    {
      if(buff->ReadArray(mshort,blen,name)){
	if(len>blen)len=blen;
        memcpy(ptr,mshort,len*sizeof(short));
        delete [] mshort;
      } else { printNoDataReturned(name); }
    break;
    }
  case Stushort:
    {
      if(buff->ReadArray(mushort,blen,name)){
	if(len>blen)len=blen;
        memcpy(ptr,mushort,len*sizeof(unsigned short));
        delete [] mushort;
      } else { printNoDataReturned(name); }
    break;
    }
  case Stint:
    {
      if(buff->ReadArray(mint,blen,name)){
	if(len>blen)len=blen;
         memcpy(ptr,mint,len*sizeof(int));
         delete [] mint;
      } else { printNoDataReturned(name); }
    break;
    }
  case Stuint:
    {
      if(buff->ReadArray(muint,blen,name)){
	if(len>blen)len=blen;
       memcpy(ptr,muint,len*sizeof(unsigned int));
       delete [] muint;
      } else { printNoDataReturned(name); }
    break;
    }
  case Stlong:
    {
      if(buff->ReadArray(mlong,blen,name)){
	if(len>blen)len=blen;
       memcpy(ptr,mlong,len*sizeof(long));
       delete [] mlong;
      } else { printNoDataReturned(name); }
    break;
    }
  case Stulong:
    {
      if(buff->ReadArray(mulong,blen,name)){
	if(len>blen)len=blen;
       memcpy(ptr,mulong,len*sizeof(unsigned long));
       delete [] mulong;
      } else { printNoDataReturned(name); }
    break;
    }
  case Stlonglong:
    {
      if(buff->ReadArray(mlonglong,blen,name)){
	if(len>blen)len=blen;
       memcpy(ptr,mlonglong,len*sizeof(long long));
       delete [] mlonglong;
      } else { printNoDataReturned(name); }
    break;
    }
  case Stfloat:
    {
      if(buff->ReadArray(mfloat,blen,name)){
       if(len>blen)len=blen;
       memcpy(ptr,mfloat,len*sizeof(float));
       delete [] mfloat;
      } else { printNoDataReturned(name); }
    break;
    }
  case Stdouble:
    {
      if(buff->ReadArray(mdouble,blen,name)){
       if(len>blen)len=blen;
       memcpy(ptr,mdouble,len*sizeof(double));
       delete [] mdouble;
      } else { printNoDataReturned(name); }
    break;
    }
  case Stmaxtype:
    printNoDataReturned(name);
    break;
  }

}

///////////////////////////////////////////////////////////////////////

void
StDbTable::WriteElement(char* ptr, char* name, int len, StTypeE type, StDbBuffer* buff){

  switch (type) {
  case Stchar:
    {
    char* mchar = ptr;
    buff->WriteScalar(mchar,name);
    break;
    }
  case Stuchar:
    {
    unsigned char* muchar = (unsigned char*)ptr;
    buff->WriteArray(muchar,len,name);
    break;
    }
  case Stshort:
    {
    short* mshort = (short*) ptr;
    buff->WriteArray(mshort ,len,name);
    break;
    }
  case Stushort:
    {
    unsigned short* mushort = (unsigned short*) ptr;
    buff->WriteArray(mushort,len,name);
    break;
    }
  case Stint:
    {
    int* mint = (int*)ptr;
    buff->WriteArray(mint,len,name);
    break;
    }
  case Stuint:
    {
    unsigned int* muint = (unsigned int*) ptr;
    buff->WriteArray(muint,len,name);
    break;
    }
  case Stlong:
    {
    long* mlong = (long*) ptr;
    //if(len==1) cout << name << " = "<< *mlong << endl;
    buff->WriteArray(mlong,len,name);
    break;
    }
  case Stulong:
    {
    unsigned long* mulong = (unsigned long*) ptr;
    buff->WriteArray(mulong,len,name);
    break;
    }
  case Stlonglong:
    {
    long long* mlonglong = (long long*) ptr;
    buff->WriteArray(mlonglong,len,name);
    break;
    }
  case Stfloat:
    {
    float* mfloat = (float*) ptr;
    //if(len==1) cout << name << " = "<< *mfloat << endl;
    buff->WriteArray(mfloat,len,name);
    break;
    }
  case Stdouble:
    {
    double* mdouble = (double*) ptr;
    buff->WriteArray(mdouble,len,name);
    break;
    }
  case Stmaxtype:
    break;
  }

}

///////////////////////////////////////////////////////////////////////
void
StDbTable::PassInElement(char* ptr, char* name, int len, StTypeE type, typeAcceptor* accept){


  switch (type) {
  case Stchar:
    {
    char* data;
    accept->pass(name,data,len);
    memcpy(ptr,data,len);
    delete [] data;
    break;
    }
  case Stuchar:
    {
     unsigned char* data; 
     accept->pass(name,data,len);
     memcpy(ptr,data,len);
     delete [] data;
     break;
    }
  case Stshort:
    {
     short* data; 
     accept->pass(name,data,len);
     memcpy(ptr,data,len*sizeof(short));
     delete [] data;
     break;
    }
  case Stushort:
    {
     unsigned short* data; 
     accept->pass(name,data,len);
     memcpy(ptr,data,len*sizeof(short));
     delete [] data;
     break;
    }
  case Stint:
    {
     int* data; 
     accept->pass(name,data,len);
     memcpy(ptr,data,len*sizeof(int));
     delete [] data;
     break;
    }
  case Stuint:
    {
     unsigned int* data; 
     accept->pass(name,data,len);
     memcpy(ptr,data,len*sizeof(int));
     delete [] data;
     break;
    }
  case Stlong:
    {
     long* data; 
     accept->pass(name,data,len);
     memcpy(ptr,data,len*sizeof(long));
     delete [] data;
     break;
    }
  case Stulong:
    {
     unsigned long* data; 
     accept->pass(name,data,len);
     memcpy(ptr,data,len*sizeof(long));
     delete [] data;
     break;
    }
  case Stlonglong:
    {
     long long* data; 
     accept->pass(name,data,len);
     memcpy(ptr,data,len*sizeof(long long));
     delete [] data;
     break;
    }
  case Stfloat:
    {
     float* data; 
     accept->pass(name,data,len);
     memcpy(ptr,data,len*sizeof(float));
     delete [] data;
     break;
    }
  case Stdouble:
    {
     double* data; 
     accept->pass(name,data,len);
     memcpy(ptr,data,len*sizeof(double));
     delete [] data;
     break;
    }
  case Stmaxtype:
    // should never get here
    break;
  }
}

///////////////////////////////////////////////////////////////////////

void
StDbTable::PassOutElement(char* ptr, char* name, int len, StTypeE type, typeAcceptor* accept){

  switch (type) {
  case Stchar:
    {
    accept->pass(name,ptr,len);
    break;
    }
  case Stuchar:
    {
     unsigned char* muchar = (unsigned char*)ptr;
     accept->pass(name, muchar,len);
    break;
    }
  case Stshort:
    {
    short* mshort = (short*)ptr;
    if(len==1){
      accept->pass(name, *mshort ,len);
    } else {
      accept->pass(name,mshort,len);
    }
    break;
    }
  case Stushort:
    {
    unsigned short* mushort = (unsigned short*)ptr;
    if(len==1){
      accept->pass(name, *mushort ,len);
    } else {
      accept->pass(name,mushort,len);
    }
    break;
    }
  case Stint:
    {
    int* mint = (int*)ptr;
    if(len==1){
      accept->pass(name, *mint ,len);
    } else {
      accept->pass(name,mint,len);
    }
    break;
    }
  case Stuint:
    {
    unsigned int* muint = (unsigned int*)ptr;
    if(len==1){
      accept->pass(name, *muint ,len);
    } else {
      accept->pass(name,muint,len);
    }
    break;
    }
  case Stlong:
    {
    long* mlong = (long*)ptr;
    if(len==1){
      accept->pass(name, *mlong ,len);
    } else {
      accept->pass(name,mlong,len);
    }
    break;
    }
  case Stulong:
    {
    unsigned long* mulong = (unsigned long*)ptr;
    if(len==1){
      accept->pass(name, *mulong ,len);
    } else {
      accept->pass(name,mulong,len);
    }
    break;
    }
  case Stlonglong:
    {
    long long* mlonglong = (long long*)ptr;
    if(len==1){
      accept->pass(name, *mlonglong ,len);
    } else {
      accept->pass(name,mlonglong,len);
    }
    break;
    }
  case Stfloat:
    {
    float* mfloat = (float*)ptr;
    if(len==1){
      accept->pass(name, *mfloat ,len);
    } else {
      accept->pass(name,mfloat,len);
    }
    break;
    }
  case Stdouble:
    {
    double* mdouble = (double*)ptr;
    if(len==1){
      accept->pass(name, *mdouble ,len);
    } else {
      accept->pass(name,mdouble,len);
    }
    break;
    }
  case Stmaxtype:
    // should never get here
    break;
  }
}

/////////////////////////////////////////////////////////////////
void
StDbTable::checkDescriptor(){

int i = mdescriptor->getNumElements();
unsigned int size = mdescriptor->getTotalSizeInBytes();
 cout <<"Descriptor for Table = " << mname<<endl;
 cout <<" number of elements = "<<i<< " with size = " << size <<" TrowSize = " << mdescriptor->getTrowSize()<< endl;
 for(int k=0; k<i;k++){
   cout <<"Name = " << mdescriptor->getElementName(k);
   cout <<" size = " << mdescriptor->getElementSize(k);
   cout <<" offset = " <<mdescriptor->getElementOffset(k);
   cout <<" type = " <<(int)mdescriptor->getElementType(k) <<  endl;
 }
}


void StDbTable::printNoDataReturned(const char* elementName){

  StString emess;
  emess<<" No data return from table="<<printName()<<" column="<<elementName;
  StDbManager::Instance()->printInfo((emess.str()).c_str(),dbMWarn,__LINE__,__CLASS__,"ReadElement(ptr,name,len,type,buffer)");
}

#undef __CLASS__












