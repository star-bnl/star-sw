/***************************************************************************
 *
 * $Id: StDbTableDescriptor.cc,v 1.28 2009/09/28 19:14:10 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Class implement table-descriptor (memory/name of data-elements)
 *              this descriptor is loaded from database
 *
 ***************************************************************************
 *
 * $Log: StDbTableDescriptor.cc,v $
 * Revision 1.28  2009/09/28 19:14:10  dmitry
 * row size is not *static* anymore, tables have different row sizes
 *
 * Revision 1.27  2009/09/10 20:01:30  dmitry
 * removed redundant output
 *
 * Revision 1.26  2009/09/10 18:06:08  dmitry
 * struct alignment fix, does not rely on fixed 4 byte cap anymore - runtime align calculation is now in use
 *
 * Revision 1.25  2005/09/07 22:04:02  deph
 * update to correct padding issue for packed tables
 *
 * Revision 1.24  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.23  2003/10/28 20:58:31  perev
 *  Linux ==> __linux__
 *
 * Revision 1.22  2003/09/16 22:44:18  porter
 * got rid of all ostrstream objects; replaced with StString+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.21  2003/09/02 17:57:50  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.20  2002/01/30 15:40:48  porter
 * changed limits on flavor tag & made defaults retrieving more readable
 *
 * Revision 1.19  2001/12/05 17:16:35  porter
 * stand-alone make file no longer had "D__linux__" in compile but this is still needed
 * and returned. Also retrieve elementID list  in query by whereClause for plotting
 * many row instances.
 *
 * Revision 1.18  2001/10/24 04:05:20  porter
 * added long long type to I/O and got rid of obsolete dataIndex table
 *
 * Revision 1.17  2001/02/08 23:23:56  porter
 * fixed initialization of schemaID in table & fixed some warnings when
 * compiled with NODEBUG
 *
 * Revision 1.16  2001/01/22 18:38:00  porter
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
 * Revision 1.15  2000/03/28 17:03:19  porter
 * Several upgrades:
 * 1. configuration by timestamp for Conditions
 * 2. query by whereClause made more systematic
 * 3. conflict between db-stored comments & number lists resolved
 * 4. ensure endtime is correct for certain query falures
 * 5. dbstl.h->handles ObjectSpace & RogueWave difference (Online vs Offline)
 *
 * Revision 1.14  2000/03/06 17:11:49  porter
 * - WriteDb(table) returns true if no data is in table
 * - fixed memory leak introduced in 2/18/00 update.
 * - modified descriptor algorythm for OnlRunDescriptor.
 *
 * Revision 1.13  2000/03/01 20:56:16  porter
 * 3 items:
 *    1. activated reConnect for server timeouts
 *    2. activated connection sharing; better resource utilization but poorer
 *       logging
 *    3. made rollback method in mysqlAccessor more robust (affects writes only)
 *
 * Revision 1.12  2000/02/24 20:30:47  porter
 * fixed padding for uchar; beginTime in mysqlAccessor;
 * added rollback safety checkes in StDbManger
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
 * Revision 1.7  1999/12/07 21:25:25  porter
 * some fixes for __linux__ warnings
 *
 * Revision 1.6  1999/12/03 19:02:01  porter
 * modified descriptor to accept tableDescriptor once this St_base object
 * has been updated to have longer name lengths.
 *
 * Revision 1.5  1999/10/19 14:30:40  porter
 * modifications relevant to use with StDbBroker and future merging with
 * "params" database structure + some docs + suppressing diagnostics messages
 *
 * Revision 1.4  1999/09/30 02:06:10  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "StDbTableDescriptor.h"
#include <stdlib.h>
#include <math.h>
#include "stdb_streams.h"
#include <iostream>

// Alignment test struct
template <typename T>                                                                                                                                        
struct StCompilerAlignTest {                                                                                                                                 
   char m0;                                                                                                                                                  
      T m1;                                                                                                                                                     
};                                                                                                                                                           
                                                                                                                                                                   
#define st_alignof(TYPE)     (size_t)&(((StCompilerAlignTest<TYPE>*)0)->m1) 

/////////////////////////////////////////////////////////////////////////

StDbTableDescriptor::StDbTableDescriptor(){  init(); }

/////////////////////////////////////////////////////////////////////////

StDbTableDescriptor::StDbTableDescriptor(int structID, int schemaID){

  init();
  mstructID=structID;
  mschemaID=schemaID;

};

/////////////////////////////////////////////////////////////////////////

void
StDbTableDescriptor::init(){

mtableSize=0;
mCur = 0;
mMax = 100;
offsetToNextEmptyByte = 0;
offsetToLast4Bytes = -4;
mnumElements = 0;
lastType=Stdouble;
padsize = 0;
mcols = new tableDescriptor[mMax];
memset(mcols,0,mMax*sizeof(tableDescriptor));
mschemaID=mstructID=0;
misValid=false;
mhasDouble=false;
rowSizeTT = 0;

mAlign[Stchar] = st_alignof(char);
mAlign[Stuchar] = st_alignof(unsigned char);
mAlign[Stshort] = st_alignof(short);
mAlign[Stushort] = st_alignof(unsigned short);
mAlign[Stint] = st_alignof(int);
mAlign[Stuint] = st_alignof(unsigned int);
mAlign[Stlong] = st_alignof(long);
mAlign[Stulong] = st_alignof(unsigned long);
mAlign[Stlonglong] = st_alignof(long long);
mAlign[Stfloat] = st_alignof(float);
mAlign[Stdouble] = st_alignof(double);

maxAlign = mAlign[Stchar];

}

/////////////////////////////////////////////////////////////////////////

StDbTableDescriptor::StDbTableDescriptor(StDbTableDescriptor& d){

mCur = (int)d.getNumElements()-1;
mMax = d.getCurrentInternalSize();
offsetToNextEmptyByte = 0;
offsetToLast4Bytes = 0;
mcols = d.getTableDescriptor();
mnumElements=d.getNumElements();
mtableSize = d.getTotalSizeInBytes();
mschemaID=d.getSchemaID();
mstructID=d.getStructID();
misValid=d.IsValid();
mhasDouble=d.mhasDouble;
maxAlign=d.maxAlign;
rowSizeTT = d.rowSizeTT;

mAlign[Stchar] = st_alignof(char);
mAlign[Stuchar] = st_alignof(unsigned char);
mAlign[Stshort] = st_alignof(short);
mAlign[Stushort] = st_alignof(unsigned short);
mAlign[Stint] = st_alignof(int);
mAlign[Stuint] = st_alignof(unsigned int);
mAlign[Stlong] = st_alignof(long);
mAlign[Stulong] = st_alignof(unsigned long);
mAlign[Stlonglong] = st_alignof(long long);
mAlign[Stfloat] = st_alignof(float);
mAlign[Stdouble] = st_alignof(double);

}

/////////////////////////////////////////////////////////////////////////

tableDescriptor*
StDbTableDescriptor::getTableDescriptor() const {

  tableDescriptor* dScr = new tableDescriptor[mMax];
  memset(dScr,0,(mMax)*sizeof(tableDescriptor));
  memcpy(dScr,mcols,(mMax)*sizeof(tableDescriptor));

return dScr;
}
/////////////////////////////////////////////////////////////////////////

StTableDescriptorI*
StDbTableDescriptor::getCpy(){

StTableDescriptorI* dScr = new StDbTableDescriptor(*this);
return dScr;

}
/////////////////////////////////////////////////////////////////////////

void
StDbTableDescriptor::fillElement(StDbBuffer* buff, int tableID){
    
  //bool ClientMode;
//  if(!(ClientMode=buff->IsClientMode()))buff->SetClientMode();
  int schemaID;
  if(tableID){  // mask off elements if tableID is non-zero
   if(!(buff->ReadScalar(schemaID,"schemaID") && (schemaID==tableID)) ){  
       // skip this one
       return;
   }
  }

   reSize(); // increase array if needed
   int i = mCur;
   char* mtype = 0;
   char* mname = 0;
   buff->ReadScalar(mname,"name");
   if(mname)strcpy(mcols[i].name,mname);
   if(mname) delete [] mname;
   if(buff->ReadScalar(mtype,"type")){
       mcols[i].type = getType(mtype);
       if(mtype)delete [] mtype;
       char* length=0;
       if(buff->ReadScalar(length,"length"))fillSizeAndOffset(length,i);
       if(length) delete [] length;
   }

   if (mcols[i].type==Stlonglong || mcols[i].type==Stdouble ) mhasDouble=true;
   if (getAlign(mcols[i].type) > maxAlign) { maxAlign = getAlign(mcols[i].type); }
   
   mCur++;
   mnumElements++;

   // for multiple rows, add padding as needed at end of structure (tableSize)

   int rowpad;
   if ( ( rowpad = offsetToNextEmptyByte%2 ) ) { rowpad = 2 - rowpad; }

   //mtableSize = offsetToNextEmptyByte + rowpad;

   // correct padding should be based on max align:
   mtableSize = int(ceil(float(offsetToNextEmptyByte) / float(maxAlign)) * maxAlign);

   //if(!ClientMode)buff->SetStorageMode();  // reset to StorageMode

 misValid=true; 

}

/////////////////////////////////////////////////////////////////////////


void
StDbTableDescriptor::reSize(){

  // simply add 10 elements

  if(mCur<mMax) return;

  int newMax = mMax+10;
  tableDescriptor* dScr = new tableDescriptor[newMax];
  memcpy(dScr,mcols,(mMax)*sizeof(tableDescriptor));
  if(mcols)delete [] mcols;
  mcols=dScr;
  mMax=newMax;

}

/////////////////////////////////////////////////////////////////////////

void
StDbTableDescriptor::fillLengths(char* length, int elementNum){

int ip = 0;
int i=elementNum;
char* id= strstr(length,",");

// preset lengths to 1;

  int k= (int)(sizeof(mcols[i].dimensionlen)/sizeof(ip));
  for(int j=0;j<k;j++)mcols[i].dimensionlen[j]=1;

 while (id && ip<3) {
   id[0]='\0';
   mcols[i].dimensionlen[ip] = atoi(length);
   ip++;
   id++;
   length=id;
   id= strstr(length,",");
 }
 mcols[i].dimensionlen[ip]=atoi(length); 

}

/////////////////////////////////////////////////////////////////////////

void
StDbTableDescriptor::fillSizeAndOffset(char* length, int elementNum){

  fillLengths(length,elementNum);
  StTypeE type = mcols[elementNum].type;

  int j = 0;
  mcols[elementNum].size = getSize(mcols[elementNum].type);

  int k= (int)(sizeof(mcols[elementNum].dimensionlen)/sizeof(j));
  for (j=0; j<k; j++) mcols[elementNum].size *= mcols[elementNum].dimensionlen[j];

  int offp = int(ceil(float(offsetToNextEmptyByte) / float(getAlign(type))) * getAlign(type));
  mcols[elementNum].offset = offp;
  offsetToNextEmptyByte = mcols[elementNum].offset + mcols[elementNum].size;

}

/////////////////////////////////////////////////////////////////////////

StTypeE
StDbTableDescriptor::getType(char* type) {

StTypeE retVal=Stchar;

//char* typenames[] = {"Stchar","Stuchar","Stshort","Stushort","Stint","Stuint","Stlong","Stulong","Stfloat","Stdouble","Stascii","Ststring"};
const char* typenames[] = {"char","uchar","short","ushort","int","uint","long","ulong","longlong","float","double","ascii","string"};

 for(int i=0; i<12;i++){
   if(strcmp(type,typenames[i])==0){
     retVal=(StTypeE)i;
     break;
   }
 }

return retVal;
}

//////////////////////////////////////////////////////////////
void StDbTableDescriptor::endRowPadding(){

  //simple item for solaris & use of doubles & longlong.
  // if the struct contains such an entity, it's size must
  // be divisable by 8.

#ifndef __linux__
  if(mhasDouble){
     int checkPadding=mtableSize%8;
     if(checkPadding>0 && checkPadding<5)mtableSize+=4;
  }
#endif

};





       
       
      











