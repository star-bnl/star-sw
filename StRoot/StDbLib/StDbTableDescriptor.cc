/***************************************************************************
 *
 * $Id: StDbTableDescriptor.cc,v 1.11 2000/02/15 20:27:45 porter Exp $
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
 * some fixes for linux warnings
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
#include <iostream.h>


/////////////////////////////////////////////////////////////////////////

StDbTableDescriptor::StDbTableDescriptor(){

mCur = 0;
mMax = 100;
offsetToNextEmptyByte = 0;
offsetToLast4Bytes = -4;
mnumElements = 0;
lastType=Stdouble;
padsize = 0;
mcols = new tableDescriptor[mMax+1];

}

///////////////////////////////////////////////////////////////////////

StDbTableDescriptor::StDbTableDescriptor(_Descriptor* d, unsigned int numElements, unsigned int sizeOfStruct){

mnumElements = numElements;
mCur = mnumElements-1;
mtableSize = sizeOfStruct;
mcols = new tableDescriptor[mnumElements];
 for(int i=0;i<(int)mnumElements;i++){
  strcpy(mcols[i].name,d[i].name);
  mcols[i].size = (unsigned int)d[i].typeSize;
  mcols[i].offset = (unsigned int)d[i].offset;

  int k= (int)(sizeof(mcols[i].dimensionlen)/sizeof(int));
  for(int j=0;j<k;j++)mcols[i].dimensionlen[j]=1;
  if(d[i].firstDimension)mcols[i].dimensionlen[0]=d[i].firstDimension;
  if(d[i].secondDimension)mcols[i].dimensionlen[1]=d[i].secondDimension;
  
  switch (d[i].type) {
  case kFloat:
    {
      mcols[i].type=Stfloat;
      break;
    }
  case kInt:
    {
      mcols[i].type=Stint;
      break;
    }
  case kLong:
    {
      mcols[i].type=Stlong;
      break;
    }
  case kShort:
    {
      mcols[i].type=Stshort;
      break;
    }
  case kDouble:
    {
      mcols[i].type=Stdouble;
      break;
    }
  case kUInt:
    {
      mcols[i].type=Stuint;
      break;
    }
  case kULong:
    {
      mcols[i].type=Stulong;
      break;
    }
  case kUShort:
    {
      mcols[i].type=Stushort;
      break;
    }
  case kUChar:
    {
      mcols[i].type=Stuchar;
      break;
    }
  case kChar:
    {
      mcols[i].type=Stchar;
      break;
    }
  default:
    {
      break;
    }
  }

 }

}

/////////////////////////////////////////////////////////////////////////

StDbTableDescriptor::StDbTableDescriptor(StDbTableDescriptor& d){

mCur = (int)d.getNumElements()-1;
mMax = 0;
offsetToNextEmptyByte = 0;
offsetToLast4Bytes = 0;
mcols = d.getTableDescriptor();
mnumElements=d.getNumElements();
mtableSize = d.getTotalSizeInBytes();

}

/////////////////////////////////////////////////////////////////////////

tableDescriptor*
StDbTableDescriptor::getTableDescriptor() const {

     tableDescriptor* dScr = new tableDescriptor[mMax+1];
     memcpy(dScr,mcols,(mMax+1)*sizeof(tableDescriptor));

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

bool ClientMode;
  if(!(ClientMode=buff->IsClientMode()))buff->SetClientMode();
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

   mCur++;
   mnumElements++;

   // for multiple rows, add padding as needed at end of structure (tableSize)
   int rowpad;
   if((rowpad=offsetToNextEmptyByte%4))rowpad=4-rowpad;

   mtableSize = offsetToNextEmptyByte+rowpad;

 if(!ClientMode)buff->SetStorageMode();  // reset to StorageMode
}

/////////////////////////////////////////////////////////////////////////


void
StDbTableDescriptor::reSize(){

  // simply add 10 elements

  if(mCur>mMax){
     int newMax = mMax+10+1;
     tableDescriptor* dScr = new tableDescriptor[newMax];
     memcpy(dScr,mcols,(mMax+1)*sizeof(tableDescriptor));
     if(mcols)delete [] mcols;
     mcols=dScr;
     mMax=newMax-1;
  }

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


  int i = elementNum;
  fillLengths(length,i);
  int space = 4-(offsetToNextEmptyByte-offsetToLast4Bytes);

  int j;
  StTypeE type = mcols[i].type;

  mcols[i].size = getSize(mcols[i].type);
  int k= (int)(sizeof(mcols[i].dimensionlen)/sizeof(j));
  for(j=0;j<k;j++)mcols[i].size*=mcols[i].dimensionlen[j];

  if(offsetToLast4Bytes < 0){

    mcols[i].offset = 0;
    offsetToNextEmptyByte = mcols[i].offset+mcols[i].size;
    j = 4* ((int) floor ( (float) (mcols[i].size-1)/4 ));
    offsetToLast4Bytes = mcols[i].offset + j;// + 4;
 

  } else if(type==Stchar || type==Stuchar){

     mcols[i].offset=offsetToNextEmptyByte;
     offsetToNextEmptyByte = mcols[i].offset+mcols[i].size;
     j = 4* ((int) floor ((float) (mcols[i].size-1)/4 ));
     offsetToLast4Bytes = offsetToLast4Bytes+j+4;

  } else if( (space>=2) && (type == Stshort || type == Stushort) ){

      mcols[i].offset=offsetToLast4Bytes+2;
      j = 4* ((int) floor ((float) (mcols[i].size-2-1)/4 ));    // note the 2
      offsetToNextEmptyByte=mcols[i].offset+mcols[i].size;
      offsetToLast4Bytes = offsetToLast4Bytes+j;//+4;  // 4 is for the 1st "row"

  } else {


    if(type==Stdouble && lastType != Stdouble && padsize < 8){
      //      offsetToLast4Bytes+=4;
      //      offsetToNextEmptyByte+=4;
      offsetToLast4Bytes+=padsize;
      offsetToNextEmptyByte+=padsize;
    }

     mcols[i].offset=offsetToLast4Bytes+4;
     offsetToNextEmptyByte = mcols[i].offset + mcols[i].size;
     j = 4* ((int) floor ((float) (mcols[i].size-1)/4 ));
     offsetToLast4Bytes = mcols[i].offset + j;// + 4;
     
  }

  if(offsetToLast4Bytes<0)offsetToLast4Bytes=0;

#ifdef LINUX
  lastType=Stdouble;
#else
  if(type==Stdouble)padsize = 0;
  lastType=type;

  unsigned int onesize = getSize(mcols[i].type);
  for(j=0;j<k;j++){
    if(mcols[i].dimensionlen[j]==1 && j>0)continue;
    for(int jj=0; jj< mcols[i].dimensionlen[j]; jj++){
      padsize=padsize+(int)onesize;
      if(padsize>=8)padsize=padsize-8;
    }
  }

#endif
}

/////////////////////////////////////////////////////////////////////////

StTypeE
StDbTableDescriptor::getType(char* type) {

StTypeE retVal;

//char* typenames[] = {"Stchar","Stuchar","Stshort","Stushort","Stint","Stuint","Stlong","Stulong","Stfloat","Stdouble","Stascii","Ststring"};
const char* typenames[] = {"char","uchar","short","ushort","int","uint","long","ulong","float","double","ascii","string"};

 for(int i=0; i<12;i++){
   if(strcmp(type,typenames[i])==0){
     retVal=(StTypeE)i;
     break;
   }
 }

return retVal;
}










