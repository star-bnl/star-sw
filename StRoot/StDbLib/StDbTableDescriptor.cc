/***************************************************************************
 *
 * $Id: StDbTableDescriptor.cc,v 1.4 1999/09/30 02:06:10 porter Exp $
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
mMax = 4;
offsetToNextEmptyByte = 0;
offsetToLast4Bytes = -4;
mnumElements = 0;
lastType=Stdouble;

mcols = new tableDescriptor[mMax+1];

}

/////////////////////////////////////////////////////////////////////////

StDbTableDescriptor::StDbTableDescriptor(StDbTableDescriptor& d){

mCur = (int)d.getNumElements()-1;
mMax = 0;
offsetToNextEmptyByte = 0;
offsetToLast4Bytes = 0;
mcols = d.getTableDescriptor();

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
  int mask;
  if(tableID){  // mask off elements if tableID is non-zero
   if(!(buff->ReadScalar(mask,"mask") && (mask & tableID)) ){  
       // don't include this element
       return;
   }
  }

   reSize(); // increase array if needed
   int i = mCur;
   char* mtype = 0;
   char* mname = 0;
   buff->ReadScalar(mname,"name");
   if(mname)strcpy(mcols[i].name,mname);
   if(buff->ReadScalar(mtype,"type")){
       mcols[i].type = getType(mtype);
       char* length=0;
       if(buff->ReadScalar(length,"length"))fillSizeAndOffset(length,i);
   }

   //  cout << "Element Loaded "<< endl;
   //  cout << " name = " << mcols[i].name << " type = " << mcols[i].type << "    //   size = " << mcols[i].size << " offset = " << mcols[i].offset << endl;
   mCur++;
   mnumElements++;
   mtableSize = offsetToNextEmptyByte;

 if(!ClientMode)buff->SetStorageMode();  // reset to StorageMode
}

/////////////////////////////////////////////////////////////////////////


void
StDbTableDescriptor::reSize(){

  // simply add 10 elements

  if(mCur==mMax){
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

  // offsetToLast4Bytes
  // offsetToNextEmptyByte

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
     //     j = 4* ((int) floor ( (mcols[i].size-space-1)/4 ));
     j = 4* ((int) floor ((float) (mcols[i].size-1)/4 ));
     offsetToLast4Bytes = offsetToLast4Bytes+j+4;

  } else if( (space>=2) && (type == Stshort || type == Stushort) ){

      mcols[i].offset=offsetToLast4Bytes+2;
      j = 4* ((int) floor ((float) (mcols[i].size-2-1)/4 ));    // note the 2
      offsetToNextEmptyByte=mcols[i].offset+mcols[i].size;
      offsetToLast4Bytes = offsetToLast4Bytes+j;//+4;  // 4 is for the 1st "row"

  } else {

    if(type==Stdouble && lastType!=Stdouble){
      offsetToLast4Bytes+=4;
      offsetToNextEmptyByte+=4;
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
  lastType=type;
#endif
}

/////////////////////////////////////////////////////////////////////////

StTypeE
StDbTableDescriptor::getType(char* type) {

StTypeE retVal;

//char* typenames[] = {"Stchar","Stuchar","Stshort","Stushort","Stint","Stuint","Stlong","Stulong","Stfloat","Stdouble","Stascii","Ststring"};
char* typenames[] = {"char","uchar","short","ushort","int","uint","long","ulong","float","double","ascii","string"};

 for(int i=0; i<12;i++){
   if(strcmp(type,typenames[i])==0){
     retVal=(StTypeE)i;
     break;
   }
 }

return retVal;
}










