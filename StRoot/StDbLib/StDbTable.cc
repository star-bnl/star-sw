/***************************************************************************
 *
 * $Id: StDbTable.cc,v 1.10 1999/12/07 21:25:25 porter Exp $
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
#include <string.h>
#include <iostream.h>
#include <malloc.h>


///////////////////////////////////////////////////////////////

StDbTable::StDbTable(const char* tableName): mtableName(0), mstructID(0), misBaseLine(false), mhasDescriptor(false), mdescriptor(0), mdata(0) { 
setTableName(tableName);
maccessor.endTime = -1;
maccessor.version=0;
mrows=1;
mrowNumber=0;
maccessor.elementID = 0;

}

///////////////////////////////////////////////////////////////

StDbTable::StDbTable(const char* tableName, int schemaID): mtableName(0), mstructID(0), misBaseLine(false), mhasDescriptor(false), mdescriptor(0), mdata(0) { 
setTableName(tableName);
//maccessor.endTime = -1;
maccessor.version=0;
maccessor.schemaID=schemaID;
mrows=1;
mrowNumber=0;
maccessor.elementID = 0;
}

///////////////////////////////////////////////////////////////

StDbTable::StDbTable(StDbTable& table){

 mtableName=0;
 mdescriptor=0;
 mdata = 0;
 mrowNumber = 0;
 mrows = table.GetNRows();
 mtableName=table.getTableName();
 mstructID=table.getStructID();
 mhasDescriptor=table.hasDescriptor();
 mdescriptor=table.getDescriptorCpy();
 setTableName(table.getTableName());
 maccessor = table.getAccessor();

 char* tmp = table.GetTable();
 if(tmp) {
   unsigned int size = mrows*table.getTableSize();
   mdata = new char[size];
   memcpy(mdata,tmp,size);
 }


}

/////////////////////////////////////////////////////////////////////

StTableDescriptorI*
StDbTable::getDescriptorCpy() const {
return mdescriptor->getCpy();
}

/////////////////////////////////////////////////////////////////////

void 
StDbTable::setDescriptor(StTableDescriptorI* descriptor){ 

 if(mdescriptor) delete mdescriptor;
 mdescriptor=descriptor;
 mhasDescriptor=true;
 //checkDescriptor();

};


/////////////////////////////////////////////////////////////////////

char* 
StDbTable::GetTable() { if(!mdata)createMemory(); return mdata;};

/////////////////////////////////////////////////////////////////////

void* 
StDbTable::GetTableCpy() { 

if(!mdata)return (void*)GetTable();

int len = mrows*getTableSize();
//char* c = new char[len];
char* c = (char*)calloc(mrows,getTableSize());
memcpy(c,mdata,len);

return (void*)c;
};

/////////////////////////////////////////////////////////////////////

void 
StDbTable::SetTable(char* c, int nrows) { 

if(mdata){
  delete [] mdata; 
  mdata = 0;
}
createMemory(nrows);
int len = nrows*getTableSize();
memcpy(mdata,c,len);


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

}
/////////////////////////////////////////////////////////////////////

char*
StDbTable::duplicateData() { 


int len1 = mrows*getTableSize();
char* dup=new char[len1];
memcpy(dup,mdata,len1);

return dup;

}


/////////////////////////////////////////////////////////////////////

bool
StDbTable::createMemory(int nrows) { 
 mrows = nrows;
 return createMemory();
}

/////////////////////////////////////////////////////////////////////

bool
StDbTable::createMemory() {
bool retVal = true;

  if(mdata)return retVal;

  if(mdescriptor && mdescriptor->getNumElements()>0){
     int len = mrows*mdescriptor->getTotalSizeInBytes();
     mdata=new char[len];
     memset(mdata,0,len);
  } else {
    if(!mtableName){mtableName=new char[8]; strcpy(mtableName,"Unknown");}
    //    cerr << "Table [ "<<mtableName<<" ] has no description to fill memory" << endl;
    retVal = false;
  }
return retVal;
}
    
/////////////////////////////////////////////////////////////////////

void
StDbTable::setTableName(const char* name){

  if(mtableName)delete [] mtableName;
  mtableName = new char[strlen(name)+1];
  strcpy(mtableName,name);

}

//////////////////////////////////////////////////////////////////////

char* 
StDbTable::getTableName() const { 

if(!mtableName)return mtableName;

char* retString = new char[strlen(mtableName)+1];
strcpy(retString,mtableName);
return retString;

}

//////////////////////////////////////////////////////////////////////

void
StDbTable::setElementID(int* elements, int nrows) { 

 mrows = nrows;
 if(mrows==1){
   maccessor.elementID = new int[1];
   *(maccessor.elementID) = 0;
 } else {
   maccessor.elementID = new int[nrows];
   for(int i=0;i<nrows;i++)maccessor.elementID[i] = elements[i];
 }

}


//////////////////////////////////////////////////////////////////////


void
StDbTable::StreamAccessor(typeAcceptor* accept, bool isReading){

   int len = 1;

   accept->pass("schemaID",maccessor.schemaID,len);

   if(isReading){

   if(maccessor.beginTime.mdateTime) delete [] maccessor.beginTime.mdateTime;
   if(maccessor.version)delete [] maccessor.version;
   if(maccessor.elementID)delete [] maccessor.elementID;

   } else {

     if(!maccessor.elementID){
       maccessor.elementID = new int[mrows];
       for(int i=0;i<mrows;i++)maccessor.elementID[i]=i;
     }

   }

   accept->pass("beginTime",maccessor.beginTime.mdateTime,len);
   accept->pass("version",maccessor.version,len);
   //   int * eids;
   accept->pass("elementID",maccessor.elementID, mrows);
   if(isReading)cout << mtableName << " & " << mrows << endl;
 
}

//////////////////////////////////////////////////////////////////////

void
StDbTable::StreamAccessor(StDbBufferI* buff, bool isReading){

  bool ClientMode;
  if(!(ClientMode=buff->IsClientMode()))buff->SetClientMode();

  int rowID;

   if(!maccessor.elementID){
     maccessor.elementID = new int[mrows];
     for(int i=0;i<mrows;i++)maccessor.elementID[i]=i;
   }

  if(isReading){
    buff->ReadScalar(rowID,"elementID");
    maccessor.elementID[mrowNumber]=rowID;

    if(mrowNumber==0){
     buff->ReadScalar(maccessor.schemaID,"schemaID");
     char* version = 0;
     buff->ReadScalar(version,"version");
     if(version)strcpy(maccessor.version,version);
     delete [] version;
    } else {
      unsigned int bTime;// , eTime;
      buff->ReadScalar(bTime,"beginTime"); 
      if(bTime>maccessor.beginTime.munixTime)maccessor.beginTime.munixTime=bTime;
    }

  } else {

   buff->WriteScalar(maccessor.schemaID,"schemaID");
   buff->WriteScalar(maccessor.beginTime.munixTime,"beginTime");
   //   buff->WriteScalar(maccessor.endTime,"endTime");
   buff->WriteScalar(maccessor.version,"version");
   rowID = maccessor.elementID[mrowNumber];   
   buff->WriteScalar(rowID,"elementID");

  }

 if(!ClientMode)buff->SetStorageMode();  // reset to StorageMode

}

///////////////////////////////////////////////////////////////////////

void
StDbTable::getElementSpecs(int elementNum, char*& c, char*& name, unsigned int& length,StTypeE& type){

    int rowIndex = mrowNumber*mdescriptor->getTotalSizeInBytes();
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
      //       cout << " Reading object " << name << endl;
      //     cout << " buffer offset is " << ptr-mdata << endl;
     ReadElement(ptr,name,length,type,(StDbBuffer*)buff);
     } else {
     WriteElement(ptr,name,length,type,(StDbBuffer*)buff);
    }       
   delete [] name;
 }

 mrowNumber++;

 } else {
   cerr << "dbStreamer:: more rows delivered than allocated " << endl;
 }


 if(!ClientMode)buff->SetStorageMode();  // reset to StorageMode
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

 } else {
   cerr << "dbTableStreamer:: more rows delivered than allocated " << endl;
   cerr << " #of rows= "<<mrows<< " current row= "<<mrowNumber << endl;
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
      // cout << "Acceptor offset is " << ptr-mdata << endl;
      PassInElement(ptr,name,length,type,accept);
      delete [] name;
     }
   } else {
     for(int i=0; i<max; i++){
      getElementSpecs(i,ptr,name,length,type);
      // cout << "Acceptor offset is " << ptr-mdata << endl;
      PassOutElement(ptr,name,length,type,accept);
      delete [] name;
     }
   }

 mrowNumber++;

 } else {
   cerr << "Cannot Stream Data" << endl;
 }

}


///////////////////////////////////////////////////////////////////////

void
StDbTable::ReadElement(char*& ptr, char* name, int len, StTypeE type, StDbBuffer* buff){

char* mchar; unsigned char* muchar; short* mshort; unsigned short* mushort; 
int* mint; unsigned int* muint; long* mlong; unsigned long* mulong; 
float* mfloat; double* mdouble;
 
  switch (type) {
  case Stchar:
    {
    buff->ReadArray(mchar,len,name);
    memcpy(ptr,mchar,len);
    delete [] mchar;
    break;
    }
  case Stuchar:
    {
       buff->ReadArray(muchar,len,name);
       // buff->ReadArray(mint,len,name);
       //unsigned char* tmp = new unsigned char[len];
       //for(int k=0;k<len;k++){
       //tmp[k]= (unsigned char)*mint;
       // mint++;
       // }
    memcpy(ptr,muchar,len*sizeof(unsigned char));
    delete [] muchar;
    // delete [] tmp;
    break;
    }
  case Stshort:
    {
    buff->ReadArray(mshort,len,name);
    memcpy(ptr,mshort,len*sizeof(short));
    delete [] mshort;
    break;
    }
  case Stushort:
    {
    buff->ReadArray(mushort,len,name);
    memcpy(ptr,mushort,len*sizeof(unsigned short));
    delete [] mushort;
    break;
    }
  case Stint:
    {
    buff->ReadArray(mint,len,name);
    memcpy(ptr,mint,len*sizeof(int));
    delete [] mint;
    break;
    }
  case Stuint:
    {
    buff->ReadArray(muint,len,name);
    memcpy(ptr,muint,len*sizeof(unsigned int));
    delete [] muint;
    break;
    }
  case Stlong:
    {
    buff->ReadArray(mlong,len,name);
    //if(len==1)cout<<name<<" = "<<*mlong<<endl;
    memcpy(ptr,mlong,len*sizeof(long));
    delete [] mlong;
    break;
    }
  case Stulong:
    {
    buff->ReadArray(mulong,len,name);
    memcpy(ptr,mulong,len*sizeof(unsigned long));
    delete [] mulong;
    break;
    }
  case Stfloat:
    {
    buff->ReadArray(mfloat,len,name);
    //if(len==1)cout<<name<<" = "<<*mfloat<<endl;
    memcpy(ptr,mfloat,len*sizeof(float));
    delete [] mfloat;
    break;
    }
  case Stdouble:
    {
    buff->ReadArray(mdouble,len,name);
    memcpy(ptr,mdouble,len*sizeof(double));
    delete [] mdouble;
    break;
    }
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
    int* tmp = new int[len] ;
    for(int k=0;k<len;k++){
      tmp[k]= (int)*muchar;
      muchar++;
    }
    //   buff->WriteArray(muchar,len,name);
    buff->WriteArray(tmp,len,name);
    delete [] tmp;
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
  case Stfloat:
    {

     float* data; 
     //     cout << "1st = " << len << endl; 
     accept->pass(name,data,len);
     //     cout << "2st = " << len << " & " << *data << endl;
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
  }

}

/////////////////////////////////////////////////////////////////


void
StDbTable::checkDescriptor(){

int i = mdescriptor->getNumElements();
unsigned int size = mdescriptor->getTotalSizeInBytes();

 cout <<"Descriptor for Table = " << mtableName<<endl;
 cout <<" number of elements = "<<i<< " with size = " << size << endl;

 for(int k=0; k<i;k++){
   cout <<"Name = " << mdescriptor->getElementName(k);
   cout <<" size = " << mdescriptor->getElementSize(k);
   cout <<" offset = " <<mdescriptor->getElementOffset(k);
   cout <<" type = " <<(int)mdescriptor->getElementType(k)<<endl;
 }


}




