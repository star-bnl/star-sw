#include "StDbTable.h"
#include "StDbBuffer.h"
#include <string.h>
#include <iostream.h>


///////////////////////////////////////////////////////////////

StDbTable::StDbTable(const char* tableName): mtableName(0), mstructID(0), misBaseLine(false), mhasDescriptor(false), mdescriptor(0), mdata(0) { 
setTableName(tableName);
maccessor.endTime = -1;
maccessor.version=0;
mrows=1;
mrowNumber=0;
maccessor.elementID = new int;
*(maccessor.elementID) = 0;

}

///////////////////////////////////////////////////////////////

StDbTable::StDbTable(const char* tableName, int schemaID): mtableName(0), mstructID(0), misBaseLine(false), mhasDescriptor(false), mdescriptor(0), mdata(0) { 
setTableName(tableName);
//maccessor.endTime = -1;
maccessor.version=0;
maccessor.schemaID=schemaID;
mrows=1;
mrowNumber=0;
maccessor.elementID = new int;
*(maccessor.elementID) = 0;
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

void 
StDbTable::SetTable(char* c, int nrows) { 

if(mdata)delete [] mdata; 

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
  } else {
    if(!mtableName){mtableName=new char[8]; strcpy(mtableName,"Unknown");}
    cerr << "Table [ "<<mtableName<<" ] has no description to fill memory" << endl;
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
   maccessor.elementID = new int;
   *(maccessor.elementID) = 0;
 } else {
   maccessor.elementID = new int[nrows];
   for(int i=0;i<nrows;i++)maccessor.elementID[i] = elements[i];
 }

}


//////////////////////////////////////////////////////////////////////


void
StDbTable::StreamAccessor(typeAcceptor* accept){

  //  cout << "stream sID " << endl;
   accept->pass("schemaID",maccessor.schemaID,1);
   //  cout << "stream bt " << endl;
   int len = 1;
   if(maccessor.beginTime.mdateTime)len=strlen(maccessor.beginTime.mdateTime);
   accept->pass("beginTime",maccessor.beginTime.mdateTime,1);
   //  cout << "stream et " << endl;
   //   accept->pass("endTime",maccessor.endTime,1);
   //  cout << "stream v " << endl;
   if(maccessor.version)len=strlen(maccessor.version);
   accept->pass("version",maccessor.version,len);
   //  cout << "stream eID " << endl;
   accept->pass("elementID",maccessor.elementID, mrows);

}



//////////////////////////////////////////////////////////////////////

void
StDbTable::StreamAccessor(StDbBufferI* buff, bool isReading){

  bool ClientMode;
  if(!(ClientMode=buff->IsClientMode()))buff->SetClientMode();

  int rowID;

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
      //      buff->ReadScalar(eTime,"endTime");
      //      if(eTime<maccessor.endTime.munixTime) maccessor.endTime.munixTime = eTime;
    }
    //   buff->ReadScalar(maccessor.beginTime,"beginTime");
    //   buff->ReadScalar(maccessor.endTime,"endTime");
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
StDbTable::dbStreamer(typeAcceptor* accept){

int max = mdescriptor->getNumElements();
char* name;
StTypeE type;
unsigned int length;
char* ptr;

 if(createMemory() && mrowNumber < mrows){

 for(int i=0; i<max; i++){
    getElementSpecs(i,ptr,name,length,type);
    // cout << "Acceptor offset is " << ptr-mdata << endl;
    PassElement(ptr,name,length,type,accept);
    delete [] name;
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
    memcpy(ptr,muchar,len);
    delete [] muchar;
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
StDbTable::PassElement(char* ptr, char* name, int len, StTypeE type, typeAcceptor* accept){

 
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




