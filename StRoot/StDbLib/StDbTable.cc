#include "StDbTable.h"
#include "StDbBuffer.h"
#include <string.h>
#include <iostream.h>


///////////////////////////////////////////////////////////////

StDbTable::StDbTable(const char* tableName): mdata(0), mtableName(0), mhasDescriptor(0), mtableID(0), mdescriptor(0)  { 
setTableName(tableName);
maccessor.endTime = -1;
maccessor.version[0]='\0';
}

///////////////////////////////////////////////////////////////

StDbTable::StDbTable(const char* tableName, int tableID): mdata(0), mtableName(0), mhasDescriptor(0), mdescriptor(0), mtableID(tableID) { 
setTableName(tableName);
maccessor.endTime = -1;
maccessor.version[0]='\0';
}

///////////////////////////////////////////////////////////////

StDbTable::StDbTable(StDbTable& table){

 mtableName=0;
 mdescriptor=0;
 mdata = 0;
 mtableName=table.getTableName();
 mtableID=table.getTableID();
 mhasDescriptor=table.hasDescriptor();
 mdescriptor=table.getDescriptorCpy();
 setTableName(table.getTableName());
 maccessor = table.getAccessor();

 char* tmp = table.GetTable();
 if(tmp) {
   unsigned int size = table.getTableSize();
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
char* 
StDbTable::GetTable() { if(!mdata)createMemory(); return mdata;};
/////////////////////////////////////////////////////////////////////

bool
StDbTable::createMemory() {
bool retVal = true;

  if(mdata)return retVal;

  if(mdescriptor && mdescriptor->getNumElements()>0){
     mdata=new char[mdescriptor->getTotalSizeInBytes()];
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
StDbTable::StreamAccessor(typeAcceptor* accept){

  //  cout << "stream sID " << endl;
   accept->pass("schemaID",maccessor.schemaID,1);
   //  cout << "stream bt " << endl;
   accept->pass("beginTime",maccessor.beginTime,1);
   //  cout << "stream et " << endl;
   accept->pass("endTime",maccessor.endTime,1);
   //  cout << "stream v " << endl;
   accept->pass("version",maccessor.version,strlen(maccessor.version));
   //  cout << "stream eID " << endl;
   accept->pass("elementID",maccessor.elementID,1);

}



//////////////////////////////////////////////////////////////////////

void
StDbTable::StreamAccessor(StDbBufferI* buff, bool isReading){

  bool ClientMode;
  if(!(ClientMode=buff->IsClientMode()))buff->SetClientMode();


  if(isReading){
   //  cout << "stream sID " << endl;
   buff->ReadScalar(maccessor.schemaID,"schemaID");
   //  cout << "stream bt " << endl;
   buff->ReadScalar(maccessor.beginTime,"beginTime");
   //  cout << "stream et " << endl;
   buff->ReadScalar(maccessor.endTime,"endTime");
   //  cout << "stream v " << endl;
   char* mversion = 0;
   buff->ReadScalar(mversion,"version");
   if(mversion)strcpy(maccessor.version,mversion);
   delete [] mversion;
   //  cout << "stream eID " << endl;
   buff->ReadScalar(maccessor.elementID,"elementID");
  } else {
   //  cout << "stream sID " << endl;
   buff->WriteScalar(maccessor.schemaID,"schemaID");
   //  cout << "stream bt " << endl;
   buff->WriteScalar(maccessor.beginTime,"beginTime");
   //  cout << "stream et " << endl;
   buff->WriteScalar(maccessor.endTime,"endTime");
   //  cout << "stream v " << endl;
   buff->WriteScalar(maccessor.version,"version");
   //  cout << "stream eID " << endl;
   buff->WriteScalar(maccessor.elementID,"elementID");
  }

 if(!ClientMode)buff->SetStorageMode();  // reset to StorageMode

}

///////////////////////////////////////////////////////////////////////

void
StDbTable::getElementSpecs(int elementNum, char*& c, char*& name, unsigned int& length,StTypeE& type){

    int i = elementNum;
    c = mdata;
    int max = mdescriptor->getElementOffset(i);
    for(int k=0;k<max;k++)c++;
    name = mdescriptor->getElementName(i);
    length = mdescriptor->getElementLength(i);;
    type = mdescriptor->getElementType(i);

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

 if(createMemory()){

 for(int i=0; i<max; i++){
    getElementSpecs(i,ptr,name,length,type);
    if(isReading){
      //       cout << " Reading object " << name << endl;
      //     cout << " buffer offset is " << ptr-mdata << endl;
     ReadElement(ptr,name,length,type,(StDbBuffer*)buff);
     } else {
     WriteElement(ptr,name,length,type,(StDbBuffer*)buff);
    }       
 }

 } else {
   cerr << "Cannot Stream Data" << endl;
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

 if(createMemory()){

 for(int i=0; i<max; i++){
    getElementSpecs(i,ptr,name,length,type);
    // cout << "Acceptor offset is " << ptr-mdata << endl;
    PassElement(ptr,name,length,type,accept);
 }

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
    buff->WriteArray(mchar,len,name);
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




