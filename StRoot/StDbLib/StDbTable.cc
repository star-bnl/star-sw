/***************************************************************************
 *
 * $Id: StDbTable.cc,v 1.16 2000/04/25 18:26:03 porter Exp $
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
#include <string.h>
#include <iostream.h>
#include <strstream.h>
#include <malloc.h>


///////////////////////////////////////////////////////////////

StDbTable::StDbTable(const char* tableName): StDbNode(tableName), mhasDescriptor(false), mdescriptor(0), mdata(0), mhasData(false), mstoreMode(false) { 

 mschemaID=0;
 mendTime.munixTime=0;
 mrows=0;
 mrowNumber=0;
 melementID = 0;
 mdataIDs = 0;
 mdataRows = 0;
 mMaxRows = 500;

 setDefaultFlavor();
 mprodTime = StDbDefaults::Instance()->getProdTime();

}

///////////////////////////////////////////////////////////////

StDbTable::StDbTable(const char* tableName, int schemaID): StDbNode(tableName), mhasDescriptor(false), mdescriptor(0), mdata(0), mhasData(false), mstoreMode(false) { 

mendTime.munixTime=0;
mschemaID=schemaID;
mrows=0;
mrowNumber=0;
melementID = 0;
 mdataIDs = 0;
 mdataRows = 0;
 mMaxRows = 500;

 setDefaultFlavor();
 mprodTime = StDbDefaults::Instance()->getProdTime();

}

///////////////////////////////////////////////////////////////

StDbTable::StDbTable(StDbTable& table): StDbNode(table){

melementID=0;

 strncpy(mflavor,table.getFlavor(),sizeof(mflavor));
 mdefaultFlavor = table.defaultFlavor();
 mprodTime = table.getProdTime();

 StDbNodeInfo mnode;
 table.getNodeInfo(&mnode);
 setNodeInfo(&mnode);
 mschemaID = table.getSchemaID();
 mstoreMode = table.IsStoreMode();
 mdescriptor=0;
 mdata = 0;
 mhasData = false;
 mrowNumber = 0;
 mrows = table.GetNRows();
 mhasDescriptor=table.hasDescriptor();
 mdescriptor=table.getDescriptorCpy();
 mdataIDs = 0;
 mdataRows = 0;
 mMaxRows = 500;

 mbeginTime.setDateTime(table.getBeginDateTime());
 mbeginTime.setUnixTime(table.getBeginTime());
 mendTime.setDateTime(table.getEndDateTime());
 mendTime.setUnixTime(table.getEndTime());

 char* tmp = table.GetTable();
 if(mrows==0) mrows = 1;
 if(tmp) {
   unsigned int size = mrows*table.getTableSize();
   mdata = new char[size];
   memcpy(mdata,tmp,size);
   mhasData = true;
 }


}

/////////////////////////////////////////////////////////////////////
void
StDbTable::setNodeInfo(StDbNodeInfo* node){

  StDbNode::setNodeInfo(node);
  if(melementID) delete [] melementID;
  melementID = mnode.getElementID((const char*)mnode.elementID,mrows);
    
}

/////////////////////////////////////////////////////////////////////

void
StDbTable::setDefaultFlavor(){

  char* flavor = StDbDefaults::Instance()->getFlavor();
  strncpy(mflavor,flavor,sizeof(mflavor));
  mdefaultFlavor = true;

}

/////////////////////////////////////////////////////////////////////

void 
StDbTable::setFlavor(const char* flavor) { 
 if(flavor){
   strncpy(mflavor,flavor,sizeof(mflavor)); 
   mdefaultFlavor = StDbDefaults::Instance()->IsDefaultFlavor(mflavor);
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
mhasData=true;
mstoreMode=true;

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
 return createMemory();
}

/////////////////////////////////////////////////////////////////////

bool
StDbTable::createMemory() {
bool retVal = true;

  if(mdata)return retVal;

  if(mdescriptor && mdescriptor->getNumElements()>0){
     if(mrows==0) mrows = 1;
     int len = mrows*mdescriptor->getTotalSizeInBytes();
     mdata=new char[len];
     memset(mdata,0,len);
     int max = mdescriptor->getNumElements();
     char* name;
     StTypeE type;
     unsigned int length;
     char * ptr;
     for(int i=0; i<max;i++){
       getElementSpecs(i,ptr,name,length,type);
       if(type==Stchar)ptr='\0';
       delete [] name;
     }
  } else {
    if(!mnode.name){mnode.mstrCpy(mnode.name,"Unknown");}
    retVal = false;
  }
return retVal;
}
    
//////////////////////////////////////////////////////////////////////

void
StDbTable::setElementID(int* elements, int nrows) { 

   mrows = nrows;

   if(mnode.elementID) delete [] mnode.elementID;
   mnode.elementID = new char[4*nrows];
   ostrstream os(mnode.elementID,4*nrows);
   melementID = new int[nrows];
   memcpy(melementID,elements,nrows*sizeof(int));

   for(int i=0;i<nrows-1;i++){
     os<<elements[i]<<",";
   }
   os<<elements[nrows-1]<<ends;

}

//////////////////////////////////////////////////////////////////////

void
StDbTable::addWrittenRow(int dataID){

  if(mdataRows==mMaxRows){
    int newMax = 2*mMaxRows;
    int* tmp= new int[newMax];
    memcpy(tmp,mdataIDs,mMaxRows*(sizeof(int)));
    delete [] mdataIDs;
    mdataIDs = tmp;
    mMaxRows = newMax;
  } else if( mdataRows == 0){
   mdataIDs = new int[mMaxRows];
   mcanRollBack = true;
  }

  mdataIDs[mdataRows] = dataID;
  mdataRows++;

}
  
//////////////////////////////////////////////////////////////////////

void
StDbTable::StreamAccessor(typeAcceptor* accept, bool isReading){

   int len = 1;

   accept->pass((char*)"schemaID",mschemaID,len);

   if(isReading){

   if(mbeginTime.mdateTime) delete [] mbeginTime.mdateTime;
   if(mnode.versionKey)delete [] mnode.versionKey;
   if(mnode.elementID)delete [] mnode.elementID;
   if(melementID)delete [] melementID;

   } else {

     if(!melementID){
       melementID = new int[mrows];
       for(int i=0;i<mrows;i++)melementID[i]=i;
     }

   }

   accept->pass((char*)"beginTime",mbeginTime.mdateTime,len);
   accept->pass((char*)"version",mnode.versionKey,len);
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
     char* version = 0;
     buff->ReadScalar(version,"version");
     if(version)mnode.mstrCpy(mnode.versionKey,version);
     if(version)delete [] version;
    } else {
      unsigned int bTime;// , eTime;
      buff->ReadScalar(bTime,"beginTime"); 
      if(bTime>mbeginTime.munixTime)mbeginTime.munixTime=bTime;
    }

  } else {

   buff->WriteScalar(mschemaID,"schemaID");
   buff->WriteScalar(mbeginTime.munixTime,"beginTime");
   buff->WriteScalar(mnode.versionKey,"version");
   rowID = melementID[mrowNumber];   
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
     ReadElement(ptr,name,length,type,(StDbBuffer*)buff);
     } else {
     WriteElement(ptr,name,length,type,(StDbBuffer*)buff);
    }       
   delete [] name;
 }

 mrowNumber++;
 if(isReading)mhasData=true;

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

 if(isReading) mhasData=true;
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
     mhasData=true;
     mstoreMode=true; // For Reading from Xml-into database
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
           char commentName[1024];
           ostrstream cn(commentName,1024);
           cn<<name<<".text"<<ends;
           //      buff->ReadScalar(mchar,name);
           mchar = 0;
           if(!buff->ReadScalar(mchar,commentName))
                buff->ReadScalar(mchar,name);              
           if(mchar){
             unsigned int len1=strlen(mchar);
             strncpy(ptr,mchar,len1);
             delete [] mchar;
           } else {
             *ptr='\0';
           }
      //    buff->ReadArray(mchar,len,name);
      //    cout << "In ReadArray " << len << " & " << mchar << endl;
      //    memcpy(ptr,mchar,len);

    break;
    }
  case Stuchar:
    {
       buff->ReadArray(muchar,len,name);
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

 cout <<"Descriptor for Table = " << mnode.name<<endl;
 cout <<" number of elements = "<<i<< " with size = " << size << endl;

 for(int k=0; k<i;k++){
   cout <<"Name = " << mdescriptor->getElementName(k);
   cout <<" size = " << mdescriptor->getElementSize(k);
   cout <<" offset = " <<mdescriptor->getElementOffset(k);
   cout <<" type = " <<(int)mdescriptor->getElementType(k)<<endl;
 }


}




