/***************************************************************************
 *
 * $Id: StDbBuffer.cc,v 1.10 2000/06/02 15:58:26 porter Exp $
 *
 * Author: Laurent Conin
 ***************************************************************************
 *
 * Description: Buffer for DB I/O
 *
 ***************************************************************************
 *
 * $Log: StDbBuffer.cc,v $
 * Revision 1.10  2000/06/02 15:58:26  porter
 * removed "\" character in a "//" comment - it gives a warning on linux
 *
 * Revision 1.9  2000/06/02 13:37:36  porter
 * built up list of minor changes:
 *  - made buffer more robust for certain null inputs
 *  - fixed small leak in StDbTables & restructure call to createMemory
 *  - added dbRhic as a database domain in StDbDefs
 *  - added setUser() in StDbManager
 *  - added more diagnostic printouts in mysqlAccessor.cc
 *
 * Revision 1.8  2000/05/10 21:39:01  porter
 * fixed delete[] bug in reading from table where input schema includes fields that
 * are not in the database by checking buffer status for reads
 *
 * Revision 1.7  2000/02/18 22:11:58  porter
 * modified buffer read of unsigned char array
 *
 * Revision 1.6  2000/02/15 20:27:44  porter
 * Some updates to writing to the database(s) via an ensemble (should
 * not affect read methods & haven't in my tests.
 *  - closeAllConnections(node) & closeConnection(table) method to mgr.
 *  - 'NullEntry' version to write, with setStoreMode in table;
 *  -  updated both StDbTable's & StDbTableDescriptor's copy-constructor
 *
 * Revision 1.5  2000/01/27 05:54:32  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
 * Revision 1.4  1999/12/07 21:39:27  porter
 * *** empty log message ***
 *
 * Revision 1.3  1999/12/07 21:25:25  porter
 * some fixes for linux warnings
 *
 * Revision 1.2  1999/09/30 02:06:01  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "StDbBuffer.h"
#include <stdlib.h>
#include <string.h>
#include <iostream.h>
#include <strstream.h>
//#include <algorithm>



void StDbBuffer::Print(){
  if (mCol) {
    int i;
    for (i=0; i<=mLast;i++) {
      if (mCol[i].type==_char) {
	char* tVal;int len;
	ReadArray(tVal,len,mCol[i].name);
	cout << mCol[i].name <<"=" << "#BIN (" << len << ") bytes"  <<endl;	
      }else {
	char** tVal; int len;
	ReadArray(tVal,len,mCol[i].name);
	int j;
	for (j=0;j<len;j++)
	{cout << mCol[i].name <<"["<< j << "]=" << tVal[j]  <<endl;}
      };
    };
  };  
};

void
StDbBuffer::zeroColumn(int istart, int iend) {
  for(int i=istart;i<iend+1;i++){
     mCol[i].name = 0;
     mCol[i].val  = 0;
  }
}


void StDbBuffer::Raz(){
  int i;
  for (i=0;i<=mLast;i++) {
    if (mCol[i].val)  {
      if (mCol[i].type>=_ascii){
	char ** tTextVal= (char**)mCol[i].val;
	int j;
	for(j=0;j<(int)mCol[i].length;j++) {
	  if (tTextVal[j]) delete [] tTextVal[j];
	};
	delete [] tTextVal ; mCol[i].val=0;
      } else {
      delete [] mCol[i].val ;mCol[i].val=0; 
      };
    };
    if (mCol[i].name) delete []  mCol[i].name;mCol[i].name=0;
    //mCol[i].type=0;
    mCol[i].length=0; 
  };
  mCur=0;
  mLast=-1;
}

char **StDbBuffer::WhatsIn(){  
  char **tIn;
  tIn=new char*[mLast+2];
  int i;
  for (i=0;i<=mLast;i++){
    tIn[i]=mCol[i].name;
  };
  tIn[mLast+1]=0;
  return tIn;
}

     
    

bool StDbBuffer::Find_Col (const   char *aName){
  int tCount=0;

  if (mLast==-1) return false;
  
  for (tCount=0;tCount<mLast+1;tCount++){
    mCur++;
    if (mCur>mLast) mCur=0;  

    if (!strcmp(mCol[mCur].name,aName)) return true;
  };

  return false;
}


void StDbBuffer::AddField(const char *aName, const myctype aTpe,const void* aVal,const int aLen) {
  /*  when the field aName doesn't exist int the buffer, this function is call
      to initialise a new field. if needed, it increase the array mCol.
      it store the name, and then call ChangeField to store the value */
  
  if (mLast+1>mMax) {
    column *tCol=new column[mMax*2+1];
    memcpy(tCol,mCol,(mMax+1)*sizeof(column));
    if (mCol) delete [] mCol;
    mCol=tCol;
    zeroColumn(mMax+1,2*mMax);
    mMax=mMax*2;
  }
  mLast++;
  mCur=mLast;
  if(mCol[mCur].name) delete [] mCol[mCur].name;
  mCol[mCur].name=new char[strlen(aName)+1];
  strcpy(mCol[mCur].name,aName);
  mCol[mCur].val=0; 
  ChangeField(aTpe,aVal,aLen);
};

void StDbBuffer::ChangeField(const myctype aTpe,const void* aVal,const int aLen) {
  if (mCol[mCur].val){
   if(mCol[mCur].type>=_ascii){
	 char ** tTextVal= (char**)mCol[mCur].val;
	 int j;
	 for(j=0;j<(int)mCol[mCur].length;j++) {
	   if (tTextVal[j]) delete [] tTextVal[j];
	 };
	 delete [] tTextVal ; mCol[mCur].val=0;
    }
  }
  mCol[mCur].type=aTpe;
  mCol[mCur].length=aLen;
  if (aTpe<_ascii) {
    mCol[mCur].val=new char[aLen*mycsize[aTpe]];
    MemSwapCpy(mCol[mCur].val,(char*)aVal,aLen*mycsize[aTpe],mycswapl[aTpe],Auto);
  } else {
    mCol[mCur].val=(char*) new char*[aLen];
    char** tVal=(char**)aVal;
    char** tStoreVal=(char**)mCol[mCur].val;
    int i;
    for (i=0;i<aLen;i++) {
      if (tVal[i]==0) {
	(tStoreVal)[i]=0;
      } else {
	tStoreVal[i]=new char[strlen(tVal[i])+1];
	strcpy(tStoreVal[i],tVal[i]);
	
      };
    };
  };
};

void StDbBuffer::MemSwapCpy(char* where,char* from,int len,int swaplen,BuffMode mode) {
  if (mode==Auto) mode=mMode;
  if (swaplen<=1||mode==Storage) {
    memcpy(where,from,len);
  } else {
    if (len%swaplen!=0) {
      cerr << "memswapcy: len not in agreement with swapping - binary  truncate " << endl;
    };
    int tNbCpy=len/swaplen;
    int i;
    for (i=0;i<tNbCpy;i++){
      where+=swaplen; 
      int j;
      for (j=0;j<swaplen;j++){
	where--;
	*where=*from;
	from++;
      }
      where+=swaplen;
    };
  };      
};

 


void StDbBuffer::StrConv(char* aVal,char &s){s=aVal[0];};
void StDbBuffer::StrConv(char* aVal,unsigned char &s){s=(unsigned char)atoi(aVal);};
void StDbBuffer::StrConv(char* aVal,short &s){s=(short)atoi(aVal);};
void StDbBuffer::StrConv(char* aVal,unsigned short &s){s=(unsigned short) atoi(aVal);};
void StDbBuffer::StrConv(char* aVal,int &s){s=atoi(aVal);};
void StDbBuffer::StrConv(char* aVal,unsigned int &s){s=atol(aVal);};
void StDbBuffer::StrConv(char* aVal,long &s){s=atoi(aVal);};
void StDbBuffer::StrConv(char* aVal,unsigned long &s){s=atol(aVal);};
void StDbBuffer::StrConv(char* aVal,float &s){s=(float) atof(aVal);};
void StDbBuffer::StrConv(char* aVal,double &s){s=atof(aVal);};
void StDbBuffer::StrConv(char* aVal,char* &s){s=new char[strlen(aVal)+1];strcpy(s,aVal);};


#define castcase(typelist,casttype,tpe) case typelist: {casttype *tVal=(casttype*)aVal;casttype tValSwap;MemSwapCpy((char*)&tValSwap,(char*)tVal,mycsize[typelist],mycswapl[typelist],Client);*s=(tpe)tValSwap;};break;

#define genwritemem(tpe) \
bool StDbBuffer::WriteMem( tpe *s,void* aVal, myctype type)\
{bool tRetVal=true;\
 switch (type) {\
   castcase(_char,char,tpe);\
   castcase(_uchar,unsigned char,tpe);\
   castcase(_short,short,tpe);\
   castcase(_ushort,unsigned short,tpe);\
   castcase(_int,int,tpe);\
   castcase(_uint,unsigned int,tpe);\
   castcase(_long,long,tpe);\
   castcase(_ulong,unsigned long,tpe);\
   castcase(_float,float,tpe);\
   castcase(_double,double,tpe);\
 case _string: {char** tVal=(char**)aVal;StrConv(*tVal,*s);};break;\
 default: cout <<"wrong type" << endl;tRetVal=false;\
  };\
  return tRetVal;\
} 
genwritemem(char);
genwritemem(unsigned char);
genwritemem(short);
genwritemem(unsigned short);
genwritemem(int);
genwritemem(unsigned int);
genwritemem(long);
genwritemem(unsigned long);
genwritemem(float);
genwritemem(double);

#define castcasest(typelist,casttype) case typelist: {casttype tVal; MemSwapCpy((char*)&tVal,(char*)aVal,mycsize[typelist],mycswapl[typelist],Client);ostrstream sStream;sStream << tVal<<ends;char *tStr=new char[strlen(sStream.str())+1];strcpy(tStr,sStream.str());s[0]=tStr; delete [] sStream.str(); };break

bool StDbBuffer::WriteMem( char **s,void* aVal, myctype type) {
  bool tRetVal=true;
  
  switch (type) {
    castcasest(_char,char);
    castcasest(_uchar,unsigned char);
    castcasest(_short,short);
    castcasest(_ushort,unsigned short);
    castcasest(_int,int);
    castcasest(_uint,unsigned int);
    castcasest(_long,long);
    castcasest(_ulong,unsigned long);
    castcasest(_float,float);
    castcasest(_double,double);
  case _string: {char** tVal=(char**)aVal;
                 s[0]=new char[strlen(*(char**)aVal)+1];
                 strcpy(s[0],*tVal);
                 }
                 break;
  default: cout <<"wrong type" << endl;tRetVal=false;\
  };\
  return tRetVal;\
}   

			   

#define Rscal(tpe) \
bool  StDbBuffer::ReadScalar(tpe s,const char *aName) \
{bool tRetVal=false; \
  if(Find_Col(aName) && WriteMem(&s,mCol[mCur].val,mCol[mCur].type)) \
    tRetVal=true;\
  return tRetVal;}

// --> old 
// if (Find_Col(aName)) 
//  { char* tSwapVal=new char[mycsize[mCol[mCur].type]];
//  if(WriteMem(&s,mCol[mCur].val,mCol[mCur].type)) tRetVal=true;}
//  return tRetVal;}

Rscal(char&); 
Rscal(unsigned char&); 
Rscal( short& );
Rscal(unsigned short& );
Rscal(int&);
Rscal(unsigned int&);
Rscal(long&);
Rscal(unsigned long& );
Rscal(float& );
Rscal(double&);
Rscal(char*&);


#define Wscal(tpe,tpelist,len) \
bool StDbBuffer::WriteScalar(const tpe s,const char *aName) \
{ if (Find_Col(aName))\
  {ChangeField(tpelist,(void*)&s,len);}\
 else\
   {AddField(aName,tpelist,(void*)&s,len);};\
return true;\
}



Wscal(char,_char,1);
Wscal(unsigned char,_uchar,1);
Wscal(short,_short ,1);
Wscal(unsigned short,_ushort,1);
Wscal(int  ,_int ,1);
Wscal(unsigned int ,_uint ,1);
Wscal(long ,_long ,1);
Wscal(unsigned long ,_ulong,1);
Wscal(float ,_float,1);
Wscal(double,_double,1);
//Wscal(char*,_string,1);



bool StDbBuffer::WriteScalar(const char* s,const char *aName)
{
  if(!s) return false;
  char** tVal=new char*[1];
  tVal[0]=new char[strlen(s)+1];
  strcpy(tVal[0],s);
 if (Find_Col(aName))
   //  { ChangeField(_string,(void*)&tVal,1);}
  { ChangeField(_string,(void*)tVal,1); delete [] tVal[0]; delete [] tVal;}
 else
   { AddField(aName,_string,(void*)tVal,1); delete [] tVal[0]; delete [] tVal;};
return true;
}




#define Rarray(tpe,tpelist) \
bool  StDbBuffer::ReadArray(tpe* &s, int &len,const char *aName)\
{ bool tRetVal=false; \
 if (Find_Col(aName)) \
  {int  i;\
  if (mCol[mCur].type==_char ) {\
    len=mCol[mCur].length/sizeof(tpe);\
    s=new tpe[len];\
    MemSwapCpy((char*)s,(char*)mCol[mCur].val,len*sizeof(tpe),mycswapl[tpelist],Auto);\
  } else {\
    len=mCol[mCur].length;\
    s=new tpe[len];\
    for (i=0;i<len;i++)\
      { if (!(WriteMem(&s[i],(void*)(((char*)mCol[mCur].val)+i*mycsize[mCol[mCur].type]),mCol[mCur].type))) break;}\
      if (i==(int)mCol[mCur].length) tRetVal=true;};}\
 return tRetVal;\
}

//else {cerr<<"WARNING::field "<<aName<<" is not in this Buffer"<<endl;}

//Rarray(char,_char);
Rarray(unsigned char,_uchar);
Rarray(short,_short );
Rarray(unsigned short,_ushort);
Rarray(int,_int   );
Rarray(unsigned int,_uint  );
Rarray(long,_long  );
Rarray(unsigned long,_ulong );
Rarray(float,_float );
Rarray(double,_double);
Rarray(char*,_string);


bool  StDbBuffer::ReadArray(char* &s, int &len,const char *aName)
{ bool tRetVal=false; 
 if (Find_Col(aName)) {
   len=mCol[mCur].length*mycsize[mCol[mCur].type];
   s=new char[len];
   MemSwapCpy((char*)s,(char*)mCol[mCur].val,len,mycswapl[mCol[mCur].type],Auto);
   tRetVal=true;
 } else { 
   s=0;
   //   cerr << "WARNING:: field " << aName << " doesnt exist in this Buffer" << endl; 
 };
 return tRetVal;\
};

#define Warray(tpe,tpelist) \
bool StDbBuffer::WriteArray(tpe* s,int len,const char *aName) \
{ if (Find_Col(aName))\
  {ChangeField(tpelist,(void*)s,len);}\
 else\
   {AddField(aName,tpelist,(void*)s,len);};\
return true;\
} 

Warray(char,_char);
Warray(unsigned char,_uchar);
Warray(short,_short );
Warray(unsigned short,_ushort);
Warray(int,_int   );
Warray(unsigned int,_uint  );
Warray(long,_long  );
Warray(unsigned long,_ulong );
Warray(float,_float );
Warray(double,_double);
Warray(char*,_string);












