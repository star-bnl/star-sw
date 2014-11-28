/***************************************************************************
 *
 * $Id: StDbBuffer.cc,v 1.25 2012/06/11 14:33:47 fisyak Exp $
 *
 * Author: Laurent Conin
 ***************************************************************************
 *
 * Description: Buffer for DB I/O
 *
 ***************************************************************************
 *
 * $Log: StDbBuffer.cc,v $
 * Revision 1.25  2012/06/11 14:33:47  fisyak
 * std namespace
 *
 * Revision 1.24  2007/08/20 18:21:28  deph
 * New Version of Load Balancer
 *
 * Revision 1.23  2007/06/17 16:05:34  deph
 * Bug fix for unsigned datatypes: max int was being returned  changed atoi to strtol.
 *
 * Revision 1.22  2007/05/16 22:48:10  deph
 * Replaced cerr with LOG_ERROR <<endm; for logger
 *
 * Revision 1.21  2004/01/15 00:02:24  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.20  2003/09/16 22:44:17  porter
 * got rid of all ostrstream objects; replaced with StString+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.19  2003/09/02 17:57:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.18  2003/04/11 22:47:35  porter
 * Added a fast multi-row write model specifically needed by the daqEventTag
 * writer. Speed increased from about 100Hz to ~3000Hz.  It is only invoked if
 * the table is marked as Non-Indexed (daqTags & scalers). For non-indexed tables
 * which include binary stored data (we don't have any yet), the fast writer  has
 * to invoke a slower buffer so that the rates are a bit slower (~500Hz at 50 rows/insert).
 *
 * Revision 1.17  2002/11/13 15:24:09  porter
 * updated strstream formatting
 *
 * Revision 1.16  2001/12/21 04:54:45  porter
 * sped up table definition for emc and changed some ostrstream usage for
 * insure tests
 *
 * Revision 1.15  2001/10/24 04:05:19  porter
 * added long long type to I/O and got rid of obsolete dataIndex table
 *
 * Revision 1.14  2001/04/25 17:18:10  perev
 * HPcorrs
 *
 * Revision 1.13  2001/03/30 18:48:26  porter
 * modified code to keep Insure from wigging-out on ostrstream functions.
 * moved some messaging into a StDbSql method.
 *
 * Revision 1.12  2001/01/22 18:37:50  porter
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
 * Revision 1.11  2000/06/30 01:57:00  porter
 * fixed a delete bug & small memory leak found by Akio via Insure++ ,
 * updated SetTable() method for containing idList, corrected enumeration
 * map to rhic domain for Conditions_rhic database
 *
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
#include "stdb_streams.h"

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

#ifdef HPUX
#define freeze(i) str()
#endif
using namespace std;
///////////////////////////////////////////////////////////////////////

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

///////////////////////////////////////////////////////////////////////
void StDbBuffer::zeroColumn(int istart, int iend) {
  for(int i=istart;i<iend+1;i++){
     mCol[i].name = 0;
     mCol[i].val  = 0;
  }
}


///////////////////////////////////////////////////////////////////////
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
	      delete [] tTextVal; 
      } else {
          delete [] mCol[i].val ;
      };
    };
    if (mCol[i].name) delete []  mCol[i].name;
    mCol[i].val=0; 
    mCol[i].name=0;
    mCol[i].type=_char;
    mCol[i].length=0; 
  };
  mCur=0;
  mLast=-1;
}

///////////////////////////////////////////////////////////////////////
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

///////////////////////////////////////////////////////////////////////
bool StDbBuffer::Find_Col (const   char *aName){
  // int tCount=0;

  if (mLast==-1) return false;
  
  for (int tCount=0;tCount<mLast+1;tCount++){
    mCur++;
    if (mCur>mLast) mCur=0; 

    if (!strcmp(mCol[mCur].name,aName)) return true;
  };

  return false;
}


///////////////////////////////////////////////////////////////////////
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

///////////////////////////////////////////////////////////////////////
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

///////////////////////////////////////////////////////////////////////
void StDbBuffer::MemSwapCpy(char* where,char* from,int len,int swaplen,BuffMode mode) {
  if (mode==Auto) mode=mMode;
  if (swaplen<=1||mode==Storage) {
    memcpy(where,from,len);
  } else {
    if (len%swaplen!=0) {
      LOG_ERROR << "memswapcy: len not in agreement with swapping - binary  truncate " << endm;
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

 
///////////////////////////////////////////////////////////////////////
void StDbBuffer::StrConv(char* aVal,char &s){s=aVal[0];};
void StDbBuffer::StrConv(char* aVal,unsigned char &s){s=(unsigned char)atoi(aVal);};
void StDbBuffer::StrConv(char* aVal,short &s){s=(short)atoi(aVal);};
void StDbBuffer::StrConv(char* aVal,unsigned short &s){s=(unsigned short) atoi(aVal);};
void StDbBuffer::StrConv(char* aVal,int &s){s=atoi(aVal);};
void StDbBuffer::StrConv(char* aVal,unsigned int &s){s=strtoul(aVal,0,10);};
void StDbBuffer::StrConv(char* aVal,long &s){s=atoi(aVal);};
void StDbBuffer::StrConv(char* aVal,unsigned long &s){s=strtoul(aVal,0,10);};
#ifndef __osf__
void StDbBuffer::StrConv(char* aVal,long long &s){s=atoll(aVal);};
#else
void StDbBuffer::StrConv(char* aVal,long long &s){s=atol(aVal);};
#endif
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
   castcase(_longlong,long long,tpe);\
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
genwritemem(long long);
genwritemem(float);
genwritemem(double);

#define castcasest(typelist,casttype) case typelist: {casttype tVal; MemSwapCpy((char*)&tVal,(char*)aVal,mycsize[typelist],mycswapl[typelist],Client);StString sStream;sStream.precision(10); sStream << tVal; string s2=sStream.str();char *tStr=new char[s2.length()+1];strcpy(tStr,s2.c_str());s[0]=tStr; };break

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
    castcasest(_longlong,long long);
    castcasest(_float,float);
    castcasest(_double,double);
  case _string: {char** tVal=(char**)aVal;
                 *s=new char[strlen(*(char**)aVal)+1];
                 strcpy(*s,*tVal);
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
Rscal(long long& );
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
Wscal(long long ,_longlong,1);
Wscal(float ,_float,1);
Wscal(double,_double,1);
//Wscal(char*,_string,1);

///////////////////////////////////////////////////////////////////////
bool StDbBuffer::WriteScalar(const char* s,const char *aName)
{
  if(!s) return false;
//  cout<< "Createin pointerstring "<<endl;
  //**  char** tVal=new char*[1];
  char* aVal=new char[strlen(s)+1];
  strcpy(aVal,s);
  char** tVal=&aVal;
//  cout<< "Creatin storag string "<<endl;
  //**  tVal[0]=new char[strlen(s)+1];
//  cout<< "Copy storag string "<<endl;
  //**  strcpy(tVal[0],s);
  /* if (Find_Col(aName))
   //  { ChangeField(_string,(void*)&tVal,1);}
  { ChangeField(_string,(void*)tVal,1); delete [] tVal[0]; delete [] tVal;}
 else
   { AddField(aName,_string,(void*)tVal,1); delete [] tVal[0]; delete [] tVal;}
   */
 if (Find_Col(aName))
  { ChangeField(_string,(void*)tVal,1); delete [] aVal;}
 else
   { AddField(aName,_string,(void*)tVal,1); delete [] aVal;};
return true;
}

///////////////////////////////////////////////////////////////////////
#define Rarray(tpe,tpelist) \
bool  StDbBuffer::ReadArray(tpe* &s, int &len,const char *aName)\
{ bool tRetVal=false; \
  bool newCheck=false;\
 if (Find_Col(aName)) \
  {int  i;\
  if (mCol[mCur].type==_char ) {\
    len=mCol[mCur].length/sizeof(tpe);\
    s=new tpe[len];\
    newCheck=true;\
    MemSwapCpy((char*)s,(char*)mCol[mCur].val,len*sizeof(tpe),mycswapl[tpelist],Auto);\
    tRetVal=true;\
  } else {\
    len=mCol[mCur].length;\
    s=new tpe[len];\
    newCheck=true;\
    for (i=0;i<len;i++)\
      { if (!(WriteMem(&s[i],(void*)(((char*)mCol[mCur].val)+i*mycsize[mCol[mCur].type]),mCol[mCur].type))) break;}\
      if (i==(int)mCol[mCur].length) tRetVal=true;}}\
 return tRetVal;\
}


//Rarray(char,_char);
Rarray(unsigned char,_uchar);
Rarray(short,_short );
Rarray(unsigned short,_ushort);
Rarray(int,_int   );
Rarray(unsigned int,_uint  );
Rarray(long,_long  );
Rarray(unsigned long,_ulong );
Rarray(long long,_longlong );
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
Warray(long long,_longlong );
Warray(float,_float );
Warray(double,_double);
Warray(char*,_string);













