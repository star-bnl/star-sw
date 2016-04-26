/***************************************************************************
 *
 * $Id: StDbBuffer.h,v 1.10 2015/05/21 19:46:53 dmitry Exp $
 *
 * Author: Laurent Conin
 ***************************************************************************
 *
 * Description: Buffer that negotiates data I/O between mysql database
 *
 ***************************************************************************
 *
 * $Log: StDbBuffer.h,v $
 * Revision 1.10  2015/05/21 19:46:53  dmitry
 * fixed - unused internal pseudotype access considered as array out of bounds access
 *
 * Revision 1.9  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.8  2003/04/11 22:47:36  porter
 * Added a fast multi-row write model specifically needed by the daqEventTag
 * writer. Speed increased from about 100Hz to ~3000Hz.  It is only invoked if
 * the table is marked as Non-Indexed (daqTags & scalers). For non-indexed tables
 * which include binary stored data (we don't have any yet), the fast writer  has
 * to invoke a slower buffer so that the rates are a bit slower (~500Hz at 50 rows/insert).
 *
 * Revision 1.7  2001/10/26 20:59:46  porter
 * fixed new endtime flag from previous checkin. made StDataBaseI available
 * at root-cli.
 *
 * Revision 1.6  2001/10/24 04:05:19  porter
 * added long long type to I/O and got rid of obsolete dataIndex table
 *
 * Revision 1.5  2000/01/27 05:54:33  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
 * Revision 1.4  1999/10/19 14:30:37  porter
 * modifications relevant to use with StDbBroker and future merging with
 * "params" database structure + some docs + suppressing diagnostics messages
 *
 * Revision 1.3  1999/09/30 02:06:01  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBBUFFER_H
#define STDBBUFFER_H
#ifndef __CINT__
#include "mysql.h"
#include "mysql_com.h"
#endif

#include "StDbBufferI.h"

enum myctype{_char,_uchar,_short,_ushort,_int,_uint,_long,_ulong,_longlong,_float,_double,_ascii,_string};

const int mycsize[]={sizeof(char),sizeof(unsigned char),sizeof(short),sizeof(unsigned short),sizeof(int),sizeof(unsigned int),sizeof(long),sizeof(unsigned long),sizeof(long long),sizeof(float),sizeof(double),sizeof(char*),sizeof(char*)};

#if defined(__sun) && !defined(__i386) || defined(__hpux) || defined(__alpha) && !defined(linux)
const int mycswapl[]={1,1,1,1,4,4,4,4,8,4,8,1,1}; // same swapping than Solaris
#else
const int mycswapl[]={1,1,1,1,1,1,1,1,1,1,1,1,1};
#endif

struct column {
   char* name;
   enum myctype  type;
   char *val;
   unsigned length; };

enum BuffMode{Auto,Client,Storage};

class StDbBuffer : public StDbBufferI  { 


private:

  int mMax;
  int mCur;
  int mLast;
  column *mCol;
  BuffMode mMode;

  void zeroColumn(int istart, int iend);

public: 

  StDbBuffer(){
    mCur=0;
    mLast=-1;
    mMax=100;
    mCol= new column[mMax+1];
    zeroColumn(0,mMax);
    mMode=Storage;
  };

  StDbBuffer(unsigned int anum_fields, column *acol){
    Init( anum_fields,acol);}

  void Init(unsigned int anum_fields, column * acol){
    mCur=0;
    mLast=anum_fields;
    mCol=acol;};

  ~StDbBuffer(){Raz();delete [] mCol;  };  

  virtual void SetClientMode() {mMode=Client;};
  virtual void SetStorageMode() {mMode=Storage;};
  virtual bool IsClientMode() {return (mMode==Client) ? true : false;};
  virtual bool IsStorageMode() {return (mMode==Storage) ? true : false;};  
   
  void Raz();
  void Print();

  virtual char** WhatsIn(); // debug , not to be use

  virtual bool  ReadScalar(char   &c, const char *aName) ;
  virtual bool  ReadScalar(unsigned char  &c, const char *) ; 
  virtual bool  ReadScalar(short  &h, const char *) ;
  virtual bool  ReadScalar(unsigned short &h, const char *) ;
  virtual bool  ReadScalar(int    &i, const char *) ;
  virtual bool  ReadScalar(unsigned int   &i, const char *) ;
  virtual bool  ReadScalar(long   &l, const char *) ;
  virtual bool  ReadScalar(unsigned long  &l, const char *) ;
  virtual bool  ReadScalar(long long  &l, const char *) ;
  virtual bool  ReadScalar(float  &f, const char *) ;
  virtual bool  ReadScalar(double &d, const char *) ;
  virtual bool  ReadScalar(char   *&c, const char *) ; 
  
  virtual bool     WriteScalar(const char   c, const char *) ;
  virtual bool     WriteScalar(const unsigned char  c, const char *) ;
  virtual bool     WriteScalar(const short  h, const char *) ;
  virtual bool     WriteScalar(const unsigned short h, const char *) ;
  virtual bool     WriteScalar(const int    i, const char *) ;
  virtual bool     WriteScalar(const unsigned int   i, const char *) ;
  virtual bool     WriteScalar(const long   l, const char *) ;
  virtual bool     WriteScalar(const unsigned long  l, const char *) ;
  virtual bool     WriteScalar(const long long  l, const char *) ;
  virtual bool     WriteScalar(const float  f, const char *) ;
  virtual bool     WriteScalar(const double d, const char *) ;
  virtual bool     WriteScalar(const char  *c, const char *);

  virtual bool     ReadArray(char    *&c, int &len, const char *);
  virtual bool     ReadArray(unsigned char  *&c, int &len, const char *);
  virtual bool     ReadArray( short  *&c, int &len, const char *);
  virtual bool     ReadArray( unsigned short  *&c, int &len, const char *);
  virtual bool     ReadArray(int  *&c, int &len, const char *);
  virtual bool     ReadArray(unsigned int   *&c, int &len, const char *);
  virtual bool     ReadArray(long  *&c, int &len, const char *);
  virtual bool     ReadArray(unsigned long  *&c, int &len, const char *);
  virtual bool     ReadArray(long long  *&c, int &len, const char *);
  virtual bool     ReadArray(float   *&c, int &len, const char *);
  virtual bool     ReadArray(double  *&c, int &len, const char *);
  virtual bool     ReadArray(char  **&c, int &len, const char *);

  virtual bool     WriteArray(char    *c, int len, const char *);
  virtual bool     WriteArray(unsigned char   *c, int len, const char *);
  virtual bool     WriteArray(short   *c, int len, const char *);
  virtual bool     WriteArray(unsigned short  *c, int len, const char *);
  virtual bool     WriteArray(int     *c, int len, const char *);
  virtual bool     WriteArray(unsigned int    *c, int len, const char *);
  virtual bool     WriteArray(long    *c, int len, const char *);
  virtual bool     WriteArray(unsigned long   *c, int len, const char *);
  virtual bool     WriteArray(long long   *c, int len, const char *);
  virtual bool     WriteArray(float   *c, int len, const char *);
  virtual bool     WriteArray(double  *c, int len, const char *);
  virtual bool     WriteArray(char **c, int len, const char *);



protected:
  //char* Raw_result (const char *aName);

  bool Find_Col (const   char *aName);
  void AddField(const char *aName, const myctype aTpe,const void* aVal,const int aLen);
  void ChangeField(const myctype aTpe,const void* aVal,const int aLen);

//bool WriteMem(char &s,void* aVal, myctype type);

  bool WriteMem(char *s,void* aVal, myctype type);
  bool WriteMem(unsigned char *s,void* aVal, myctype type);
  bool WriteMem(short *s,void* aVal, myctype type);
  bool WriteMem(unsigned short *s,void* aVal, myctype type);
  bool WriteMem(int *s,void* aVal, myctype type);
  bool WriteMem(unsigned int *s,void* aVal, myctype type);
  bool WriteMem(long *s,void* aVal, myctype type);
  bool WriteMem(unsigned long *s,void* aVal, myctype type);
  bool WriteMem(long long *s,void* aVal, myctype type);
  bool WriteMem(float *s,void* aVal, myctype type);
  bool WriteMem(double *s,void* aVal, myctype type);
  bool WriteMem(char **s,void* aVal, myctype type);

  void StrConv(char* aVal,char &s);
  void StrConv(char* aVal,unsigned char &s);
  void StrConv(char* aVal,short &s);
  void StrConv(char* aVal,unsigned short &s);
  void StrConv(char* aVal,int &s);
  void StrConv(char* aVal,unsigned int &s);
  void StrConv(char* aVal,long &s);
  void StrConv(char* aVal,unsigned long &s);
  void StrConv(char* aVal,long long &s);
  void StrConv(char* aVal,float &s);
  void StrConv(char* aVal,double &s); 
  void StrConv(char* aVal,char* &s);

  void MemSwapCpy(char* where,char* from,int len,int swaplen,BuffMode mode);

  //ClassDef(StDbBuffer,0)


};



#endif






