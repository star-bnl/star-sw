/***************************************************************************
 *
 * $Id: MysqlDb.h,v 1.4 1999/10/19 14:30:37 porter Exp $
 *
 * Author: Laurent Conin
 ***************************************************************************
 *
 * Description: Mysql - SQL Query handler
 *
 ***************************************************************************
 *
 * $Log: MysqlDb.h,v $
 * Revision 1.4  1999/10/19 14:30:37  porter
 * modifications relevant to use with StDbBroker and future merging with
 * "params" database structure + some docs + suppressing diagnostics messages
 *
 * Revision 1.3  1999/09/30 02:06:00  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef MYSQDB_HH
#define MYSQDB_HH
#ifndef __CINT__
#include "mysql.h"
#else
typedef  char  MYSQL_RES;
typedef  char MYSQL_ROW;
typedef  char MYSQL;
typedef  int MYSQL_FIELD;
#endif
#include <iostream.h>
#include <strstream.h>
#include <string.h>
#include "StDbBuffer.h"


#define endsql ";"
 

class StDbBuffer;

class MysqlResult {
  
private:
  MYSQL_RES * mRes;
  MYSQL_ROW mRow;
  char mSep;
  
public: 
  MysqlResult() {mRes=0;};
  //  virtual char float* NextRowBin();
  //virtual void Set(const MYSQL_RES *aPnt) {mRes=aPnt;};
  virtual unsigned NbRows () {return mysql_num_rows(mRes);};
  virtual unsigned NbFields () {return mysql_num_fields(mRes);};
  virtual void Release () {mysql_free_result(mRes);mRes=0;};
  MysqlResult &operator++() {mRow=mysql_fetch_row(mRes);return *this;};
  //  MysqlResult &operator >>(char *aString);

 protected:
  //virtual MYSQL_RES *Res () {return mRes;}; // debug  - must not be use
  //virtual MYSQL_ROW Row () {return mRow;}; // debug  - must not be use
  // virtual char* NextRowAscii();
  
  friend class MysqlDb; 	      
};

class MysqlBin{
  
private:
  char *mBinData;
  unsigned  long int mLen;
  
public:
  MysqlBin() {mBinData=0;mLen=0;};
  virtual void Input(const unsigned long int aLen,char *aBin) 
    {mBinData=aBin;mLen=aLen;};
  
  friend class MysqlDb;
};


class MysqlDb{
  
private:
  
  MYSQL mData;
  char* mQuery;
  unsigned long int mQueryLen;
  char* mQueryMess;
  char* mQueryLast;
  MysqlResult* mRes;
  bool mqueryState;
  
public:
  MysqlDb() ;
  virtual ~MysqlDb();
  virtual bool Connect(const char *aHost, const char *aUser, 
	const char *aPasswd, const char *aDb, const int aPort=0);

  virtual unsigned NbRows () {return mRes->NbRows();};
  virtual unsigned NbFields () {return mRes->NbFields();};
  virtual void Release() {mRes->Release();};

  //  virtual char* Query();
  //    virtual char* LastQuery();

  virtual bool Input(const char *aName,StDbBuffer *aBuff);
  //virtual bool InitBuff(StDbBuffer *aBuff);
  virtual bool Output(StDbBuffer *aBuff);  
  MysqlDb &operator<<(const char *c);
  MysqlDb &operator<<(const MysqlBin *aBin);
  char **DecodeStrArray(char* strinput , int &aLen) ; 
  char* CodeStrArray(char** strarr , int aLen);
  virtual int GetLastInsertID(){ return (int)mysql_insert_id(&mData);}

protected:
  virtual void RazQuery() ;
  virtual bool ExecQuery();
  //virtual column* NextRow();    
  //virtual column* PrepareWrite();  
  //virtual void Print() {cout << mQueryMess << "|" <<mQueryLast <<endl;};  // debug
  //virtual void Out();  //debug
  //virtual MysqlResult* Result() { return mRes;}; //debug

  //ClassDef(MysqlDb,0)

};  


extern MysqlBin *Binary(const unsigned long int aLen,const float *aBin);
//extern MysqlBin *Binary(const unsigned long int aLen,const char  *aBin);
#endif

