/***************************************************************************
 *
 * $Id: MysqlDb.h,v 1.30 2016/05/25 20:17:51 dmitry Exp $
 *
 * Author: Laurent Conin
 ***************************************************************************
 *
 * Description: Mysql - SQL Query handler
 *
 ***************************************************************************
 *
 * $Log: MysqlDb.h,v $
 * Revision 1.30  2016/05/25 20:17:51  dmitry
 * coverity - uninit ctor
 *
 * Revision 1.29  2012/05/04 17:19:14  dmitry
 * Part One integration for Hyper Cache. HyperCache added to workflow, but config is set to DISABLE
 *
 * Revision 1.28  2011/01/07 18:19:02  dmitry
 * user name lookup is done once now (for speedup, based on profiler report)
 *
 * Revision 1.27  2007/10/29 22:50:56  deph
 * Abstracted load balancer call from connect to own function called from reconnect
 * removed extraneuos methods from header
 *
 * Revision 1.26  2007/01/09 16:27:39  deph
 * Updates for load balancing "added 1)write privilege 2)xml comments 3)camelCase notation
 *
 * Revision 1.25  2006/08/17 02:58:56  deph
 * updated load balancer - removing hard-coded nodes from API to xml
 *
 * Revision 1.24  2006/06/02 18:23:23  deph
 * Added an extra machine (db01) for analysis between 11pm and 7am
 *
 * Revision 1.22  2005/12/15 03:14:27  jeromel
 * Mem Leak fixes / Missing delete in new and stream context.
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
 * Revision 1.18  2003/07/02 18:39:24  porter
 * added server version to connection notification and host:port to connection failure notification
 *
 * Revision 1.17  2003/04/11 22:47:35  porter
 * Added a fast multi-row write model specifically needed by the daqEventTag
 * writer. Speed increased from about 100Hz to ~3000Hz.  It is only invoked if
 * the table is marked as Non-Indexed (daqTags & scalers). For non-indexed tables
 * which include binary stored data (we don't have any yet), the fast writer  has
 * to invoke a slower buffer so that the rates are a bit slower (~500Hz at 50 rows/insert).
 *
 * Revision 1.16  2002/03/22 19:05:38  porter
 * #-of-retries on server connect increased to 7 with timeout period doubled per
 * retry starting at 1 sec.  DOES NOT work (is ignored) on STAR's Redhat 6.2
 * version of mysqlclient but does on Redhat 7.2. Needed for maintainable
 * multiple mirror servers using dns for round-robin load balancing.
 *
 * Revision 1.15  2002/01/30 15:40:47  porter
 * changed limits on flavor tag & made defaults retrieving more readable
 *
 * Revision 1.14  2001/10/24 04:05:08  porter
 * added long long type to I/O and got rid of obsolete dataIndex table
 *
 * Revision 1.13  2001/04/25 17:17:33  perev
 * HPcorrs
 *
 * Revision 1.12  2001/03/30 18:48:26  porter
 * modified code to keep Insure from wigging-out on ostrstream functions.
 * moved some messaging into a StDbSql method.
 *
 * Revision 1.11  2001/02/09 23:06:24  porter
 * replaced ostrstream into a buffer with ostrstream creating the
 * buffer. The former somehow clashed on Solaris with CC5 iostream (current .dev)
 *
 * Revision 1.10  2001/01/22 18:37:50  porter
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
 * Revision 1.9  2000/03/01 20:56:15  porter
 * 3 items:
 *    1. activated reConnect for server timeouts
 *    2. activated connection sharing; better resource utilization but poorer
 *       logging
 *    3. made rollback method in mysqlAccessor more robust (affects writes only)
 *
 * Revision 1.8  2000/02/18 16:58:09  porter
 * optimization of table-query, + whereClause gets timeStamp if indexed
 *  + fix to write multiple rows algorithm
 *
 * Revision 1.7  2000/02/15 20:27:43  porter
 * Some updates to writing to the database(s) via an ensemble (should
 * not affect read methods & haven't in my tests.
 *  - closeAllConnections(node) & closeConnection(table) method to mgr.
 *  - 'NullEntry' version to write, with setStoreMode in table;
 *  -  updated both StDbTable's & StDbTableDescriptor's copy-constructor
 *
 * Revision 1.6  2000/01/27 05:54:32  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
 * Revision 1.5  2000/01/10 20:37:53  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
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

#include "stdb_streams.h"
#include "StDbBuffer.h"
#include "StDbLogger.hh"
#include "parseXmlString.hh"
#include "StDbManagerImpl.hh"
#include "StHyperCacheManager.h"

#include <string>

#ifdef HPUX
#define freeze(i) str()
#endif

#define endsql ";"
 

class StDbBuffer;

class MysqlResult {
  
private:
  MYSQL_RES * mRes;
  MYSQL_ROW mRow;
  char mSep = '\0';

public: 
  MysqlResult() {mRes=0;};
  //  virtual char float* NextRowBin();
  //virtual void Set(const MYSQL_RES *aPnt) {mRes=aPnt;};
  virtual unsigned NbRows () {return mysql_num_rows(mRes);};
  virtual unsigned NbFields () {return mysql_num_fields(mRes);};
  virtual void Release () {if(mRes)mysql_free_result(mRes);mRes=0;};
  MysqlResult &operator++() {mRow=mysql_fetch_row(mRes);return *this;};
  //  MysqlResult &operator >>(char *aString);

 protected:
  
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
  char* mQueryLast; // for printing query after execution
  MysqlResult* mRes;
  bool mqueryState;
  bool mhasConnected;
  bool mhasBinaryQuery;

  char* mdbhost;
  char* mdbName;
  char* mdbuser;
  char* mdbpw;
  int mdbPort;  
  char* mdbServerVersion;

  std::string mSysusername;

  unsigned int mtimeout; // wait time between connection tries


  // these are specific for fast multi-row writes of non-indexed tables
  int   jfields;
  char* cnames[200];
  bool  isBlob[200];
  bool  isBinary[200];
  bool  isSpecialType[200];
  bool loadBalance();

public:

  // for logging times
  bool mlogTime;
  StDbLogger mqueryLog;
  StDbLogger msocketLog;
  StDbLogger mconnectLog;

  MysqlDb();
  virtual ~MysqlDb();
  virtual bool Connect(const char *aHost, const char *aUser, 
	const char *aPasswd, const char *aDb, const int aPort=0);
  virtual bool reConnect();

  virtual unsigned NbRows() { 
	if (mqueryState) { 
		if (m_Mgr.isActive() && m_Mgr.isValueFound()) { return m_Mgr.getNumRows(); }
		return mRes->NbRows();   
	}; 
	return 0; 
  };

  virtual unsigned NbFields() { 
	if (mqueryState) { 
		if (m_Mgr.isActive() && m_Mgr.isValueFound()) { return m_Mgr.getNumFields(); }
		return mRes->NbFields(); 
	}; 
	return 0; 
  };

  virtual void Release() {mRes->Release();};

  virtual char* printQuery();
  //    virtual char* LastQuery();

  // these 3 meths are specific for fast multi-row writes of non-index tables
  virtual bool InputStart(const char *aName,StDbBuffer *aBuff, const char* colList, int nRows, bool& hasBinary);
  virtual bool InputRow(StDbBuffer* aBuff, int row);
  virtual bool InputEnd();


  virtual bool Input(const char *aName,StDbBuffer *aBuff);
  //virtual bool InitBuff(StDbBuffer *aBuff);
  virtual bool Output(StDbBuffer *aBuff);  
  MysqlDb &operator<<(const char *c);
  MysqlDb &operator<<(const string s);
  MysqlDb &operator<<(const MysqlBin *aBin);

  MysqlDb &operator<<(const short aq);
  MysqlDb &operator<<(const unsigned short aq);
  MysqlDb &operator<<(const int aq);
  MysqlDb &operator<<(const unsigned int aq);
  MysqlDb &operator<<(const long long aq);
  MysqlDb &operator<<(const float aq);
  MysqlDb &operator<<(const double aq);

  char **DecodeStrArray(char* strinput , int &aLen) ; 
  char* CodeStrArray(char** strarr , int aLen);
  virtual int GetLastInsertID(){ return (int)mysql_insert_id(&mData);}
  virtual bool QueryStatus() { return mqueryState; }
  virtual bool checkForTable(const char* tableName);
  virtual void Close();
  virtual bool IsConnected() { return mhasConnected; }
  virtual bool setDefaultDb(const char* dbName);

  StDbManagerImpl* my_manager; // need access to the list of servers and their properties for load balancing

protected:
  virtual void RazQuery() ;
  virtual bool ExecQuery();

  StHyperCacheManager m_Mgr;

  //virtual column* NextRow();    
  //virtual column* PrepareWrite();  
  //virtual void Print() {cout << mQueryMess << "|" <<mQueryLast <<endl;};  // debug
  //virtual void Out();  //debug
  //virtual MysqlResult* Result() { return mRes;}; //debug

  //ClassDef(MysqlDb,0)

};  

inline void MysqlDb::Close(){
  if(mhasConnected)mysql_close(&mData);
  mhasConnected=false;
}

inline MysqlDb &MysqlDb::operator<<( const string s){
  return *this<<s.c_str();
}

inline MysqlDb &MysqlDb::operator<<( const short aq){
  StString ts;  ts<<aq;
  return *this<<ts.str();
}

inline MysqlDb &MysqlDb::operator<<( const unsigned short aq){
  StString ts;  ts<<aq;
  return *this<<ts.str();
}

inline MysqlDb &MysqlDb::operator<<( const int aq){
  StString ts;  ts<<aq;
  return *this<<ts.str();
}

inline MysqlDb &MysqlDb::operator<<( const unsigned int aq){
  StString ts;  ts<<aq;
  return *this<<ts.str();
}

inline MysqlDb &MysqlDb::operator<<( const long long aq){
  StString ts;  ts<<aq;
  return *this<<ts.str();
}

inline MysqlDb &MysqlDb::operator<<( const float aq){
  StString ts;  ts<<aq;
  return *this<<ts.str();
}

inline MysqlDb &MysqlDb::operator<<( const double aq){
  StString ts;  ts<<aq;
  return *this<<ts.str();
}


extern const MysqlBin *Binary(const unsigned long int aLen,const float *aBin);
//extern MysqlBin *Binary(const unsigned long int aLen,const char  *aBin);
#endif







