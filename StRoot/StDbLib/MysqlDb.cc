/***************************************************************************
 *
 * $Id: MysqlDb.cc,v 1.8 2000/03/28 17:03:18 porter Exp $
 *
 * Author: Laurent Conin
 ***************************************************************************
 *
 * Description: Mysql - SQL Query handler
 *
 ***************************************************************************
 *
 * $Log: MysqlDb.cc,v $
 * Revision 1.8  2000/03/28 17:03:18  porter
 * Several upgrades:
 * 1. configuration by timestamp for Conditions
 * 2. query by whereClause made more systematic
 * 3. conflict between db-stored comments & number lists resolved
 * 4. ensure endtime is correct for certain query falures
 * 5. dbstl.h->handles ObjectSpace & RogueWave difference (Online vs Offline)
 *
 * Revision 1.7  2000/03/01 20:56:15  porter
 * 3 items:
 *    1. activated reConnect for server timeouts
 *    2. activated connection sharing; better resource utilization but poorer
 *       logging
 *    3. made rollback method in mysqlAccessor more robust (affects writes only)
 *
 * Revision 1.6  2000/02/18 16:58:08  porter
 * optimization of table-query, + whereClause gets timeStamp if indexed
 *  + fix to write multiple rows algorithm
 *
 * Revision 1.5  2000/02/15 20:27:43  porter
 * Some updates to writing to the database(s) via an ensemble (should
 * not affect read methods & haven't in my tests.
 *  - closeAllConnections(node) & closeConnection(table) method to mgr.
 *  - 'NullEntry' version to write, with setStoreMode in table;
 *  -  updated both StDbTable's & StDbTableDescriptor's copy-constructor
 *
 * Revision 1.4  2000/01/27 05:54:31  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
 * Revision 1.3  1999/12/07 21:25:25  porter
 * some fixes for linux warnings
 *
 * Revision 1.2  1999/09/30 02:05:59  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "MysqlDb.h"

//#include "errmsg.h"

#define CR_MIN_ERROR            2000    /* For easier client code */
#define CR_MAX_ERROR            2999
#define CR_UNKNOWN_ERROR        2000
#define CR_SOCKET_CREATE_ERROR  2001
#define CR_CONNECTION_ERROR     2002
#define CR_CONN_HOST_ERROR      2003
#define CR_IPSOCK_ERROR         2004
#define CR_UNKNOWN_HOST         2005
#define CR_SERVER_GONE_ERROR    2006
#define CR_VERSION_ERROR        2007
#define CR_OUT_OF_MEMORY        2008
#define CR_WRONG_HOST_INFO      2009
#define CR_LOCALHOST_CONNECTION 2010
#define CR_TCP_CONNECTION       2011
#define CR_SERVER_HANDSHAKE_ERR 2012
#define CR_SERVER_LOST          2013
#define CR_COMMANDS_OUT_OF_SYNC 2014
#define CR_NAMEDPIPE_CONNECTION 2015
#define CR_NAMEDPIPEWAIT_ERROR 2016
#define CR_NAMEDPIPEOPEN_ERROR 2017
#define CR_NAMEDPIPESETSTATE_ERROR 2018

////////////////////////////////////////////////////////////////////////

MysqlDb::MysqlDb(): mdbhost(0), mdbName(0), mdbuser(0), mdbpw(0), mdbPort(0) {

mhasConnected=false;
mQuery=0;
mQueryMess=0;
mQueryLast=0;
RazQuery();
RazQuery();
//if (mRes) delete mRes;
mRes= new MysqlResult;
}
//////////////////////////////////////////////////////////////////////

MysqlDb::~MysqlDb(){
if(mQuery) delete [] mQuery;
if(mQueryMess) delete [] mQueryMess;
if(mQueryLast) delete [] mQueryLast;
Release();
if(mRes) delete mRes;
if(mhasConnected)mysql_close(&mData);

if(mdbhost) delete [] mdbhost;
if(mdbuser) delete [] mdbuser;
if(mdbpw) delete [] mdbpw;
if(mdbName) delete [] mdbName;

}


//////////////////////////////////////////////////////////////////////// 
bool MysqlDb::reConnect(){

bool tRetVal=false;
if(mysql_real_connect(&mData,mdbhost,mdbuser,mdbpw,mdbName,mdbPort,NULL,0)){ 
    tRetVal=true;
  } else {
    cerr << "Error Making Connection to DataBase = " << mdbName << endl;
    cerr << " MySQL returned error " << mysql_error(&mData) << endl;
  }

return tRetVal;
}

//////////////////////////////////////////////////////////////////////// 
bool MysqlDb::Connect(const char *aHost, const char *aUser, const char *aPasswd,  const char *aDb, const int aPort){
  

  if(mdbhost) delete [] mdbhost;
  mdbhost  = new char[strlen(aHost)+1];   strcpy(mdbhost,aHost);
  if(mdbuser) delete [] mdbuser;
  mdbuser  = new char[strlen(aUser)+1];   strcpy(mdbuser,aUser);
  if(mdbpw) delete [] mdbpw;
  mdbpw    = new char[strlen(aPasswd)+1]; strcpy(mdbpw,aPasswd);
  mdbPort  = aPort;

  if(mdbName) delete [] mdbName;
  mdbName  = new char[strlen(aDb)+1];     strcpy(mdbName,aDb);

  //  if(mhasConnected)mysql_close(&mData);
  bool tRetVal = false;
  if (!mysql_init(&mData)) {
    cout << &mData <<endl;
    cout << "Init Error : " << mysql_error(&mData) << endl;
  } else {
    if(mysql_real_connect(&mData,aHost,aUser,aPasswd,aDb,aPort,NULL,0)){ 
      // cout << "connected on " << aDb <<mysql_error(&mData) << 
      //	mysql_ping(&mData) <<endl;
      tRetVal=true;
    } else {
      cerr << "Error Making Connection to DataBase = " << aDb << endl;
      cerr << " MySQL returned error " << mysql_error(&mData) << endl;
    }
  }
  mhasConnected = tRetVal;
  return tRetVal;
}
////////////////////////////////////////////////////////////////////////

void MysqlDb::RazQuery() {
  
  if (mQueryLast) delete [] mQueryLast;
  mQueryLast=mQueryMess;
  if(mQuery) delete [] mQuery;  
  mQuery = new char[1];
  mQueryMess = new char[1];
  mQueryLen=0;
  strcpy(mQuery,"");
  strcpy(mQueryMess,"");
  
}

////////////////////////////////////////////////////////////////////////

bool MysqlDb::ExecQuery(){

bool tOk=false;
unsigned int mysqlError;


  if(!mysql_real_query(&mData,mQuery,mQueryLen)){ // no errors? ok store-&-return

     mRes->Release();
     mRes->mRes=mysql_store_result(&mData);
     tOk=true;
     mqueryState=true;
     return tOk;

  } else {

    mysqlError = mysql_errno(&mData);
    if(mysqlError==CR_SERVER_GONE_ERROR || mysqlError==CR_SERVER_LOST){

     reConnect();  

     if(mysql_real_query(&mData,mQuery,mQueryLen)){        

       cout << "Query Failed on DB="<<mData.db<< mQueryMess << endl;
       cout << "Returned Error : " << mysql_error(&mData) << endl;
 
       mqueryState=false;

     } else {

       mRes->Release();
       mRes->mRes=mysql_store_result(&mData);
       tOk=true;
       mqueryState=true;
     }

    } else {

      //         cout << " Query Error Number = " << mysqlError << endl;
      //           cout << "Query Failed : " << mQueryMess << endl;
      cout << "Query Failed on DB="<<mData.db<< mQueryMess << endl;
             cout << "Returned Error : " << mysql_error(&mData) << endl;
            mqueryState=false;
    }

  }


  return tOk;
}

////////////////////////////////////////////////////////////////////////

MysqlDb &MysqlDb::operator<<( const char *aQuery){ 
   
  //cout << "debug in ---"<< mQueryMess << "|" << mQueryLast << endl; 
  if (strcmp(aQuery,";")==0){
    ExecQuery();
    RazQuery();
  } else {

    char *tQuery = new char[mQueryLen+strlen(aQuery)+1];
    char *tQueryMess = new char[strlen(mQueryMess)+strlen(aQuery)+1];
    memcpy(tQuery,mQuery,mQueryLen);
    strcpy(&tQuery[mQueryLen],aQuery);
    strcpy(tQueryMess,mQueryMess);
    
    strcat(tQueryMess,aQuery);
    //cout <<"debug 2 --->"<< mQueryMess << "|" << mQueryLast <<"|" << tQueryMess << endl;
    if(mQuery)delete [] mQuery;
    mQuery=tQuery;
    mQueryLen=mQueryLen+strlen(aQuery);
    if(mQueryMess)delete [] mQueryMess;
    mQueryMess=tQueryMess;    
  };
  //cout <<"debug out --->"<< mQueryMess << "|" << mQueryLast << endl;
  return *this;
}

////////////////////////////////////////////////////////////////////////

MysqlDb &MysqlDb::operator<<( const MysqlBin *aBin ){

  
  const char *tMess="#BIN#";
  char *tQuery = new char[mQueryLen+aBin->mLen+1];
  char *tQueryMess = new char[strlen(mQueryMess)+strlen(tMess)+1];
  memcpy(tQuery,mQuery,mQueryLen);
  memcpy(&tQuery[mQueryLen],aBin->mBinData,aBin->mLen);
  tQuery[mQueryLen+aBin->mLen]='\0';
  strcpy(tQueryMess,mQueryMess);
  strcat(tQueryMess,tMess); 
  if(mQuery)delete [] mQuery;
  mQuery=tQuery;
  mQueryLen=mQueryLen+aBin->mLen;
  if(mQueryMess)delete [] mQueryMess;
  mQueryMess=tQueryMess;    
  
  return *this;
};

////////////////////////////////////////////////////////////////////////

bool MysqlDb::Input(const char *table,StDbBuffer *aBuff){
  bool tRetVal=false;
  bool change=aBuff->IsClientMode();
  if (change) aBuff->SetStorageMode();
  if (aBuff) {
    *this << "select * from " << table << " where null"<< endsql;
    *this << "insert into " << table << " set ";
    bool tFirst=true;
    char* tVal;tVal=0;
    int len; 
    unsigned i;

 for (i=0;i<NbFields();i++) {
    if ((IS_BLOB(mRes->mRes->fields[i].flags) ) || 
        mRes->mRes->fields[i].type ==254) {
      if (mRes->mRes->fields[i].flags&BINARY_FLAG) { // Binary
	    if (aBuff->ReadArray(tVal,len,mRes->mRes->fields[i].name)){
	       if (tFirst) {
               tFirst=false;
           } else {
               *this << ",";
           };
	         *this << mRes->mRes->fields[i].name << "='" << Binary(len,(float*)tVal)<<"'";
	   };
	 }else{  // text types
       if(mRes->mRes->fields[i].type==254){
 	     if (aBuff->ReadScalar(tVal,mRes->mRes->fields[i].name)) {
	       if (tFirst) {
               tFirst=false;
           } else {
               *this << ",";
           };
	       *this << mRes->mRes->fields[i].name << "='" << tVal << "'";
	     };
       } else {
	     char** tVal2=0;
	     if (aBuff->ReadArray(tVal2,len,mRes->mRes->fields[i].name)){
	       tVal=CodeStrArray(tVal2,len);
	       int j;for (j=0;j<len;j++) {if (tVal2[j]) delete [] tVal2[j];};
	       delete [] tVal2;
	       if (tFirst) {
               tFirst=false;
           } else {
               *this << ",";
           };
	       *this << mRes->mRes->fields[i].name << "='" << tVal<<"'";
         }
       };
	};
  } else {  // not binary nor text
	      if (aBuff->ReadScalar(tVal,mRes->mRes->fields[i].name)) {
	         if (tFirst) {
                 tFirst=false;
             } else {
                 *this << ",";
             };
	         *this << mRes->mRes->fields[i].name << "='" << tVal << "'";
	      };
      };
      if (tVal) delete [] tVal;tVal=0;
    };
    if (tFirst) { 
      RazQuery();
      cout << " Mysql : no matching field in Buffer" << endl;
    } else {
      *this << endsql;
      if(mqueryState)tRetVal=true;
    };
  };
  if (!tRetVal) cout << "insert Failed"<< endl;
  if (change) aBuff->SetClientMode();
  return tRetVal;
}; 

////////////////////////////////////////////////////////////////////////

bool  MysqlDb::Output(StDbBuffer *aBuff){


  MYSQL_ROW tRow=mysql_fetch_row(mRes->mRes);
  if(!tRow) return false;
  unsigned long * lengths=mysql_fetch_lengths(mRes->mRes);
  unsigned tNbFields=NbFields();
  int i;
  bool tRetVal=false;
  bool change=aBuff->IsClientMode();
  if (change) aBuff->SetStorageMode();
  if (tRow) {
    for (i=0;i<(int)tNbFields;i++){
      //      cout <<mRes->mRes->fields[i].name<<" = "<< tRow[i] << endl;
      if (IS_BLOB(mRes->mRes->fields[i].flags)) {
	    if (mRes->mRes->fields[i].flags&BINARY_FLAG) {
	       aBuff->WriteArray((char*)tRow[i],lengths[i],mRes->mRes->fields[i].name);
	    }else {
	       char** tStrPtr;
	       int len;
	       tStrPtr=DecodeStrArray((char*)tRow[i],len);
	       aBuff->WriteArray(tStrPtr,len,mRes->mRes->fields[i].name);
           for(int k=0;k<len;k++)delete tStrPtr[k];
           delete tStrPtr;

           // something of a cludge: "," are used in "Decode.."  
           // as array delimeters. But we also want to have
           // char arrays for comment structures - so write'em both
           // to the buffer. 

           char commentName[1024];
           ostrstream cn(commentName,1024);
           cn<<mRes->mRes->fields[i].name<<".text"<<ends;
	       aBuff->WriteScalar((char*)tRow[i],commentName);
	    };
      } else {
	       aBuff->WriteScalar((char*)tRow[i],mRes->mRes->fields[i].name);
      };
    };
    tRetVal=true;
  };
  if (change) aBuff->SetClientMode();
  return tRetVal;
}

////////////////////////////////////////////////////////////////////////

char** MysqlDb::DecodeStrArray(char* strinput , int &aLen){
  
  char* tPnt=strinput;
  aLen=0;
  while (tPnt&&aLen<100) {
    tPnt=strpbrk( tPnt,"\\,");
    if (tPnt!=0){
      if (*tPnt==',') {
	aLen++;tPnt++;
      } else { //(*tPnt=='\\') 
	tPnt++;tPnt++;};
    };
  };
  aLen++;
  char** strarr=new char*[aLen];
  tPnt=strinput;
  char* tPntNew=tPnt;
  char *tBuff=new char[strlen(strinput)+1];
  char *tBuffInd=tBuff;
  int tCount=0;
  while (tPntNew) {
    tPntNew=strpbrk( tPnt,"\\,");
    if ((tPntNew==0)||*tPntNew==',') {
      if (tPntNew==0) {
	strcpy(tBuffInd,tPnt);
	
      } else {
	strncpy(tBuffInd,tPnt,tPntNew-tPnt);
	*(tBuffInd+(tPntNew-tPnt))='\0';
      }
      strarr[tCount]=new char[strlen(tBuff)+1];
      strcpy(strarr[tCount],tBuff);
      tBuffInd=tBuff;
      tPnt=tPntNew+1;
      tCount++;
    } else { //(*tPntNew=='\\') 
      strncpy(tBuffInd,tPnt,tPntNew-tPnt);
      tBuffInd=tBuffInd+(tPntNew-tPnt);
      tPntNew++;
      if(*tPntNew=='\\'||*tPntNew==',') {
	*tBuffInd=*tPntNew;
	tBuffInd++;
      };
      *(tBuffInd)='\0';
      tPnt=tPntNew+1;
    };
  };
  delete [] tBuff;
  return strarr;
}

////////////////////////////////////////////////////////////////////////

char* MysqlDb::CodeStrArray(char** strarr , int aLen){
  int tMaxLen=0;
  int i;
  for (i=0;i<aLen;i++) {
    if (strarr[i]) tMaxLen=tMaxLen+strlen(strarr[i])*2;
    tMaxLen++;
  };
  char* tTempVal=new char[tMaxLen+1];
  char* tRead;
  char* tWrite=tTempVal;
  for (i=0;i<aLen;i++) {
    if (strarr[i]){
      int j;
      tRead=strarr[i];
      for (j=0;j<(int)strlen(strarr[i]);j++) {
	if (*tRead=='\\'||*tRead==',') {
	  *tWrite='\\';
	  tWrite++;
	};
	*tWrite=*tRead;
	tWrite++;
	tRead++;
      };
    };
    *tWrite=',';
    tWrite++;
  };
  tWrite--;
  *tWrite='\0';
  char *tRetVal=new char[strlen(tTempVal)+1];
  strcpy(tRetVal,tTempVal);
  if (tTempVal) delete [] tTempVal;
  return tRetVal;
};
  
/////////////////////////////////////////////////////

bool
MysqlDb::setDefaultDb(const char* dbName){

if(mdbName) delete [] mdbName;
mdbName=new char[strlen(dbName)+1];
strcpy(mdbName,dbName);

bool tOk=false;
unsigned int mysqlError;

 if(mysql_select_db(&mData,dbName)){

    mysqlError = mysql_errno(&mData);
    if(mysqlError==CR_SERVER_GONE_ERROR || mysqlError==CR_SERVER_LOST){
       reConnect();  
       if(mysql_select_db(&mData,dbName)){
         cerr<< "Error selecting database=" << dbName << endl;
       } else {
         tOk=true;
       }
    } else {
       cerr<< "Error selecting database=" << dbName << endl;
    }
 } else {
   tOk=true;

 }


return tOk;
}
  
////////////////////////////////////////////////////////////////////////

MysqlBin *Binary(const unsigned long int aLen,const float *aBin){
  
  MysqlBin *tBin=new MysqlBin;
  char *tString=new char[2*aLen+1];
  unsigned long int tNewLen=mysql_escape_string(tString,(char*) aBin,aLen);
  tBin->Input(tNewLen,tString);
  return tBin;
}




