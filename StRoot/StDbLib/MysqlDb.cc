/***************************************************************************
 *
 * $Id: MysqlDb.cc,v 1.2 1999/09/30 02:05:59 porter Exp $
 *
 * Author: Laurent Conin
 ***************************************************************************
 *
 * Description: Mysql - SQL Query handler
 *
 ***************************************************************************
 *
 * $Log: MysqlDb.cc,v $
 * Revision 1.2  1999/09/30 02:05:59  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "MysqlDb.h"



/*
char *MysqlResult::NextRowAscii(){
  unsigned i;
  unsigned nl=mysql_num_fields(mRes);
  MYSQL_ROW row =mysql_fetch_row(mRes);
  if (row) {
    char buf[4096]="";
    ostrstream ost(buf,4096);
    for (i=0;i<nl;i++) {
      ost <<row[i]<<"|";
    }
    ost << ends;
    char *ret=new char[strlen(buf)+1];
    strncpy(ret,buf,strlen(buf)+1);
    return ret;  }
  else {
    return 0;
  }
}
*/
////////////////////////////////////////////////////////
/*
MysqlResult &MysqlResult::operator>>(char *aString){

char *tString  ;
  if (aString) delete [] aString;
  tString=NextRowAscii();
  cout << tString <<endl; 
  
  return *this;
}
*/
////////////////////////////////////////////////////////////////////////

MysqlDb::MysqlDb(){


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
}




//////////////////////////////////////////////////////////////////////// 
bool MysqlDb::Connect(const char *aHost, const char *aUser, const char *aPasswd, 
		      const char *aDb, const int aPort){
  
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
  return tRetVal;
}
////////////////////////////////////////////////////////////////////////

void MysqlDb::RazQuery() {
  
  if (mQueryLast) delete [] mQueryLast;
  mQueryLast=mQueryMess;
  mQuery = new char[1];
  mQueryMess = new char[1];
  mQueryLen=0;
  strcpy(mQuery,"");
  strcpy(mQueryMess,"");
  
}

////////////////////////////////////////////////////////////////////////

bool MysqlDb::ExecQuery(){

bool tOk=false;

//cout <<"Attempting Query:: "<<mQuery << endl;
  if(mysql_real_query(&mData,mQuery,mQueryLen)){
    cout << "Query Failed : " << mQueryMess << endl;
    cout << "Returned Error : " << mysql_error(&mData) << endl;
    mqueryState=false;
  } else {
    //    cout << "Query : "<< mQueryMess <<endl;
    mRes->Release();
    mRes->mRes=mysql_store_result(&mData);
    tOk=true;
    mqueryState=true;
  }
  //  RazQuery();
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
/*
void MysqlDb::Out() {

unsigned i;

 for (i=0;i<mRes->NbRows();i++){
   cout << mRes->NextRowAscii() <<endl;
 };
};
*/
////////////////////////////////////////////////////////////////////////
/*
column *MysqlDb::NextRow(){
  MYSQL_ROW tRow=mysql_fetch_row(mRes->mRes);
  if (tRow) {
    unsigned tNbCol=mysql_num_fields(mRes->mRes);
    column *tCol = new struct column[tNbCol];
    long unsigned *lengths=mysql_fetch_lengths(mRes->mRes);
    unsigned i;
    for (i=0;i<tNbCol;i++) {
      tCol[i].name=new char[strlen(mRes->mRes->fields[i].name)+1];
      strcpy(tCol[i].name,mRes->mRes->fields[i].name);
      tCol[i].length=lengths[i];
      tCol[i].val=new char [ tCol[i].length];     
      memcpy(tCol[i].val,tRow[i],tCol[i].length);
      tCol[i].val[tCol[i].length]='\0';
      tCol[i].type=mRes->mRes->fields[i].type;
    };
    return tCol;
  } else {
    return 0;
  };
};

*/

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
      if  ((IS_BLOB(mRes->mRes->fields[i].flags) ) || mRes->mRes->fields[i].type ==254) {
	if (mRes->mRes->fields[i].flags&BINARY_FLAG) {
	  if (aBuff->ReadArray(tVal,len,mRes->mRes->fields[i].name)){
	    if (tFirst) {tFirst=false;} else {*this << ",";};
	    *this << mRes->mRes->fields[i].name << "='" << Binary(len,(float*)tVal)<<"'";
	  };
	}else{
      if(mRes->mRes->fields[i].type==254){
 	  if (aBuff->ReadScalar(tVal,mRes->mRes->fields[i].name)) {
	    if (tFirst) {tFirst=false;} else {*this << ",";};
	    *this << mRes->mRes->fields[i].name << "='" << tVal << "'";
	  };
          } else {
	  char** tVal2=0;
	  if (aBuff->ReadArray(tVal2,len,mRes->mRes->fields[i].name)){
	    tVal=CodeStrArray(tVal2,len);
	    int j;for (j=0;j<len;j++) {if (tVal2[j]) delete [] tVal2[j];};
	    delete [] tVal2;
	    if (tFirst) {tFirst=false;} else {*this << ",";};
	    *this << mRes->mRes->fields[i].name << "='" << tVal<<"'";
	  }
     };
	};
      }else {
	if (aBuff->ReadScalar(tVal,mRes->mRes->fields[i].name)) {
	  if (tFirst) {tFirst=false;} else {*this << ",";};
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

/*
bool  MysqlDb::InitBuff(StDbBuffer *aBuff){
  column* tNRow=PrepareWrite();
  if (tNRow) {
    aBuff->Init(NbFields(),tNRow);
    return 1;
  } else {
    return 0;
  }
}
*/
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
    for (i=0;i<tNbFields;i++){
      if (IS_BLOB(mRes->mRes->fields[i].flags)) {
	    if (mRes->mRes->fields[i].flags&BINARY_FLAG) {
	       aBuff->WriteArray((char*)tRow[i],lengths[i],mRes->mRes->fields[i].name);
	    }else {
	       char** tStrPtr;
	       int len;
	       tStrPtr=DecodeStrArray((char*)tRow[i],len);
	       aBuff->WriteArray(tStrPtr,len,mRes->mRes->fields[i].name);
	    };
      } else {
        // cout << " Writing Scalar Named = " << mRes->mRes->fields[i].name << endl;
	       aBuff->WriteScalar((char*)tRow[i],mRes->mRes->fields[i].name);
      };
    };
    tRetVal=true;
  };
  if (change) aBuff->SetClientMode();
  return tRetVal;
}



////////////////////////////////////////////////////////////////////////

/*
column *MysqlDb::PrepareWrite(){
  unsigned tNbCol=mysql_num_fields(mRes->mRes);
  if (tNbCol){
     column *tCol = new struct column[tNbCol];
    unsigned i;
    for (i=0;i<tNbCol;i++) {
      tCol[i].name=new char[strlen(mRes->mRes->fields[i].name)+1];
      strcpy(tCol[i].name,mRes->mRes->fields[i].name);
      tCol[i].val=0;
      tCol[i].length=0;
      tCol[i].type=mRes->mRes->fields[i].type;

    };
    return tCol;
  }else {
    return 0;
  };
};
      
*/
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
      for (j=0;j<strlen(strarr[i]);j++) {
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
  
      
      
				    
  

  

////////////////////////////////////////////////////////////////////////

MysqlBin *Binary(const unsigned long int aLen,const float *aBin){
  
  MysqlBin *tBin=new MysqlBin;
  char *tString=new char[2*aLen+1];
  unsigned long int tNewLen=mysql_escape_string(tString,(char*) aBin,aLen);
  tBin->Input(tNewLen,tString);
  return tBin;
}




