#include "mysqlQuery.hh"

////////////////////////////////////////////////////////

mysqlQuery::mysqlQuery(){

mdbname = 0;
mquery = 0;
mselect = " SELECT ";
mfrom   = " FROM ";
mwhere  = " WHERE ";
minsert = " INSERT ";
mset    = " SET ";

}

/////////////////////////////////////////////////////////////

bool
mysqlQuery::init(const char* dbname){

bool retVal = false;
//if(!mserver)return retVal;

mysql_init(&mdata);

// if(mysql_real_connect(&mdata,mserver->getHostName(),"","",dbname,mserver->getPortNumber(),NULL,mserver->getUnixSocket())){
// if(mysql_real_connect(&mdata,"localhost","","",dbname,mserver->getPortNumber(),NULL,mserver->getUnixSocket())){
//  retVal = true;
// } else {
   cerr << "Error Making Connection to DataBase = " << dbname << endl;
   cerr << " MySQL returned error " << mysql_error(&mdata) << endl;
   // }

return retVal;

}

////////////////////////////////////////////////////////////////

bool
mysqlQuery::init(const char* dbname, const char* serverName, const char *hostName, int portNumber){

// a cludge for current tests

if(mdbname)delete [] mdbname;
mdbname = new char[strlen(dbname)+1];
strcpy(mdbname,dbname);

bool retVal = false;

mysql_init(&mdata);

// if(mysql_real_connect(&mdata,hostName,"","",dbname,portNumber,adummy.getUnixSocket(),0)){
//if(mysql_real_connect(&mdata,hostName,"","",dbname,0,NULL,0)){
if(mysql_real_connect(&mdata,hostName,"","",dbname,0,NULL,0)){
  retVal = true;
 } else {
   cerr << "Error Making Connection to DataBase = " << dbname << endl;
   cerr << " MySQL returned error " << mysql_error(&mdata) << endl;
 }
return retVal;
}

////////////////////////////////////////////////////////////////////////

void
mysqlQuery::setPredicateClause(const char* select, const char* from, const char* where){

char hquery[1024];
ostrstream ost(hquery,1024);
ost << mselect << select << mfrom << from << mwhere << where << ends;

 delete [] mquery;
 mquery = new char[strlen(hquery)+1];
 strcpy(mquery,hquery);

}

////////////////////////////////////////////////////////////////////

MYSQL_RES*
mysqlQuery::selectFromWhere(const char* select, const char* from, const char* where){

setPredicateClause(select,from,where);
// cout << "My Query is = " << mquery << endl;
// cout << "my database is =" << mdata.db << endl;
return execute();
}

//////////////////////////////////////////////////////////////////

MYSQL_RES*
mysqlQuery::insertSET(const char* tableName, const char* dataString){

  cout << minsert << endl << tableName << endl << mset << endl << dataString << endl;
  //char * hquery = new char[strlen(minsert)+strlen(tableName)+strlen(dataString)+ strlen(mset)+10];

char hquery[2048];
ostrstream ost(hquery,2048);

 ost << minsert << tableName << mset << dataString << ends;
 delete [] mquery;
 mquery = new char[strlen(hquery)+1];
 strcpy(mquery,hquery);
 cout << mquery << endl;
// delete [] hquery;

return execute();
}

/////////////////////////////////////////////////////////////////


MYSQL_RES*
mysqlQuery::execute(){

MYSQL_RES* result = NULL;
  if(mysql_real_query(&mdata,mquery,strlen(mquery))){
    cerr << "mysqlQuery Failed : " << mquery << endl;
    cerr << " MySQL returned Error " << mysql_error(&mdata) << endl;
    return result;
  }
 
return mysql_store_result(&mdata);
}











