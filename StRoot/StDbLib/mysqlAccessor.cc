#include "mysqlAccessor.hh"
#include <strings.h>

////////////////////////////////////////////////////////////

int
mysqlAccessor::QueryDb(StDbConfigNode* node){


char* keyTable = getKeyName(node->getName());
char* queryString = prepareConfigQuery(node->getConfigName());
mresult = mquery.selectFromWhere("*",keyTable,queryString);
 if(mresult){
   int mnum_rows = mysql_num_rows(mresult);
   if(mnum_rows != 1){
     cerr << "Cannot Fullfil Query " << mnum_rows << endl;
     return 0;
   }
     
     mrow = mysql_fetch_row(mresult);
     mnum_fields = mysql_num_fields(mresult);
     mfields = mysql_fetch_fields(mresult);

     int iversion;
     int elementID;
     StDbConfigNode* newNode;
  

     for(int i=1;i<mnum_fields;i++){ // Table Name & Version
       //cout << i << " field = " << mfields[i].name << endl;
       //cout << i << " row = " << mrow[i] << endl;
       if(strlen(mrow[i])==0)continue;  // null field
       //cout << i << " field = " << mfields[i].name << endl;

       if(isConfig(mfields[i].name)){ // it is a configKey

         char* nodeName = getNodeName(mfields[i].name);
         newNode = new StDbConfigNode(node,nodeName,mrow[i]);

         } else {
         
         elementID = getElementID(node->getName());
         //cout << "element id = " << elementID << endl;
         iversion = getVersionIndex(mrow[i]);
         //cout << " version id = " << iversion << endl;
         node->addTable(mfields[i].name,iversion,elementID);
         //cout << "Table = " << mfields[i].name << " is added" << endl;
       } // !isConfig


     } // table/config loop
   } // if(mresult)

return 1;
}

////////////////////////////////////////////////////////////////

int
mysqlAccessor::QueryDb(StDbTableComponent* table){

char* tableName = table->getTableName();
int iversion = table->getVersion();
int requestTime = table->getRequestTime();
char* queryString = prepareDataQuery(requestTime,iversion);

mresult = mquery.selectFromWhere("*",tableName,queryString);


 if(mresult){

   int mnum_rows = mysql_num_rows(mresult);
  
  if(mnum_rows != 1) { 
    cerr << "Cannot Fullfil Query " << mnum_rows << endl;
    return 0;
  } else {
   mrow = mysql_fetch_row(mresult);
   mnum_fields = mysql_num_fields(mresult);
   //int nextcolumn = fillMetaData(mrow);

   int nextcolumn = 1;

   mfields = mysql_fetch_fields(mresult);
   unsigned long* col_length = mysql_fetch_lengths(mresult);
   if(mReader->initArray(mrow,mfields,nextcolumn,mnum_fields,col_length)){
        return nextcolumn;
   } else {
     return 0;
   }
  }

 }  

return 1;
}

////////////////////////////////////////////////////////////////

int
mysqlAccessor::WriteDb(StDbTableComponent* table){

char* tableName = table->getTableName();
int iversion = table->getVersion();
//int requestTime = table->getRequestTime();
char* queryString = prepareDataQuery(0,iversion);

mresult = mquery.selectFromWhere("*",tableName,queryString);


 if(mresult){

   int mnum_rows = mysql_num_rows(mresult);
  
  if(mnum_rows != 1) { 
    cerr << "Cannot Fullfil Query " << mnum_rows << endl;
    return 0;
  } else {
   mrow = mysql_fetch_row(mresult);
   mnum_fields = mysql_num_fields(mresult);
   //int nextcolumn = fillMetaData(mrow);

   mrow[0] = NULL; // to increment autocounter
   int nextcolumn = 1;

   mfields = mysql_fetch_fields(mresult);
   unsigned long* col_length = mysql_fetch_lengths(mresult);
   if(mWriter->initArray(mrow,mfields,nextcolumn,mnum_fields,col_length)){
        return nextcolumn;
   } else {
     return 0;
   }
  }

 }  

return 1;
}

////////////////////////////////////////////////////////////////

bool
mysqlAccessor::isConfig(const char* name){
 bool retVal = false;

 char* teststr = strstr(name,"Keys");
 if(teststr)retVal=true;

 return retVal;
}


////////////////////////////////////////////////////////////////

char*
mysqlAccessor::getKeyName(const char* nodeName){

 char* keys = "Keys";
 int len = strlen(nodeName)+strlen(keys);
 char* keyName = new char[len+1];
 ostrstream os(keyName,len+1);
 os << nodeName << "Keys" << ends;
 // strcat(keyName,nodeName);
 // strcat(keyName,keys);

return strdup(keyName);
}

////////////////////////////////////////////////////////////////

char*
mysqlAccessor::getNodeName(const char* keyName){

char* nodeName=0;
char* aname = new char[strlen(keyName)+1];
strcpy(aname,keyName);

 char* id = strstr(aname,"Keys");
 if(!id)return nodeName;
 int i = id-aname;

 char* p= &aname[0];
 int size = i;
 nodeName = new char[size];
 strncpy(nodeName,p,size);
 nodeName[size]='\0';

return strdup(nodeName);
}

////////////////////////////////////////////////////////////////

int
mysqlAccessor::getElementID(const char* nodeName){

char* id=strstr(nodeName,"_");
if(!id)return 0;
id++;
return atoi(id);
}

////////////////////////////////////////////////////////////

int
mysqlAccessor::fillMetaData(MYSQL_ROW row){

  if(row){ // note row[0] is simply a counter so skip it
           // row[1] & row[2] are object & struct names (sanity checks) 
//      mmetaData->setVersion(atoi(row[3]));
//      mmetaData->setBeginTime(atoi(row[1]));
//      mmetaData->setEndTime(atoi(row[2]));
      //      mmetaData->setFileType(atoi(row[4]));  
    } else {
      cerr << "fillMetaData: row is Null" << endl;
      return 0;
    }

return 4;
}

////////////////////////////////////////////////////////////

char*
mysqlAccessor::prepareDataQuery(int timeStamp, int version){

char hquery[1024];
ostrstream ost(hquery,1024);

 ost << " beginTime<=" << timeStamp << " AND endTime>" << timeStamp << " AND version=" << version << ends;

 char * queryString = new char[strlen(hquery)+1];
 strcpy(queryString,hquery);

return strdup(queryString);

}


char*
mysqlAccessor::prepareInitialQuery(int icount, int version){

char hquery[1024];
ostrstream ost(hquery,1024);

 ost << " count=" << icount << " AND version=" << version << ends;

 char * queryString = new char[strlen(hquery)+1];
 strcpy(queryString,hquery);

return strdup(queryString);

}

char*
mysqlAccessor::prepareConfigQuery(const char* configName){

char hquery[1024];
ostrstream ost(hquery,1024);

 ost << " KeyName='" << configName << "'"<< ends; 

 char * queryString = new char[strlen(hquery)+1];
 strcpy(queryString,hquery);

return strdup(queryString);

}

bool
mysqlAccessor::execute(const char* name){

  //char dataString;

 ostrstream ost;
 if(mrow){ 
    for(int i=0; i<mnum_fields-1;i++){
      ost << mrow[i] << ",";
    }
    ost << mrow[mnum_fields-1] << ends;
    mquery.insertSET(name,ost.str());
  }
return true;
}











