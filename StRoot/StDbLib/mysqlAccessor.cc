#include "mysqlAccessor.hh"
#include "StDbTableDescriptor.h"
#include <strings.h>

////////////////////////////////////////////////////////////

int
mysqlAccessor::QueryDb(StDbConfigNode* node){
  
  char* configName = node->getConfigName();
  char* keyName = getKeyName(configName);

    Db<< "SELECT NodeKeys.ID FROM NodeKeys WHERE NodeKeys.KeyName='";
    Db<<configName<<"' AND NodeKeys.Name='"<<keyName<<"'"<<endsql;

    Db.Output(&buff);

    int TableID;
    buff.SetClientMode();
    //    buff.Print();
    if(!buff.ReadScalar(TableID,"ID")){
      cerr <<"QueryDb::Config: No Key = "<< configName<< " found " << endl;
      return 0;
    }

   Db.Release(); 
   buff.Raz();

    //    Db<<"SELECT NodeKeys.ID, Nodes.NodeName, Nodes.ID, ";
    //    Db<<"Nodes.versionName, Nodes.elementID, NodeRelation.ID ";
    Db<<"SELECT Nodes.NodeName, ";
    Db<<"Nodes.versionName, Nodes.elementID ";
    Db<<"FROM NodeKeys LEFT JOIN NodeRelation ON ";
    Db<<"NodeKeys.ID=NodeRelation.KeyID LEFT JOIN Nodes ON ";
    Db<<"NodeRelation.NodeID=Nodes.ID WHERE NodeKeys.KeyName='";
    Db<< configName <<"' AND NodeKeys.Name='"<<keyName<<"'"<<endsql;

    delete [] configName;

  if(Db.NbRows() == 0){
    cerr << "No Rows Satisfying Query " << Db.NbRows()<< endl;
    Db.Release();
    return 0;
  }

  int elementID;
  char* eID;
  char* version;
  StDbConfigNode* newNode;
  //  cout << "Will Loop through results " << endl;
  while(Db.Output(&buff)){
   //    cout << "loop check " << chck << endl;
   //      buff.Print();

     buff.SetClientMode();

   char* ConfigName = 0;

   if(!buff.ReadScalar(ConfigName,"NodeName")){ 
     delete [] ConfigName;
     buff.Raz();
     continue;
   }

   //   cout << ConfigName << endl;
    if(isConfig(ConfigName)){ // it is a configKey
      //      cout << "is config " << ConfigName << endl;
      char* nodeName = getNodeName(ConfigName);
      delete [] ConfigName;
      if(!buff.ReadScalar(ConfigName,"versionName"))cout << "Config err reading version" << endl; 
      newNode = new StDbConfigNode(node,nodeName,ConfigName);
      delete [] nodeName;
      delete [] ConfigName;

    } else { // it is a table
      //  cout << "is Table " << ConfigName << endl;
      //      buff.Print();
      
      if(!buff.ReadScalar(version,"versionName"))cout << "Table err reading versoin" << endl;
      if(!buff.ReadScalar(eID,"elementID"))cout << "Table err reading elementID " << endl;
      elementID = getElementID(eID);
      
      StDbTable* table = node->addTable(ConfigName,version,elementID);
  
      //     QueryDescriptor(table);
      delete [] ConfigName;
      delete [] version;
      delete [] eID;
    } // !isConfig

  buff.Raz();

  } // table/config loop

  Db.Release();
  return 1;
}

////////////////////////////////////////////////////////////////

int
mysqlAccessor::QueryDb(StDbTable* table){


  char* tableName = table->getTableName();

  if(!table->hasDescriptor())QueryDescriptor(table);

  // now query for data. This will return both the data-row and the
  // index row as 1 row.

  Db<< "Select * from " << tableName <<" LEFT JOIN " <<tableName<<"Index on ";
  Db<<  tableName<<"Index.dataID="<<tableName<<".dataID";
 
  char* version = table->getVersion();
  int requestTime = table->getRequestTime();
  char rTime[80];

  ostrstream os(rTime,80);
  os<<requestTime<<ends;

  Db<<" Where beginTime<="<<rTime<<" AND " <<"endTime>"<<rTime;
  Db<<" AND version='"<<version<<"'"<<endsql;

  if(Db.Output(&buff)){
   table->StreamAccessor(&buff,true);
   table->dbStreamer(&buff,true);
   buff.Raz();
  } else {
    buff.Raz();
    Db.Release();
    cerr << "QueryDb::Table: Error in data query" << endl;
    return 0;
  }
   
  delete [] tableName;
  Db.Release();

  return 1; 
}  


////////////////////////////////////////////////////////////////

int
mysqlAccessor::WriteDb(StDbTable* table){
  
  char* tableName = table->getTableName();
  //int iversion = table->getVersion();
  //int requestTime = table->getRequestTime();
  //char* queryString = prepareDataQuery(0,iversion);
  
  Db << "select * from " << tableName << " where null" << endsql;
  //  Db.InitBuff(&buff);
  Db.Release();

  
  return 1;
}

////////////////////////////////////////////////////////////////
int
mysqlAccessor::QueryDescriptor(StDbTable* table){


  if(!table->hasDescriptor()){

    char* tableName=table->getTableName();
    cout << "Getting Descriptor for table = " << tableName << endl;  

    Db<< "SELECT structure.KeyID FROM structure WHERE structure.name='";
    Db<<tableName<<"'"<<endsql;
    Db.Output(&buff);
    int TableID;
    buff.SetClientMode();
    if(!buff.ReadScalar(TableID,"KeyID")){
      cerr <<"QueryDb::Table: No structure = "<< tableName<< " found " << endl;
      return 0;
    }

    Db.Release();
    buff.Raz();

    Db<<"SELECT structure.KeyID, schema.name, ";
    Db<<"schema.mask, schema.type, schema.length, ";
    Db<<"relation.position FROM structure LEFT JOIN relation ON ";
    Db<<"structure.ID=relation.structID LEFT JOIN schema ON ";
    Db<<"relation.memberID=schema.ID WHERE structure.name='";
    Db<< tableName <<"' ORDER BY relation.position"<<endsql;
    
    StDbTableDescriptor* descriptor = 0;
    int tabID = table->getTableID();

    while(Db.Output(&buff)){
      if(!descriptor)descriptor = new StDbTableDescriptor();
      descriptor->fillElement(&buff,tabID);
      buff.Raz();
    }

    if(descriptor){
      table->setDescriptor(descriptor);    
      //      cout << "Setting Descriptor into StDbTable " << endl;
      //      cout << "check " << table->hasDescriptor() << endl;
    }
    Db.Release();
    delete [] tableName;
  }

return 1;

}


////////////////////////////////////////////////////////////////

bool
mysqlAccessor::isConfig(const char* name){
 bool retVal = false;
 // cout << "isConfig:: " << name << endl;
 char* teststr = strstr(name,"Keys");
 // cout << " isCongif:: teststr" << teststr << endl;
 if(teststr)retVal=true;

 return retVal;
}


////////////////////////////////////////////////////////////////

char*
mysqlAccessor::getKeyName(const char* nodeName){
  
  // appends "Keys" to node name
  char* keys = "Keys";
  int len = strlen(nodeName)+strlen(keys);
  char* keyName = new char[len+1];
  ostrstream os(keyName,len+1);
  os << nodeName << "Keys" << ends;
  return keyName;
  delete [] keys; 
}

////////////////////////////////////////////////////////////////

char*
mysqlAccessor::getNodeName(const char* keyName){

  // strips "Keys" from keyName

char* nodeName=0;
char* aname = new char[strlen(keyName)+1];
strcpy(aname,keyName);

 char* id = strstr(aname,"Keys");
 if(!id)return nodeName;
 int i = id-aname;
 char* p= &aname[0];
 int size = i;

 nodeName = new char[size+1];
 strncpy(nodeName,p,size);

 nodeName[size]='\0';
 delete [] aname;   

return nodeName;
}

////////////////////////////////////////////////////////////////

int
mysqlAccessor::getElementID(const char* nodeName){

char* id=strstr(nodeName,"None");
if(id)return 0;
return atoi(nodeName);
}

/////////////////////////////////////////////////////////////////

bool
mysqlAccessor::execute(const char* name){
  Db.Input(name,&buff);
  return true;
}











