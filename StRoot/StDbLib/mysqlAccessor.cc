/***************************************************************************
 *
 * $Id: mysqlAccessor.cc,v 1.8 1999/09/30 02:06:14 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Storage specific SQL queries to database
 *
 ***************************************************************************
 *
 * $Log: mysqlAccessor.cc,v $
 * Revision 1.8  1999/09/30 02:06:14  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "mysqlAccessor.hh"
#include "StDbTableDescriptor.h"
#include <strings.h>

////////////////////////////////////////////////////////////

int
mysqlAccessor::QueryDb(StDbConfigNode* node){
  
  char* configName = node->getConfigName();
  if(!configName)return 0;
  char* nodeName = node->getName();

    Db<< "SELECT Nodes.ID FROM Nodes WHERE Nodes.KeyName='";
    Db<<configName<<"' AND Nodes.Name='"<<nodeName<<"'"<<endsql;

    Db.Output(&buff);

    int NodeID;
    buff.SetClientMode();
    if(!buff.ReadScalar(NodeID,"ID")){
      cerr <<"QueryDb::Config: No Key = "<< configName<< " found " << endl;
     return 0;
    }

   Db.Release(); 
   buff.Raz();

   char thisNode[100];
   ostrstream os(thisNode,100); os<<NodeID<<ends;


    Db<<"SELECT subNodes.Name, ";
    Db<<"subNodes.KeyName, subNodes.elementID, subNodes.NodeType, subNodes.baseline ";
    Db<<"FROM Nodes LEFT JOIN NodeRelation ON ";
    Db<<"Nodes.ID=NodeRelation.ParentID LEFT JOIN Nodes as subNodes ON ";
    Db<<"NodeRelation.NodeID=subNodes.ID WHERE Nodes.ID="<<thisNode<<endsql;


    delete [] configName;
    delete [] nodeName;

  if(Db.NbRows() == 0){
    cerr << "No Rows Satisfying Query " << Db.NbRows()<< endl;
    Db.Release();
    return 0;
  }

  int* elementID;
  char* eID;
  char* version;
  char* baseLine;
  StDbConfigNode* newNode;
  //  cout << "Will Loop through results " << endl;

  cout << "Number of Rows = " << Db.NbRows() << endl;
  while(Db.Output(&buff)){
   //    cout << "loop check " << chck << endl;
   //      buff.Print();

     buff.SetClientMode();

   char* nodeType = 0;
   cout<<"will attempt to get nodeName"<< endl;

   if(!buff.ReadScalar(nodeName,"Name")){ 
     buff.Raz();
     continue;
   }

   cout<<"will attempt to get nodeType"<< endl;
   if(!buff.ReadScalar(nodeType,"NodeType")){ 
     buff.Raz();
     continue;
   }
   cout<<"got nodeName & nodeType = "<<nodeName<<" & "<<nodeType<< endl;


   //   cout << ConfigName << endl;
    if(isConfig(nodeType)){ // it is a configKey
      //      cout << "is config " << ConfigName << endl;
      //      char* nodeName = getNodeName(ConfigName);
      //     delete [] ConfigName;
      if(!buff.ReadScalar(configName,"KeyName"))cout << "Config err reading version" << endl; 
      newNode = new StDbConfigNode(node,nodeName,configName);
      delete [] nodeName;
      delete [] nodeType;
      delete [] configName;

    } else { // it is a table
      //  cout << "is Table " << ConfigName << endl;
      //      buff.Print();
      
      if(!buff.ReadScalar(version,"KeyName"))cout << "Table err reading versoin" << endl;
      if(!buff.ReadScalar(eID,"elementID"))cout << "Table err reading elementID " << endl;

      int numRows=0;
      elementID = getElementID(eID,numRows); 
      
      if(!buff.ReadScalar(baseLine,"baseline"))cout << "Table err reading baseline" << endl;
      bool isBase = isBaseLine(baseLine);  delete [] baseLine;

      StDbTable* table = node->addTable(nodeName,version,isBase);
      table->setElementID(elementID,numRows);  
      
      //     QueryDescriptor(table);
      delete [] nodeName;
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
mysqlAccessor::QueryDb(StDbTable* table, const char* reqTime){

unsigned int rTime = getUnixTime(reqTime);
return QueryDb(table,rTime);

}

////////////////////////////////////////////////////////////////

int
mysqlAccessor::QueryDb(StDbTable* table, unsigned int reqTime){


  char* tableName = table->getTableName();

  if(!table->hasDescriptor())QueryDescriptor(table);

  int tableID = table->getStructID(); // structID=tableName index 
                                      // that comes from QueryDescriptor
  char* version = table->getVersion();

  char* rTime=getDateTime(reqTime);
  
  int countRows = 0;
  table->setRowNumber();  // initialize counter to 0

  //  table->StreamAccessor(&buff,true);

  char baseString[256];
  ostrstream os(baseString,256);
  os<<" Where structID="<<tableID;
  os<<" AND version='"<<version<<"'"<<ends;

  int numRows=table->GetNRows();
  int* elementID = table->getElementID();

  char elementString[1024];
  ostrstream es(elementString,1024);
  es << " (";
  for(int i=0;i<numRows-1;i++){
   es<<"elementID="<<elementID[i]<<" OR ";
  }
   es<<"elementID="<<elementID[numRows-1]<<")"<<ends;

  char* bTime=0;  
  char* eTime=0;

  Db << " select beginTime as mendTime from dataIndex";
  Db << baseString <<" AND beginTime>'"<<rTime <<"' AND "<<elementString; 
  Db << "Order by beginTime limit 1"<<endsql;


  if(Db.Output(&buff)){
   buff.SetClientMode();
   buff.ReadScalar(eTime,"mendTime");
   buff.SetStorageMode();
   Db.Release();
   buff.Raz();
  
  table->setEndTime(eTime);
  table->setEndTime(getUnixTime(eTime));
  } else {
  unsigned int itmp = 0;
  table->setEndTime(itmp);
  table->setEndTime(getDateTime(itmp));
  }


  unsigned int t1,t2;
  int indexID;

  t1=t2=0;

  for(i=0;i<numRows;i++){

   char thisElement[100];
   ostrstream tes(thisElement,100); tes<<elementID[i]<<ends;

  Db << " select count, beginTime as mbeginTime from dataIndex";
  Db << baseString <<" AND beginTime<'"<<rTime;
  Db <<"' AND elementID="<<thisElement;
  Db << " Group by count order by beginTime DESC limit 1"<<endsql;  


   
  if(Db.Output(&buff)){
   buff.SetClientMode();
   buff.ReadScalar(bTime,"mbeginTime");
   buff.ReadScalar(indexID,"count");
   buff.SetStorageMode();
   Db.Release();
   buff.Raz();
   t1 = getUnixTime(bTime);
  } else {
    cerr<<"QueryDb::Table no valid row for this query" << endl;
    return 0;
  }

   char thisIndex[100];
   ostrstream inds(thisIndex,100); inds<<indexID<<ends;

  Db<< "Select * from " << tableName <<" LEFT JOIN dataIndex on ";
  Db<< "dataIndex.dataID="<<tableName<<".dataID ";
  Db<< "where dataIndex.count="<<thisIndex<<endsql;

  if(Db.Output(&buff)){
//    table->StreamAccessor(&buff,true);
    table->dbStreamer(&buff,true);
    countRows++;
    buff.Raz();
  }

   if(!t2){
     t2=t1;
   } else {
     if(t1>t2)t2=t1;
   }

  }

   if(bTime)delete [] bTime;
   bTime = getDateTime(t2);
   table->setBeginTime(bTime);
   table->setBeginTime(t2);

   // reset row number to 0 for future dbStreaming

   table->setRowNumber();

  if(countRows != table->GetNRows()){
    cerr <<"Query::Table: Mismatch between NRows Requested & Delivered"<<endl;
    cerr <<" NRows Requested = "<<table->GetNRows() << "  ";
    cerr <<" NRows Delivered = "<<countRows<<endl;
  }


  if(tableName)delete [] tableName;
  if(version)delete [] version;
  if(bTime)delete [] bTime;
  if(eTime)delete [] eTime;
  Db.Release();

  return 1; 
}  


////////////////////////////////////////////////////////////////

int
mysqlAccessor::WriteDb(StDbTable* table, unsigned int storeTime){
  
char* sTime = getDateTime(storeTime);
int retVal = WriteDb(table,sTime);
delete [] sTime;
return retVal;

}

////////////////////////////////////////////////////////////////

int
mysqlAccessor::WriteDb(StDbTable* table, const char* storeTime){
  
  char* tableName = table->getTableName();
  int tableID = table->getStructID(); // c-struct ID

  table->setRowNumber(); // set to 0
  int nrows = table->GetNRows(); // get number of rows to write out
  int* elements = table->getElementID();

  int eID;
  for(int i=0; i<nrows; i++){

   
   table->dbStreamer(&buff,false);
   Db.Input(tableName,&buff); // input to database
 
   int dataID = Db.GetLastInsertID(); // get auto-generated row-id
   Db.Release();
   buff.Raz();

   eID=elements[i];
   //   table->StreamAccessor(&buff,false); // write address to buffer
   buff.WriteScalar(table->getSchemaID(),"schemaID");
   buff.WriteScalar(storeTime,"beginTime");
   buff.WriteScalar(table->getVersion(),"version");
   buff.WriteScalar(eID,"elementID");
   //   buff.WriteScalar(elements[i],"elementID");
   buff.WriteScalar(dataID,"dataID");  
   buff.WriteScalar(tableID,"structID");

   if(!Db.Input("dataIndex",&buff)){ // write row address or delete data
      Db.Release();
      char dID[7]; ostrstream ds(dID,7); ds<<dataID<<ends;
      Db<<"delete from "<<tableName;
      Db<<" where dataID="<<dID<<" limit 1"<<endsql;
   }

   Db.Release();
   buff.Raz();

  } // element loop

  table->setRowNumber(); // reset row number to 0 for future dbStreaming
return 1;

}

////////////////////////////////////////////////////////////////
int
mysqlAccessor::QueryDescriptor(StDbTable* table){

  // Query the database for the elements associated with this
  // tableName and with the schemaID that is added at the 
  // constructure or over-written by an input file ... e.g. requested SchemaID

  if(!table->hasDescriptor()){

    char* tableName=table->getTableName();
    //cout << "Getting Descriptor for table = " << tableName << endl;  
    /*
    Db<< "SELECT structure.lastSchemaID, structure.ID FROM structure WHERE structure.name='";
    Db<<tableName<<"'"<<endsql;
    Db.Output(&buff);
    int tableID, schemaID;
 
    buff.SetClientMode();
    if(!buff.ReadScalar(tableID,"ID")){
      cerr <<"QueryDb::Table: No structure = "<< tableName<< " found " << endl;
      return 0;
    }
    if(!buff.ReadScalar(schemaID,"lastSchemaID"))cerr<<"QueryDb::Descriptor: No schemaID found"<<endl;

    Db.Release();
    buff.Raz();
    */
    
    Db.Release();
    buff.Raz();

    int tableID, schemaID;
    Db<<"SELECT structure.lastSchemaID, structure.ID, schema.name, ";
    Db<<"schema.mask, schema.type, schema.length, ";
    Db<<"relation.position FROM structure LEFT JOIN relation ON ";
    Db<<"structure.ID=relation.structID LEFT JOIN schema ON ";
    Db<<"relation.memberID=schema.ID WHERE structure.name='";
    Db<< tableName <<"' ORDER BY relation.position"<<endsql;
    
    Db.Output(&buff);
    buff.SetClientMode();
    if(!buff.ReadScalar(tableID,"ID")){
      cerr <<"QueryDb::Table: No structure = "<< tableName<< " found " << endl;
      return 0;
    }
    if(!buff.ReadScalar(schemaID,"lastSchemaID")){
      cerr<<"QueryDb::Descriptor: No schemaID found"<<endl;
    }

    table->setStructID(tableID);

    StDbTableDescriptor* descriptor = 0;
    int requestSchemaID = table->getSchemaID();
    if(requestSchemaID==0){
      requestSchemaID = schemaID; // most recent one in the database
      table->setSchemaID(schemaID);
    }

      if(!descriptor)descriptor = new StDbTableDescriptor();
      descriptor->fillElement(&buff,requestSchemaID);
      buff.Raz();

    while(Db.Output(&buff)){
      if(!descriptor)descriptor = new StDbTableDescriptor();
      descriptor->fillElement(&buff,requestSchemaID);
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
 if(name && !strcmp(name,"table")==0)retVal=true;
 // char* teststr = strstr(name,"Keys");
 // cout << " isCongif:: teststr" << teststr << endl;
 // if(teststr)retVal=true;

 return retVal;
}

////////////////////////////////////////////////////////////////

bool
mysqlAccessor::isBaseLine(const char* baseline){

 bool retVal = false;
 if(baseline && strcmp(baseline,"Y")==0)retVal=true;
 return retVal;

}

////////////////////////////////////////////////////////////////

char*
mysqlAccessor::getBoolString(bool baseline){

 char* retVal = new char[2];
 if(baseline){
  strcpy(retVal,"Y");
 } else {
  strcpy(retVal,"N");
 }

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
                                                 /*
int
mysqlAccessor::getElementID(const char* nodeName){

char* id=strstr(nodeName,"None");
if(id)return 0;
return atoi(nodeName);

}
  
                                               */
////////////////////////////////////////////////////////////////

int*
mysqlAccessor::getElementID(char* nodeName, int numRows){

numRows=1;
int * retVal = 0;
char* id=strstr(nodeName,"None");
if(id)return retVal;

int numElements = 1;
id = strstr(nodeName,",");
 while(id){
   numElements++;
   id=strstr(id,",");
 }

 retVal = new int[numElements];
 char* p1=&nodeName[0];
 char* anID;

 int num=0;
 while(p1){
   anID = getNextID(p1);
   retVal[num]=atoi(anID);
   num++;
   if(anID)delete [] anID;
 }

numRows=numElements;

return retVal;
}


////////////////////////////////////////////////////////////////

char*
mysqlAccessor::getNextID(char*& currentElement){

char* nextID = 0;
if(!currentElement)return nextID;

char* id = strstr(currentElement,",");

if(!id) {
  nextID = new char[strlen(currentElement)+1];
  strcpy(nextID,currentElement);
  currentElement = 0;
} else {
  int iloc = id-currentElement;
  nextID = new char[iloc+1];
  strncpy(nextID,currentElement,iloc);
  nextID[iloc]='\0';
  currentElement = id; currentElement++;
}

return nextID;
}

/////////////////////////////////////////////////////////////////
char*
mysqlAccessor::getDateTime(unsigned int time){

char* retVal;
char thisTime[100];
ostrstream os(thisTime,100); os<<time<<ends;
    Db<<"select from_unixtime("<<thisTime<<") as requestTime"<<endsql;

  Db.Output(&buff);
  buff.SetClientMode(); 
  buff.ReadScalar(retVal,"requestTime");
  buff.Raz();
  Db.Release();

return retVal;

}

/////////////////////////////////////////////////////////////////
unsigned int
mysqlAccessor::getUnixTime(const char* time){

unsigned int retVal;

    Db<<"select unix_timestamp('"<<time<<"') as requestTime"<<endsql;

  Db.Output(&buff);
  buff.SetClientMode(); 
  buff.ReadScalar(retVal,"requestTime");
  buff.Raz();
  Db.Release();

return retVal;
}


/////////////////////////////////////////////////////////////////

bool
mysqlAccessor::execute(const char* name){

  Db.Input(name,&buff);
  return true;
}
