/***************************************************************************
 *
 * $Id: mysqlAccessor.cc,v 1.11 1999/12/07 21:25:25 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Storage specific SQL queries to database
 *
 ***************************************************************************
 *
 * $Log: mysqlAccessor.cc,v $
 * Revision 1.11  1999/12/07 21:25:25  porter
 * some fixes for linux warnings
 *
 * Revision 1.10  1999/12/03 22:24:01  porter
 * expanded functionality used by online, fixed bug in
 * mysqlAccessor::getElementID(char*), & update StDbDataSet to
 * conform to changes in Xml reader & writer
 *
 * Revision 1.9  1999/10/19 14:30:41  porter
 * modifications relevant to use with StDbBroker and future merging with
 * "params" database structure + some docs + suppressing diagnostics messages
 *
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
   // cout<<"will attempt to get nodeName"<< endl;

   if(!buff.ReadScalar(nodeName,"Name")){ 
     buff.Raz();
     continue;
   }

   // cout<<"will attempt to get nodeType"<< endl;
   if(!buff.ReadScalar(nodeType,"NodeType")){ 
     buff.Raz();
     continue;
   }
   // cout<<"got nodeName & nodeType = "<<nodeName<<" & "<<nodeType<< endl;


   //   cout << ConfigName << endl;
    if(isConfig(nodeType)){ // it is a configKey

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

  //
  //  Using name & version informataion in StDbTable + request timestamp
  //  fill data from the database into the StDbTable pointer.
  //

  // --> get table name &, if needed, get the descriptor
  char* tableName = table->getTableName();
  char* cstructName=0; 
  if(!table->hasDescriptor())QueryDescriptor(table);

  // --> find ID for this table Name & check if storage is binary
    Db<<"SELECT structure.*, namedRef.ID as nameID"; 
    Db<<" from structure";
    Db<<" left join namedRef on structure.ID=namedRef.structID";
    Db<<" WHERE namedRef.name='";
    Db<< tableName <<"'"<<endsql;
   
    char* isbin=0;
    int nameID=0; // name reference into c-struct
    Db.Output(&buff);
    buff.SetClientMode();
    if(!buff.ReadScalar(isbin,"IsBinary")){
      //      cerr <<"QueryDb::Table: No storage specified for structure = "<< tableName<< endl;
    isbin = 0;
    }
    if(!buff.ReadScalar(cstructName,"name")){
      cerr<<"QueryDb::Table no cstruct for requested Name "<<tableName<< endl;
      return 0;
    }
    buff.ReadScalar(nameID,"nameID");
    bool IsBinary=false;
    if(isbin && strstr(isbin,"Y"))IsBinary=true;
    char* isindexed=0;
    bool IsIndexed=true;
    if(buff.ReadScalar(isindexed,"IsIndexed")){
     if(isindexed && strstr(isindexed,"N"))IsIndexed=false;
    } // else field is not there so assume it is indexed
    Db.Release(); buff.Raz();

    if(!IsIndexed){
      cerr << "QueryDb:: Cannot request Table via this API - no Index"<< endl;
      return 0;
    }

// --> get the "version" & requestTime information

   char* version = table->getVersion();
   char* rTime=getDateTime(reqTime);
  
   table->setRowNumber();  // initialize counter to 0

// begin preparing queries
  char baseString[256];
  ostrstream os(baseString,256);
  os<<" Where nameID="<<nameID;
  os<<" AND version='"<<version<<"' "<<ends;

  int numRows=table->GetNRows();
  int* elementID = table->getElementID();

  // loop over numRows to get minimum endTime in 1 query
  char elementString[1024];
  ostrstream es(elementString,1024);

  int i;

  if(elementID){
   es << " AND (";
   for(i=0;i<numRows-1;i++){
    es<<"elementID="<<elementID[i]<<" OR ";
   }
    es<<"elementID="<<elementID[numRows-1]<<")"<<ends;
  } else {
    // don't worry 'bout it
    es<<" "<<ends;
  }

  unsigned int z = 0;
  table->setBeginTime(z);
  table->setBeginTime(getDateTime(z));
  char* bTime=0;  
  char* eTime=0;
  int numberOfRows=0;

  Db << " select beginTime as mendTime from dataIndex";
  Db << baseString <<" AND beginTime>'"<<rTime <<"'"<<elementString; 
  Db << "Order by beginTime limit 1"<<endsql;

  if(Db.Output(&buff)){
    buff.SetClientMode();
    buff.ReadScalar(eTime,"mendTime");
    buff.SetStorageMode();
    Db.Release(); buff.Raz();
    table->setEndTime(eTime);
    table->setEndTime(getUnixTime(eTime));
  } else {
    table->setEndTime(getEndTime()); // simply use dec-31st 2037, 11:59:59
    table->setEndTime(getEndDateTime()); // simply use dec-31st 2037, 11:59:59
  }

  unsigned int t1,t2;
  int indexID;
  t1=t2=0;
  int countRows = 0;
  int eID;

  if(!IsBinary){ // each dataID -> points to data row with as a table row

    if(!elementID){ // get all rows that satisfy version + timestamp

     Db << " select DISTINCT elementID from dataIndex";
     Db << baseString <<" order by elementID"<< endsql;

     numRows=0; int nmax = 500;
     int swapSize;
     int* storeIt = new int[nmax];
     int* swapIt=0;
     while(Db.Output(&buff)){
      buff.ReadScalar(eID,"elementID");
      if(numRows==nmax){
        swapSize = 2*nmax; 
        swapIt = new int[swapSize];
        memcpy(swapIt,storeIt,4*nmax);
        delete [] storeIt; storeIt = swapIt; swapIt = 0;
        nmax = swapSize;
      }
      storeIt[numRows]=eID;
      numRows++;
     }

     elementID = new int[numRows];
     memcpy(elementID,storeIt,4*numRows);              
     delete [] storeIt;
     table->setElementID(elementID,numRows);

    }

   // loop over rows in 'elementID'
    for(i=0;i<numRows;i++){

     char thisElement[100];
     ostrstream tes(thisElement,100); tes<<elementID[i]<<ends;

     // query Index to retrieve pointer to data
     Db << " select count, beginTime as mbeginTime from dataIndex";
     Db << baseString <<" AND beginTime<'"<<rTime;
     Db <<"' AND elementID="<<thisElement;
     Db << " Group by count order by beginTime DESC limit 1"<<endsql;  
   
     if(Db.Output(&buff)){
       buff.SetClientMode();
       buff.ReadScalar(bTime,"mbeginTime");
       buff.ReadScalar(indexID,"count");
       buff.SetStorageMode();
       Db.Release(); buff.Raz();
       t1 = getUnixTime(bTime);
     } else {
       cerr<<"QueryDb::Table no valid row for this query" << endl;
       return 0;
     }

     char thisIndex[100];
     ostrstream inds(thisIndex,100); inds<<indexID<<ends;

    // get data for this Index pointer

     Db<< "Select * from " << cstructName <<" LEFT JOIN dataIndex on ";
     Db<< "dataIndex.dataID="<<cstructName<<".dataID ";
     Db<< "where dataIndex.count="<<thisIndex<<endsql; 

     // send data into the table
     if(Db.Output(&buff)){
        table->dbStreamer(&buff,true);
        countRows++;
        buff.Raz();
      }

      // keep track of maximum beginTime & minimum 'endTime'
      if(!t2){
       t2=t1;
      } else {
      if(t1>t2)t2=t1;
      }

     }// end loop over rows

  } else {  
    // many rows in a binary Blob in 1 table row

   // query Index to retrieve pointer to data
    Db << " select count, numRows, beginTime as mbeginTime from dataIndex";
    Db << baseString <<" AND beginTime<'"<<rTime;
    Db << "' Group by count order by beginTime DESC limit 1"<<endsql;  
 
    if(Db.Output(&buff)){
      buff.SetClientMode();
      buff.ReadScalar(bTime,"mbeginTime");
      buff.ReadScalar(indexID,"count");
      buff.ReadScalar(numberOfRows,"numRows");
      buff.SetStorageMode();
      Db.Release(); buff.Raz();
      t2 = getUnixTime(bTime);
    } else {
      cerr<<"QueryDb::Table no valid row for this query" << endl;
      return 0;
    }

    char thisIndex[100];
    ostrstream inds(thisIndex,100); inds<<indexID<<ends;

   // get data for this Index pointer
  
    Db<< "Select * from bytes LEFT JOIN dataIndex on ";
    Db<< "dataIndex.dataID=bytes.dataID ";
    Db<< "where dataIndex.count="<<thisIndex<<endsql; 
  
   // send data into the table
    table->SetNRows(numberOfRows);
    if(Db.Output(&buff)){
       table->dbTableStreamer(&buff,"bytes",true);
       countRows+=numberOfRows;
       buff.Raz();
    }

  } //End binary stream

  //store max-beginTime & min-EndTime
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
  if(cstructName) delete [] cstructName;
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
  char* cstructName=0;

    Db<<"SELECT structure.*, namedRef.ID as namedID"; 
    Db<<" from structure";
    Db<<" left join namedRef on structure.ID=namedRef.structID";
    Db<<" WHERE namedRef.name='";
    Db<< tableName <<"'"<<endsql;

    //    Db<<"SELECT IsBinary, ID from structure WHERE structure.name='";
    //    Db<< tableName <<"'"<<endsql;
   
    char* isbin=0;
    char* isindexed = 0;
    int nameID=0; // c-struct ID
    Db.Output(&buff);
    buff.SetClientMode();
    if(!buff.ReadScalar(isbin,"IsBinary")){
      cerr <<"QueryDb::Table: No storage specified for structure = "<< tableName<< endl;
    isbin = 0;
    }
    if(!buff.ReadScalar(cstructName,"name")){
      cerr<<"WriteDb::Table no cstruct for requested Name "<<tableName<< endl;
      return 0;
    }
    buff.ReadScalar(nameID,"ID");
    bool IsBinary=false;
    if(isbin && strstr(isbin,"Y"))IsBinary=true;
    bool IsIndexed=true;
    if(buff.ReadScalar(isindexed,"IsIndexed")){
     if(isindexed && strstr(isindexed,"N"))IsIndexed=false;
    }
    
    Db.Release(); buff.Raz();


  table->setRowNumber(); // set to 0
  int nrows = table->GetNRows(); // get number of rows to write out
  int* elements = table->getElementID();
  if(!elements){
    elements = new int[nrows];
    for(int k=0;k<nrows;k++)elements[k]=k;
  }
  int dataID;

  if(IsBinary){
    table->dbTableStreamer(&buff,"bytes",false);
    Db.Input("bytes",&buff);
    dataID = Db.GetLastInsertID();
    Db.Release(); buff.Raz();

    if(IsIndexed){
    // now write to index
    buff.WriteScalar(table->getSchemaID(),"schemaID");
    buff.WriteScalar(storeTime,"beginTime");
    buff.WriteScalar(table->getVersion(),"version");
    buff.WriteScalar(dataID,"dataID");  
    buff.WriteScalar(nameID,"nameID");
    buff.WriteScalar(nrows,"numRows");
    if(!Db.Input("dataIndex",&buff)){ // write to index or delete the data
      Db.Release();
      char dID[7]; ostrstream ds(dID,7); ds<<dataID<<ends;
      Db<<"delete from bytes";
      Db<<" where dataID="<<dID<<" limit 1"<<endsql;
    }
   }
      
  } else {

   int eID;
   for(int i=0; i<nrows; i++){
  
     table->dbStreamer(&buff,false);
     Db.Input(cstructName,&buff); // input to database
 
     dataID = Db.GetLastInsertID(); // get auto-generated row-id
     Db.Release(); buff.Raz();

     if(IsIndexed){
     eID=elements[i];

     buff.WriteScalar(table->getSchemaID(),"schemaID");
     buff.WriteScalar(storeTime,"beginTime");
     buff.WriteScalar(table->getVersion(),"version");
     buff.WriteScalar(eID,"elementID");
     buff.WriteScalar(dataID,"dataID");  
     buff.WriteScalar(nameID,"nameID");

     if(!Db.Input("dataIndex",&buff)){ // write row address or delete data
       Db.Release();
       char dID[7]; ostrstream ds(dID,7); ds<<dataID<<ends;
       Db<<"delete from "<<cstructName;
       Db<<" where dataID="<<dID<<" limit 1"<<endsql;
     }
     }
     Db.Release();
     buff.Raz();

    } // element loop
  }

  if(tableName) delete [] tableName;
  if(cstructName) delete [] cstructName;

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

    Db.Release();
    buff.Raz();

    char* tableName=table->getTableName();
    char* cstructName;
    Db<<"SELECT structure.name from structure";
    Db<<" left join namedRef on structure.ID=namedRef.structID";
    Db<<" WHERE namedRef.name='";
    Db<< tableName <<"'"<<endsql;

    Db.Output(&buff);
    buff.SetClientMode();
    if(!buff.ReadScalar(cstructName,"name")){
      cerr<<"QueryDb::Table no cstruct for requested Name "<<tableName<< endl;
      return 0;
    }
    Db.Release(); buff.Raz();
    
    int tableID, schemaID;
    Db<<"SELECT structure.lastSchemaID, structure.ID, schema.name, ";
    Db<<"schema.mask, schema.type, schema.length, ";
    Db<<"schema.position FROM structure LEFT JOIN schema ON ";
    Db<<"structure.ID=schema.structID WHERE structure.name='";
    Db<< cstructName <<"' ORDER BY schema.position"<<endsql;
    
    Db.Output(&buff);
    buff.SetClientMode();
    if(!buff.ReadScalar(tableID,"ID")){
      // cerr <<"QueryDb::Table: No structure = "<< tableName<< " found " << endl;
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
    delete [] cstructName;
  }


return 1;

}


////////////////////////////////////////////////////////////////

bool
mysqlAccessor::isConfig(const char* name){
 bool retVal = false;
 if(name && !strcmp(name,"table")==0)retVal=true;
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
////////////////////////////////////////////////////////////////

int*
mysqlAccessor::getElementID(char* nodeName, int& numRows){

numRows=1;
int * retVal = 0;
char* id=strstr(nodeName,"None");
if(id)return retVal;

// On the to-do list:
//   now we expect list in the form 1,2,3,4, ... 
//   but we may want to also allow 1-800 and/or 1,2,6-12,15,19
//   so I should remake string so that 1-800 = 1,2,3,4,...,799,800
//
// cout << "My elementID = " << nodeName << endl;

char* tmpName = new char[strlen(nodeName)+1];
strcpy(tmpName,nodeName);

int numElements = 1;
id = strstr(tmpName,",");
char* id1;
char* id2;

id2 = strstr(tmpName,"-");
char islist[2048];
ostrstream sl(islist,2048);

if(id2 && id && id2<id){
  id=id2;
  id[0]=',';
  sl<<"r";
} else {
  sl<<"l";
}

int numEntries = 1;
if(id)id++;
while(id){
  //  cout << "id = " << id << endl;
   numEntries++;
   id1=strstr(id,",");
   id2=strstr(id,"-");
   id = id1;
   if(id && id2 && id2<id){
       id=id2;
       id[0]=',';
       sl<<"r";
   } else {
       sl<<"l";
   }
   if(id)id++;
}
 sl << ends;

 // cout << "My string list = " << islist << endl;

 int* tmpElements = new int[100000];
 char* p1=&tmpName[0];
 char* anID;
 anID = getNextID(p1);
 tmpElements[0] = atoi(anID);
 numElements = 1;
 int iEnd, iStart, k;
 for(int ient=1;ient<numEntries;ient++){
   anID = getNextID(p1);
   if(islist[ient-1]=='r'){
     iEnd = atoi(anID);
     iStart = tmpElements[numElements-1];
     int irange=iEnd-iStart;
     for(int ir=1;ir<=irange;ir++){
       numElements++;
       tmpElements[numElements-1]=iStart+ir;
     }
   } else {
     numElements++;
     tmpElements[numElements-1]=atoi(anID);
   }
   if(anID) delete [] anID;
 }

 retVal = new int[numElements];
 for(k=0;k<numElements;k++)retVal[k]=tmpElements[k];
 numRows = numElements;
 delete [] tmpElements;
 delete [] tmpName; 

 // for(k=0;k<numElements;k++)cout<<"retRow="<<k<<" & retID="<<retVal[k]<<endl;

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
