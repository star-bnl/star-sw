/***************************************************************************
 *
 * $Id: mysqlAccessor.cc,v 1.16 2000/01/27 05:54:35 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Storage specific SQL queries to database
 *
 ***************************************************************************
 *
 * $Log: mysqlAccessor.cc,v $
 * Revision 1.16  2000/01/27 05:54:35  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
 * Revision 1.15  2000/01/24 15:10:17  porter
 * bug fix to dbType+dbDomain info in deep tree
 *
 * Revision 1.14  2000/01/19 20:20:08  porter
 * - finished transaction model needed by online
 * - fixed CC5 compile problem in StDbNodeInfo.cc
 * - replace TableIter class by StDbTableIter to prevent name problems
 *
 * Revision 1.13  2000/01/14 14:50:52  porter
 * expanded use of verbose mode & fixed inconsistency in
 * StDbNodeInfo::getElementID
 *
 * Revision 1.12  2000/01/10 20:37:55  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
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
#include "StDbTableIter.hh"
#include "StDbManager.hh"
#include <strings.h>

////////////////////////////////////////////////////////////////

mysqlAccessor::~mysqlAccessor(){

if(mdbName) delete [] mdbName;

}

////////////////////////////////////////////////////////////

int 
mysqlAccessor::initDbQuery(const char* dbname, const char* serverName, const char* host, const int portNumber){ 

if(mdbName) delete [] mdbName;
mdbName=new char[strlen(dbname)+1];
strcpy(mdbName,dbname);

return (int)Db.Connect(host,"","",dbname,portNumber);

};

////////////////////////////////////////////////////////////////

char*
mysqlAccessor::getDbName() const {

char* retVal=0;

if(!mdbName)return retVal;

retVal = new char[strlen(mdbName)+1];
strcpy(retVal,mdbName);

return retVal;
}

///////////////////////////////////////////////////////////////////////

int
mysqlAccessor::QueryDb(StDbConfigNode* node){
  

StDbNodeInfo currentNode;
int NodeID;

 if(!node->IsConfigured()){//  1st node only has name & key 
    if(!prepareNode(node,&currentNode)) {
      if(StDbManager::Instance()->IsVerbose()){
        cout << " Node "<< node->getName() << " & ";
        cout << node->getVersion() <<"  not found in DB " << endl;
      }
      return 0;
    }
  }
    NodeID=node->getNodeID(); // all I need here is NodeID.

    char* tmpName;
    if(!(tmpName=node->getDbName())){
      node->setDbName(mdbName);
    } else {
      delete [] tmpName;
    }
    if(!currentNode.dbName)currentNode.mstrCpy(currentNode.dbName,mdbName);
    currentNode.dbType = mdbType;
    currentNode.dbDomain = mdbDomain; 

   char thisNode[100];
   ostrstream os(thisNode,100); os<<NodeID<<ends;

   // Db<<"Select subNode.name, subNode.versionKey, subNode.nodeType from Nodes ";
   Db<<"Select subNode.* from Nodes ";
   Db<<"LEFT JOIN NodeRelation ON Nodes.ID=NodeRelation.ParentID ";
   Db<<"LEFT JOIN Nodes as subNode ON NodeRelation.NodeID=subNode.ID ";
   Db<<" Where Nodes.ID="<<thisNode<<endsql;

   if(Db.NbRows() == 0){
     cerr << "No Rows Satisfying Query " << Db.NbRows()<< endl;
     Db.Release();
     return 0;
   }

//
// Loop over rows in Configuration
//
   StDbTable* table; // for setting configured state

  while(Db.Output(&buff)){

     buff.SetClientMode();
     if(currentNode.name) delete [] currentNode.name;
     if(currentNode.versionKey) delete currentNode.versionKey;
     if(!buff.ReadScalar(currentNode.name,"name"))return 0;
     if(!buff.ReadScalar(currentNode.versionKey,"versionKey"))return 0;

     if(!readNodeInfo(&currentNode))cerr<< "read-err"<<endl; // just read this node

     if(strcmp(currentNode.nodeType,"table")!=0){ // it is a node

       if(StDbManager::Instance()->IsVerbose()){
         cout << "tableQuery:: Found node " << currentNode.name;
         cout << " of version = " << currentNode.versionKey <<endl;
         cout << " Parent = " << node->getName();
         cout << " of version = " << node->getVersion()<< endl;;
       }
      new StDbConfigNode(node,&currentNode);

     } else { // it is a table

       if(StDbManager::Instance()->IsVerbose()){
         cout << "tableQuery:: Found Table " << currentNode.name;
         cout << " of version = " << currentNode.versionKey <<endl;
         cout << " Parent = " << node->getName();
         cout << " of version = " << node->getVersion()<< endl;;
       }
      table = node->addTable(&currentNode);
      if(table)table->setConfigured(true);

     }  // isNode check

    buff.Raz(); //  erase prior to next retrieval

  } // table/config loop

  buff.Raz();
  Db.Release();
  return 1;
}

////////////////////////////////////////////////////////////////

int
mysqlAccessor::QueryDb(StDbTable* table, unsigned int reqTime){

  //
  //  Using name & version informataion in StDbTable + request timestamp
  //  fill data from the database into the StDbTable pointer.
  //
  // --> get table name &, if needed, get the descriptor

  StDbNodeInfo currentNode;

  if(!table->IsConfigured()){
  
   if(!prepareNode((StDbNode*)table,&currentNode))return 0;
  
   } else {   
    table->getNodeInfo(&currentNode);
    currentNode.name = table->getName();
    currentNode.versionKey = table->getVersion();
   }

   if(!table->hasDescriptor()){
      if(!QueryDescriptor(table))return 0;
   }

   if(!table->IsIndexed()){
      cerr << "QueryDb:: Cannot request Table via this API - no Index"<< endl;
      return 0;
   }

// --> get the "version" & requestTime information

   char* rTime=getDateTime(reqTime);
   table->setRowNumber();  // initialize counter to 0

// begin preparing queries
  char baseString[256];
  ostrstream os(baseString,256);
  os<<" Where nodeID="<<currentNode.nodeID;
  os<<" AND version='"<<currentNode.versionKey<<"' "<<ends;

  int numRows;
  int* elementID = currentNode.getElementID((const char*)currentNode.elementID,numRows);

  // loop over numRows to get minimum endTime in 1 query
  char elementString[1024];
  ostrstream es(elementString,1024);

  int i;

  if(elementID){
   es << " AND (";
   for(i=0;i<numRows-1;i++)es<<"elementID="<<elementID[i]<<" OR ";
   es<<"elementID="<<elementID[numRows-1]<<")"<<ends;
  } else {
    // don't query on elementID
    es<<" "<<ends;
  }

  unsigned int z = 0;
  table->setBeginTime(z);
  char* bTime=getDateTime(z);  
  table->setBeginTime(bTime);
  if(bTime){
    delete [] bTime;
    bTime=0;  
  }
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

  if(!currentNode.IsBinary){ // each dataID points to data row

   
    if(!elementID){//then request all rows of this version+timestamp

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
      buff.Raz();
     }

     elementID = new int[numRows];
     memcpy(elementID,storeIt,(unsigned int)numRows*sizeof(int));              
     delete [] storeIt;
     table->setElementID(elementID,numRows);
     
    }


   // loop over rows in 'elementID' either from request or just all 

    for(i=0;i<numRows;i++){

     char thisElement[100];
     ostrstream tes(thisElement,100); tes<<elementID[i]<<ends;

     // query Index to retrieve pointer to data
     Db << " select count, beginTime as mbeginTime from dataIndex";
     Db << baseString <<" AND beginTime<'"<<rTime;
     Db <<"' AND elementID="<<thisElement;
     Db << " Group by count order by beginTime DESC limit 1"<<endsql;  

     if(StDbManager::Instance()->IsVerbose()){
       char qs[1024];
       ostrstream queryS(qs,1024);
       queryS<<"select count, beginTime as mbeginTime from dataIndex";
       queryS<<baseString <<" AND beginTime<'"<<rTime;
       queryS<<"' AND elementID="<<thisElement;
       queryS<<" Group by count order by beginTime DESC limit 1"<<ends;
       cout << " My Query :: " << endl;
       cout << qs << endl;
     }

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

     Db<< "Select * from " << currentNode.structName;
     Db<<" LEFT JOIN dataIndex on ";
     Db<< "dataIndex.dataID="<<currentNode.structName<<".dataID ";
     Db<< "where dataIndex.count="<<thisIndex<<endsql; 

     // send data into the table
     if(Db.Output(&buff)){
        table->dbStreamer(&buff,true);
        countRows++;
        buff.Raz();
     } else {
       cerr<<"QueryDb::Table no valid DATA row for this query" << endl;
       return 0;
     }

      // keep track of maximum beginTime & minimum 'endTime'
      if(!t2){
       t2=t1;
      } else {
      if(t1>t2)t2=t1;
      }
      if(bTime){
        delete [] bTime;
        bTime = 0;
      }

     }// end loop over rows

  } else {  


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

  if(bTime)delete [] bTime;
  if(eTime)delete [] eTime;
  if(rTime)delete [] rTime;
  if(elementID) delete [] elementID;
  Db.Release();

  return 1; 
}  

////////////////////////////////////////////////////////////////

int
mysqlAccessor::QueryDb(StDbTable* table, const char* whereClause){

   if(!table->hasDescriptor()){
      if(!QueryDescriptor(table))return 0;
   }

   Db.Release();
   Db<<"Select * from "<<table->getMyName()<<" "<<whereClause<<endsql;

   int icount=0;
   while(Db.Output(&buff)){
     table->dbStreamer(&buff,true);
     buff.Raz();
     icount++;
   }
   table->setRowNumber();
   if(icount==0){
     cerr<<"Query::Table NO DATA via whereClause [ "<<endl;
     cerr<<" Select * from "<<table->getMyName()<<" "<<whereClause<<" ]"<<endl;
   }

return icount;
}

////////////////////////////////////////////////////////////////

int
mysqlAccessor::WriteDb(StDbTable* table, unsigned int storeTime){
  
  StDbNodeInfo currentNode;
  // first check if node exists

  currentNode.dbName = table->getDbName();
  if(!currentNode.dbName){
     table->setDbName(mdbName);
     currentNode.dbName=table->getDbName();
  }

  currentNode.name = table->getName();
  currentNode.versionKey = table->getVersion();

  if(!queryNodeInfo(&currentNode)){ 
     // then we should store the node in DB
     // I assume the table has the right node information
     char* cName = table->getCstrName();
     if(cName)currentNode.structName = currentNode.mstrDup((const char*)cName);
     currentNode.elementID  = ((StDbNode*)table)->getElementID();
     currentNode.IsBaseLine = table->IsBaseLine();
     if(!storeNodeInfo(&currentNode))return 0;
  }

  // check if it baseline &, if so, if an instance is already stored 
  if(currentNode.IsBaseLine && hasInstance(&currentNode)){
    cerr << "WriteDb::Table Error: trying to add to baseline instance"<<endl;
    return 0;
  }

  // some node information can change e.g. isIndexed, structName,...

  if(!table->hasDescriptor()){
     if(!QueryDescriptor(table))return 0;
  }

  char* sTime = getDateTime(storeTime);

  table->setRowNumber(); // set to 0
  int nrows; // get number of rows to write out
  int* elements = table->getElementID(nrows);
  if(!elements){
    elements = new int[nrows];
    for(int k=0;k<nrows;k++)elements[k]=k;
    table->setElementID(elements,nrows);
    delete [] elements;
    elements = table->getElementID(nrows);
  }
  int dataID;

  if(currentNode.IsBinary){

    table->dbTableStreamer(&buff,"bytes",false);
    if(!Db.Input("bytes",&buff)){
      table->setRowNumber();
      return 0;
    }

    dataID = Db.GetLastInsertID();
    Db.Release(); buff.Raz();

    if(currentNode.IsIndexed){

    // now write to index
     buff.WriteScalar(table->getSchemaID(),"schemaID");
     buff.WriteScalar(sTime,"beginTime");
     buff.WriteScalar(table->getVersion(),"version");
     buff.WriteScalar(dataID,"dataID");  
     buff.WriteScalar(currentNode.nodeID,"nodeID");
     buff.WriteScalar(nrows,"numRows");

     if(!Db.Input("dataIndex",&buff)){ // write to index or delete the data
       Db.Release();
       // roll it back
       deleteRows("bytes",&dataID,1);
       table->setRowNumber();
       return 0;
     }
    }
    table->addWrittenRow(dataID);
          
  } else {  // not binary

   int eID;
   int* storedData = new int[nrows];
   int* storedIndex = new int[nrows];
   for(int i=0; i<nrows; i++){
  
     table->dbStreamer(&buff,false);
     Db.Input(currentNode.structName,&buff); // input to database
 
     dataID = Db.GetLastInsertID(); // get auto-generated row-id
     storedData[i]=dataID;
     Db.Release(); buff.Raz();
     
     if(currentNode.IsIndexed){
      eID=elements[i];

      char* version = table->getVersion();
      buff.WriteScalar(table->getSchemaID(),"schemaID");
      buff.WriteScalar(sTime,"beginTime");
      if(version){
      buff.WriteScalar(version,"version");
      delete [] version;
      }
      buff.WriteScalar(eID,"elementID");
      buff.WriteScalar(dataID,"dataID");  
      buff.WriteScalar(currentNode.nodeID,"nodeID");

      if(!Db.Input("dataIndex",&buff)){ // write row address or delete data
        Db.Release();
        // roll back this transaction
        deleteRows(currentNode.structName,storedData,i+1);
        deleteRows("dataIndex",storedIndex,i);
        table->setRowNumber(); // reset row number to 0 for future dbStreaming
        table->commit(); //zero written rows
        delete [] storedData; 
        delete [] storedIndex;
        return 0;
      } else {
        storedIndex[i] = Db.GetLastInsertID();
      }

     Db.Release();
     buff.Raz();

     } // isIndexed

    table->addWrittenRow(dataID);
    } // element loop

     delete [] storedData; 
     delete [] storedIndex;

  }

  table->setRowNumber(); // reset row number to 0 for future dbStreaming
  delete [] sTime;

return 1;
}

///////////////////////////////////////////////////////////////

void
mysqlAccessor::deleteRows(const char* tableName, int* rowID, int nrows){

  for(int i=0;i<nrows; i++){    
       char dID[7]; ostrstream ds(dID,7); ds<<rowID[i]<<ends;
       Db<<"delete from "<<tableName;
       Db<<" where dataID="<<dID<<" limit 1"<<endsql;
       Db.Release();
  }

}

///////////////////////////////////////////////////////////////

bool
mysqlAccessor::prepareNode(StDbNode* dbNode, StDbNodeInfo* node){
 
   char* version = dbNode->getVersion();
   char* nodeName = dbNode->getName();

   if(!version || !nodeName )return false;

   if(node->name) delete [] node->name;
   if(node->versionKey) delete [] node->versionKey;
   node->name = nodeName;
   node->versionKey = version;

   if(queryNodeInfo(node)){

      if(node->dbName) {
        delete [] node->dbName; node->dbName = 0;
      }      
      node->mstrCpy(node->dbName,mdbName);
      node->dbType = mdbType;
      node->dbDomain = mdbDomain;
      dbNode->setNodeInfo(node);

    } else {
      return false;
    }

return true;
}

///////////////////////////////////////////////////////////////

bool
mysqlAccessor::queryNodeInfo(StDbNodeInfo* node){

node->IsBinary   = false;
node->IsBaseLine = false;
node->IsIndexed  = true;
bool retVal=true;

    Db.Release(); buff.Raz();

    Db<< "Select * from Nodes where Nodes.name='";
    Db<< node->name<<"' AND Nodes.versionKey='"<<node->versionKey<<"'"<<endsql;

    if(!Db.Output(&buff)) { // no such node... but maybe named reference is ok
       retVal = false;
       Db.Release(); buff.Raz();
       Db<< "Select * from Nodes where Nodes.name='";
       Db<< node->name<<"' limit 1"<<endsql;
       if(!Db.Output(&buff)){
         Db.Release();
         buff.Raz();
         return retVal;
       }
    }   
    buff.SetClientMode();

    bool retCheck = readNodeInfo(node);
    if(retVal && !retCheck)retVal=false;

    Db.Release(); buff.Raz();

return retVal;
}

/////////////////////////////////////////////////////////////////

bool
mysqlAccessor::readNodeInfo(StDbNodeInfo* node){

    node->deleteInfoPointers(); // deletes (if needed) all strings loaded here
    //    node->mstrDup(node->dbName,mdbName);
    if(!buff.ReadScalar(node->structName,"structName"))return 0;
    if(!buff.ReadScalar(node->nodeID,"ID")) return 0;
    if(!buff.ReadScalar(node->nodeType,"nodeType"))return 0;
    if(!buff.ReadScalar(node->elementID,"elementID"))return 0;

    char* tmpString;
    if(buff.ReadScalar(tmpString,"baseLine")){
       if(strstr(tmpString,"Y"))node->IsBaseLine=true;
       delete [] tmpString;
    }

    if(buff.ReadScalar(tmpString,"isBinary")){
       if(strstr(tmpString,"Y"))node->IsBinary=true;
       delete [] tmpString;
    }

    if(buff.ReadScalar(tmpString,"isIndexed")){
       if(strstr(tmpString,"N"))node->IsIndexed=false;
       delete [] tmpString;
    }

    /*
    cout << " name = " << node->name ;
    cout << " version = " << node->versionKey;
    cout << " nodeType = " << node->nodeType;
    cout << " cstructName = " << node->structName << endl;
    */
return true;
};

////////////////////////////////////////////////////////////////
bool
mysqlAccessor::storeNodeInfo(StDbNodeInfo* node){

  // note this is a protected method - I demand all stores are
  // done after a queryNodeInfo to check if it is already
  // in the database

  Db<<"insert into Nodes set name='"<<node->name<<"', versionKey='";
  Db<<node->versionKey<<"', nodeType='"<<node->nodeType<<"', structName='";
  Db<<node->structName<<"'";

  if(node->IsBaseLine)Db<<", baseLine='Y'";
  if(node->IsBinary)Db<<", isBinary='Y'";
  if(!node->IsIndexed)Db<<", isIndexed='N'";
  if(node->elementID)Db<<", elementID='"<< node->elementID<<"'"; 

  Db<<endsql;

  if(!Db.QueryStatus()){
     node->nodeID=0;
     return false;
  }

  node->nodeID = Db.GetLastInsertID();
  Db.Release();

return true;
}

////////////////////////////////////////////////////////////////
bool
mysqlAccessor::hasInstance(StDbNodeInfo *node){

  Db.Release(); buff.Raz();
  char thisNode[10];
  ostrstream os(thisNode,10);
  os<<node->nodeID<<ends;

  Db<<"select * from dataIndex where nodeID="<<thisNode;
  Db<<" AND version='"<<node->versionKey<<"'"<<endsql;

  if(Db.NbRows() != 0) return true;
  Db.Release();

return false;
}


////////////////////////////////////////////////////////////////////

int
mysqlAccessor::QueryDescriptor(StDbTable* table){

  // Query the database for the elements associated with this
  // tableName and with the schemaID that is added at the 
  // constructure or over-written by an input file ... e.g. requested SchemaID

  if(!table->hasDescriptor()){

    Db.Release();
    buff.Raz();

    char* tableName=table->getName();
    char* cstructName;

    Db<<"SELECT structure.name, structure.lastSchemaID, structure.ID ";
    Db<<" from structure left join Nodes on structure.name=Nodes.structName";
    Db<<" WHERE Nodes.name='"<<tableName <<"'"<<endsql;

    if(!Db.Output(&buff)){
      cerr<<"QueryDb::Table no cstruct for requested Name "<<tableName<< endl;
      return 0;
    }
  
    buff.SetClientMode();
    if(!buff.ReadScalar(cstructName,"name")){
      cerr<<"QueryDb::Table no cstruct for requested Name "<<tableName<< endl;
      delete [] tableName;
      buff.Raz();
      return 0;
    }
 
    int schemaID;
    int structID;
    buff.ReadScalar(schemaID,"lastSchemaID");
    buff.ReadScalar(structID,"ID");

    Db.Release(); buff.Raz();
    
    int requestSchemaID;
    if(!(requestSchemaID=table->getSchemaID())){
      requestSchemaID=schemaID;
      table->setSchemaID(schemaID);
    } 
  
    char strID[10];
    char schID[10];
    ostrstream ostr(strID,10); ostr<<structID<<ends;
    ostrstream osch(schID,10); osch<<requestSchemaID<<ends;

    Db<<"SELECT schema.name, schema.schemaID, schema.type, schema.length, ";
    Db<<"schema.position from schema WHERE schema.structID=";
    Db<<strID<<" AND schema.schemaID="<<schID;
    Db<<" ORDER by schema.position"<<endsql;
     
    StDbTableDescriptor* descriptor = 0;
    while(Db.Output(&buff)){
      if(!descriptor)descriptor = new StDbTableDescriptor();
      descriptor->fillElement(&buff,requestSchemaID);
      buff.Raz();
    }
        
    if(descriptor){
      table->setDescriptor(descriptor);    
    } else {
      return 0;
    }

    Db.Release();
    delete [] tableName;
    delete [] cstructName;

  }  
   
return 1;
}

int
mysqlAccessor::WriteDb(StDbConfigNode* node, int currentID){

StDbNodeInfo currentNode;

  node->getNodeInfo(&currentNode);
  if(!currentID){
    currentNode.mstrCpy(currentNode.nodeType,"Config");
  } else {
    currentNode.mstrCpy(currentNode.nodeType,"directory");
  }

  if(!queryNodeInfo(&currentNode)){
       if(!storeNodeInfo(&currentNode))return 0;
  }

   char parentID[10];
   ostrstream pid(parentID,10); pid<<currentNode.nodeID<<ends;
   Db.Release();

  if(currentID){

    char startID[10];
    ostrstream sid(startID,10); sid<<currentID<<ends;
    Db<<"insert into NodeRelation set ParentID="; 
    Db<<startID<<", NodeID="<<parentID<<endsql;
    Db.Release();

  }

  StDbNodeInfo childNode;  

  if(node->hasData()){
     StDbTableIter* itr=node->getStDbTableIter();
     StDbTable* table = 0;
     while(!itr->done()){

       table= (StDbTable*)itr->next();
       if(table){
         table->getNodeInfo(&childNode);
         if(!queryNodeInfo(&childNode)){ // put it in
             if(!storeNodeInfo(&childNode)) return 0;
         table->addWrittenNode(childNode.nodeID);
         }
       }

       char childID[10];
       ostrstream cid(childID,10); cid<<childNode.nodeID<<ends;
       
       Db<<"insert into NodeRelation set ParentID=";
       Db<<parentID<<", NodeID="<<childID<<endsql;
       Db.Release();
       
     }
  }

return currentNode.nodeID;
}
            
/////////////////////////////////////////////////////////////////
bool
mysqlAccessor::rollBack(StDbNode* node){

  if(node->canRollBack()){
    char nodeID[10]; 
    ostrstream os(nodeID,10); os<<node->getNodeID() << ends;
    Db<<"delete from Nodes where ID="<<nodeID<<endsql;
    Db.Release();
    Db<<"delete from NodeRelations where ParentID="<<nodeID;
    Db<<" OR NodeID="<<nodeID<<endsql;
    Db.Release();
    node->commit();
  }

return true;
} 

/////////////////////////////////////////////////////////////////
bool
mysqlAccessor::rollBack(StDbTable* table){

int numrows;
int* dataIDs;

 if((dataIDs=table->getWrittenRows(&numrows))){
     
   char* dataString = new char[4*numrows+1];
   ostrstream os(dataString,4*numrows+1);
   for(int i=0;i<numrows-1;i++)os<<"dataID="<<dataIDs[i]<<" OR ";
   os<<"dataID="<<dataIDs[numrows-1]<<ends;
   if(table->IsBinary()){
     Db<<"Delete from bytes where "<<dataString<<endsql;
   } else {
     Db<<"Delete from "<<table->getCstrName()<<" where "<<dataString<<endsql;
   }

   Db.Release();
   if(table->IsIndexed()){
      char * nodeID = new char[10];
      ostrstream nos(nodeID,10); os<<table->getNodeID()<<ends;
      Db<<"delete from dataIndex where nodeID="<<nodeID<<" AND (";
      Db<<dataString<<")"<<endsql;
   }

   delete [] dataString;
   Db.Release();
   table->commitData();

 }

return true;
}

/////////////////////////////////////////////////////////////////

char*
mysqlAccessor::getDateTime(unsigned int time){

char* retVal=0;
char thisTime[100];
ostrstream os(thisTime,100); os<<time<<ends;
// note the " + 0" part requests it without delimiters 
// e.g. 1999-01-01 00:12:20 becomes 19990101001220

    Db<<"select from_unixtime("<<thisTime<<") + 0 as requestTime"<<endsql;

  if(!Db.Output(&buff))return retVal;
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

  if(!Db.Output(&buff))return 0;
  buff.SetClientMode(); 
  buff.ReadScalar(retVal,"requestTime");
  buff.Raz();
  Db.Release();

return retVal;
}

/////////////////////////////////////////////////////////////////

bool
mysqlAccessor::IsConnected() {
  // simple test of the connect state

  Db.Release();
  Db<<"show tables"<<endsql;
  if(Db.NbRows() == 0)return false;
  Db.Release();

return true;

}










