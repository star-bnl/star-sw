/***************************************************************************
 *
 * $Id: mysqlAccessor.cc,v 1.22 2000/03/09 20:11:30 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Storage specific SQL queries to database
 *
 ***************************************************************************
 *
 * $Log: mysqlAccessor.cc,v $
 * Revision 1.22  2000/03/09 20:11:30  porter
 * modified datetime string format for endTime in QueryDb(table)
 *
 * Revision 1.21  2000/03/06 17:11:49  porter
 * - WriteDb(table) returns true if no data is in table
 * - fixed memory leak introduced in 2/18/00 update.
 * - modified descriptor algorythm for OnlRunDescriptor.
 *
 * Revision 1.20  2000/03/01 20:56:16  porter
 * 3 items:
 *    1. activated reConnect for server timeouts
 *    2. activated connection sharing; better resource utilization but poorer
 *       logging
 *    3. made rollback method in mysqlAccessor more robust (affects writes only)
 *
 * Revision 1.19  2000/02/24 20:30:49  porter
 * fixed padding for uchar; beginTime in mysqlAccessor;
 * added rollback safety checkes in StDbManger
 *
 * Revision 1.18  2000/02/18 16:58:09  porter
 * optimization of table-query, + whereClause gets timeStamp if indexed
 *  + fix to write multiple rows algorithm
 *
 * Revision 1.17  2000/02/15 20:27:45  porter
 * Some updates to writing to the database(s) via an ensemble (should
 * not affect read methods & haven't in my tests.
 *  - closeAllConnections(node) & closeConnection(table) method to mgr.
 *  - 'NullEntry' version to write, with setStoreMode in table;
 *  -  updated both StDbTable's & StDbTableDescriptor's copy-constructor
 *
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
mysqlAccessor::mysqlAccessor(const char* serverName, int portNumber): theEndTime(2145934799), mdbName(0), mdbType(dbStDb), mdbDomain(dbDomainUnknown) {

mserverName=new char[strlen(serverName)+1];
strcpy(mserverName,serverName);
mportNumber = portNumber;

}

////////////////////////////////////////////////////////////////

mysqlAccessor::~mysqlAccessor(){

if(mdbName) delete [] mdbName;
if(mserverName) delete [] mserverName;

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

   if(StDbManager::Instance()->IsVerbose()){
     char qString[1024];
     ostrstream qs(qString,1024);
    qs<<"Select subNode.* from Nodes ";
    qs<<"LEFT JOIN NodeRelation ON Nodes.ID=NodeRelation.ParentID ";
    qs<<"LEFT JOIN Nodes as subNode ON NodeRelation.NodeID=subNode.ID ";
    qs<<" Where Nodes.ID="<<thisNode<<ends;

    cout << "NodeRelation Query = " << endl;
    cout << qString<<endl;
   }

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

///////////////////////////////////////////////////////////////

int
mysqlAccessor::QueryDb(StDbNode* node){

  StDbNodeInfo currentNode;
  if(!node->IsConfigured()){
   if(!prepareNode(node,&currentNode))return 0;
  }

return 1;
}


////////////////////////////////////////////////////////////////

int
mysqlAccessor::QueryDb(StDbTable* table, unsigned int reqTime){

  //
  //  Using name & version information in StDbTable + request timestamp
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
  // and in query of data

  char elementString[4096];
  ostrstream es(elementString,4096);

  int i;
  if(elementID){
   es << " AND elementID IN(";
   for(i=0;i<numRows-1;i++)es<<elementID[i]<<",";
   es<<elementID[numRows-1]<<")"<<ends;
  } else {
    // don't query on elementID
    es<<" "<<ends;
  }

  // zero beginTime for table
  table->setBeginTime((unsigned int)0);
  char* bTime=getDateTime(0);  
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
    Db.Release(); buff.Raz();
    //    table->setEndTime(eTime);
    table->setEndTime(getUnixTime(eTime));
    char* tmpEtime=getDateTime(table->getEndTime());
    table->setEndTime(tmpEtime); delete [] tmpEtime;    
  } else {
    table->setEndTime(getEndTime()); // simply use dec-31st 2037, 11:59:59
    eTime=getEndDateTime();
    table->setEndTime(eTime); // simply use dec-31st 2037, 11:59:59
  }

  if(eTime) {
     delete [] eTime;
     eTime=0;
  }
  unsigned int t1,t2;
  t1=t2=0;
  int countRows = 0;
  int eID;

  if(!currentNode.IsBinary){ // each dataID points to data row

    if(!elementID){//then request all rows of this version+timestamp

     Db << " select DISTINCT elementID from dataIndex";
     Db << baseString <<" order by elementID"<< endsql;

     numRows=0; int nmax = 500;
     int swapSize; int* storeIt = new int[nmax]; int* swapIt=0;

     while(Db.Output(&buff)){
       buff.ReadScalar(eID,"elementID");
       if(numRows==nmax){ // up the 'storeIt' array
        swapSize = 2*nmax; 
        swapIt = new int[swapSize];
        memcpy(swapIt,storeIt,4*nmax);
        delete [] storeIt;  storeIt = swapIt;  swapIt = 0;
        nmax = swapSize;
       }
      storeIt[numRows]=eID;
      numRows++;
      buff.Raz();
     }

     // now copy contents of 'storeIt' into elementID array
     elementID = new int[numRows];
     memcpy(elementID,storeIt,(unsigned int)numRows*sizeof(int));              
     delete [] storeIt;
     table->setElementID(elementID,numRows);
     
    }

     char Nrows[10];
     ostrstream NR(Nrows,10); NR<<numRows<<ends;

     // query Index to retrieve pointer(s) to data
     char qs[1024]; ostrstream queryS(qs,1024);
     queryS << " select elementID, unix_timestamp(Max(beginTime)) as mbeginTime ";
     queryS << " from dataIndex";
     queryS << baseString <<" AND beginTime<='"<<rTime<<"' "<<elementString;
     queryS << " Group by elementID order by elementID DESC limit ";
     queryS <<Nrows<<ends;       

     Db << qs << endsql;

     if(StDbManager::Instance()->IsVerbose())       
        cout << " My ID Query :: " << endl << qs << endl;

     bool * foundElem = new bool[numRows];
     int * id2data = new int[numRows];
     unsigned int* btimes = new unsigned int[numRows];
     for(i=0;i<numRows;i++){
       foundElem[i]=false;
       id2data[i]=0;
       btimes[i]=0;
     }

     int icount=0;
     int j=0; 
     int dataID;

     while(Db.Output(&buff)){
          buff.ReadScalar(t1,"mbeginTime");
          buff.ReadScalar(eID,"elementID");
          //buff.ReadScalar(dataID,"dataID");       
          for(i=j;i<numRows;i++){ 
            if(eID==elementID[i]){
               if(i==j)j++;
               foundElem[i]=true;
               // id2data[i]=dataID;
               btimes[i] = t1;
               if(t1>t2)t2=t1;
               break;
            }
          }
          icount++;
     }

     Db.Release(); buff.Raz(); 

     if(icount==0){
       cerr<<"WARNING::QueryDb Table: no valid data found in DB"<<endl;
       return 0;
     }
       
     if(icount<numRows)
        cerr<<"WARNING::QueryDb: not all rows are filled from DB"<<endl;

     icount=0;
     int rcount=0;
     for(i=0;i<numRows;i++){
       char dquery[1024]; ostrstream dqy(dquery,1024);
       if(foundElem[i]){
         dqy<<"Select dataID from dataIndex "<< baseString;
         dqy<<" And elementID="<<elementID[i];
         dqy<<" And beginTime=from_unixtime("<<btimes[i]<<")"<<ends;
         if(StDbManager::Instance()->IsVerbose())
             cout << " My dataID Query :: " << endl << dquery << endl;
         Db<<dquery<<endsql;
         icount++;
         if(Db.Output(&buff)){
            buff.ReadScalar(dataID,"dataID");
            id2data[i]=dataID;
            Db.Release();
            rcount++;
         }
       }
     }

     if(rcount!=icount){
        cerr<<"WARNING::QueryDb: not all dataIDs are found In DB"<<endl;
        cerr<<" rows requested = " << icount <<" rows read = " <<rcount<<endl;
     }

     char dataIDString[4096];
     ostrstream dstr(dataIDString,4096);
     dstr<<" dataID In(";
     for(i=0;i<numRows-1;i++)dstr<<id2data[i]<<",";
     dstr<<id2data[numRows-1]<<") "<<ends;

     char qdata[4096];
     ostrstream qd(qdata,4096);
     
     qd << " Select * from "<<currentNode.structName;
     qd << " where "<<dataIDString<<ends;
      
     if(StDbManager::Instance()->IsVerbose())       
        cout << " My Data Query :: " << endl << qdata << endl;
    
     Db << qdata <<endsql;
  
     j=0; dataID=0;
     while(Db.Output(&buff)){
       buff.ReadScalar(dataID,"dataID");
          for(i=j;i<numRows;i++){ 
            if(dataID==id2data[i]){
               if(i==j)j++;
               table->setRowNumber(i);
               table->dbStreamer(&buff,true);
               countRows++;
               break;
            }
          }
     buff.Raz();
     }

     Db.Release(); buff.Raz();

     delete [] foundElem;
     delete [] id2data;
     delete [] btimes;

     if(countRows==0) {
       cerr<<"ERROR::Query reference to data is broken: no data found"<<endl; 
       return 0;
     }

  } else {   // binary data store

   // query Index to retrieve pointer to data
    Db << " select dataID, numRows, unix_timestamp(Max(beginTime)) as mbeginTime" ;
    Db << " from dataIndex";
    Db << baseString <<" AND beginTime<='"<<rTime;
    Db << "' Group by dataID order by beginTime DESC limit 1"<<endsql;  
 
    int dataID;
    if(Db.Output(&buff)){
      buff.ReadScalar(t2,"mbeginTime");
      buff.ReadScalar(dataID,"dataID");
      buff.ReadScalar(numberOfRows,"numRows");
      Db.Release(); buff.Raz();
    } else {
      cerr<<"QueryDb::Table no valid row for this query" << endl;
      return 0;
    }

    char thisIndex[100];
    ostrstream inds(thisIndex,100); inds<<dataID<<ends;

   // get data for this Index pointer
  
    Db<< "Select * from bytes where dataID="<<thisIndex<<endsql;

   // send data into the table
    table->SetNRows(numberOfRows);
    if(Db.Output(&buff)){
       table->dbTableStreamer(&buff,"bytes",true);
       countRows+=numberOfRows;
       buff.Raz();
    }
    Db.Release();

  } //End binary stream

  //store max-beginTime & min-EndTime
    if(bTime)delete [] bTime;
    bTime = getDateTime(t2);
    table->setBeginTime(bTime);
    table->setBeginTime(t2);
   
   // reset row number to 0 for future dbStreaming
  table->setRowNumber();
  table->setStoreMode(false);

  if(countRows != table->GetNRows()){
     cerr <<"Query::Table: Mismatch between NRows Requested & Delivered"<<endl;
     cerr <<" NRows Requested = "<<table->GetNRows() << "  ";
     cerr <<" NRows Delivered = "<<countRows<<endl;
  }

  if(bTime)delete [] bTime;
  if(eTime)delete [] eTime;
  if(rTime)delete [] rTime;
  if(elementID) delete [] elementID;
  buff.Raz();
  Db.Release();

  return 1; 
}  

////////////////////////////////////////////////////////////////

int
mysqlAccessor::QueryDb(StDbTable* table, const char* whereClause){

   if(!table->hasDescriptor()){
      if(!QueryDescriptor(table))return 0;
   }

   if(table->IsBinary()) return 0;

   Db.Release();
   Db<<"Select * from "<<table->getCstrName()<<" "<<whereClause<<endsql;

   int icount=0;
   int dataID=0;
   table->setRowNumber();
   while(Db.Output(&buff)){
     if(table->IsIndexed() && icount==0)buff.ReadScalar(dataID,"dataID");
     table->dbStreamer(&buff,true);
     buff.Raz();
     icount++;
   }

   if(icount==0){
     cerr<<"Query::Table NO DATA via whereClause [ "<<endl;
     cerr<<" Select * from "<<table->getMyName()<<" "<<whereClause<<" ]"<<endl;
     return 0;
   }

   // This checks to see if row is indexed on timestamp
   // & if so, get the beginTime & endTime for returned row
   if(dataID){
      int nodeID = table->getNodeID();
      if(!nodeID){
        if(!QueryDb((StDbNode*)table))return icount;
        nodeID=table->getNodeID();
      }

      char NodeID[12]; char DataID[12];
      ostrstream nid(NodeID,12); ostrstream did(DataID,12);
      nid<<nodeID<<ends;  did<<dataID<<ends;
      char* bTime=0;

      Db.Release();
      Db<<"select beginTime from dataIndex where nodeID="<<NodeID;
      Db<<" and dataID="<<DataID<<" limit 1 "<<endsql;

      if(Db.Output(&buff)){
        if(buff.ReadScalar(bTime,"beginTime")){
           table->setBeginTime(bTime);
           table->setBeginTime(getUnixTime(bTime));
        }
      } else { return icount; }

      Db.Release();
      Db<<"select beginTime from dataIndex where nodeID="<<NodeID;
      Db<<" and dataID="<<DataID<<" And beginTime>'"<<bTime<<"' ";
      Db<<" order by beginTime limit 1 "<<endsql;

      char* eTime=0;

      if(Db.Output(&buff)){
        if(buff.ReadScalar(eTime,"beginTime")){
           table->setEndTime(eTime);
           table->setEndTime(getUnixTime(eTime));
        }
      } else { if(bTime)delete [] bTime; return icount; }
      
      if(eTime) delete [] eTime;
   }


return icount;
}
 
////////////////////////////////////////////////////////////////

int
mysqlAccessor::WriteDb(StDbTable* table, unsigned int storeTime){

    if(!table->hasData() && !table->IsStoreMode()){
      table->commitData(); // prevents rollback on this table
      if(!StDbManager::Instance()->IsQuiet()) 
         cout<<"WriteDb::Info no data in table="<<table->getMyName()<<". No write done"<<endl;
      return 1;
    }
  
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
     // then we should store the node information in DB
     // I assume the table has the right node information
     char* cName = table->getCstrName();
     if(cName)currentNode.structName = currentNode.mstrDup((const char*)cName);
     currentNode.elementID  = ((StDbNode*)table)->getElementID();
     currentNode.IsBaseLine = table->IsBaseLine();
     if(!storeNodeInfo(&currentNode)){
       table->commitData();return 0;}
  }


    table->setNodeID(currentNode.nodeID);


  // check if it baseline &, if so, if an instance is already stored 
  if(currentNode.IsBaseLine && hasInstance(&currentNode)){
    cerr << "WriteDb::Table Error: trying to add to baseline instance"<<endl;
    table->commitData();
    return 0;
  }

  // some node information can change e.g. isIndexed, structName,...

  if(!table->hasDescriptor()){
    if(!QueryDescriptor(table)){ table->commitData(); return 0;}
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

  int dataID=0;
  int startDataID=0;
  if(!table->hasData()) dataID=findDefaultID(table);

  if(currentNode.IsBinary){

    if(dataID==0 && table->hasData()) {  //dataID may = 0 if !indexed
     table->dbTableStreamer(&buff,"bytes",false);
     if(!Db.Input("bytes",&buff)){
      table->setRowNumber();
      table->commitData();
      return 0;
    }

    dataID = Db.GetLastInsertID();
    Db.Release(); buff.Raz();

   }

    if(currentNode.IsIndexed){
    // now write to index
     buff.WriteScalar(table->getSchemaID(),"schemaID");
     buff.WriteScalar(sTime,"beginTime");
     buff.WriteScalar(table->getVersion(),"version");
     buff.WriteScalar(dataID,"dataID");  
     buff.WriteScalar(currentNode.nodeID,"nodeID");
     buff.WriteScalar(nrows,"numRows");

     if(!Db.Input("dataIndex",&buff) && table->hasData()){ // write to index or delete the data
       Db.Release();
       // roll it back
       deleteRows("bytes",&dataID,1);
       table->setRowNumber();
       table->commitData();
       return 0;
     }
    }
    table->addWrittenRow(dataID);
          
  } else {  // not binary

     int eID;
     int* storedData = new int[nrows];
     int* storedIndex = new int[nrows];
     startDataID = dataID;
     for(int i=0; i<nrows; i++){
  
    if(startDataID==0 && table->hasData()){ //dataID may = 0 if !indexed
       table->dbStreamer(&buff,false);
       Db.Input(currentNode.structName,&buff); // input to database
       dataID = Db.GetLastInsertID(); // get auto-generated row-id
       Db.Release(); buff.Raz();
    }
       storedData[i]=dataID;
      
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

        if(!Db.Input("dataIndex",&buff) && table->hasData()){ 
        // write row address or delete data
          Db.Release();
        // roll back this transaction
          deleteRows(currentNode.structName,storedData,i+1);
          deleteRows("dataIndex",storedIndex,i);
          table->setRowNumber(); // reset row number to 0 for next dbStreaming
          table->commit(); //zero written rows
          delete [] storedData; 
          delete [] storedIndex;
          table->commitData();
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
  table->setStoreMode(false);
  delete [] sTime;

return 1;
}

//////////////////////////////////////////////////////////////

int
mysqlAccessor::findDefaultID(StDbTable* table){


 if(!table->IsIndexed()) return 0;

 buff.Raz(); Db.Release();

 Db<<"Select ID from Nodes where name='"<<table->getMyName()<<"'";
 Db<<" and versionKey='NullEntry'"<<endsql;

 int NodeID;
 if(!Db.Output(&buff)){
   if(StDbManager::Instance()->IsVerbose()){
     cout<<"findDefaultID:: failed - No NullEntry version of this table"<<endl;
     cout<<"Will Insert 'NullEntry' for table = "<<table->getMyName()<<endl;
   }
     StDbTable* table2 = table->Clone();
     table2->setVersion("NullEntry");   
     table2->setStoreMode(true);
     buff.Raz(); Db.Release();
     WriteDb(table2,(unsigned int)0);
     delete table2;

 } else {

   if(!buff.ReadScalar(NodeID,"ID")){
     if(StDbManager::Instance()->IsVerbose())
       cout<<"findDefaultID:: failed - No NullEntry-nodeID found"<<endl;
     Db.Release();
     return 0;
   }
 }

 char nodeID[10]; ostrstream os(nodeID,10); os<<NodeID << ends;
 Db<<"Select dataID from dataIndex where nodeID="<<nodeID<<" AND ";
 Db<<" version='NullEntry'"<<endsql;

 int dataID=0;

 if(!Db.Output(&buff)){
   if(StDbManager::Instance()->IsVerbose())
     cout<<"Will Insert 'NullEntry' for table = "<<table->getMyName()<<endl;
   StDbTable* table2 = table->Clone();
   table2->setVersion("NullEntry");   
   buff.Raz(); Db.Release();
   WriteDb(table2,(unsigned int)0);
   dataID=findDefaultID(table2);
   delete table2;
   return dataID;
 } else {
   buff.ReadScalar(dataID,"dataID");
 }
 buff.Raz(); Db.Release();

return dataID;
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
    cout << " node ID = " << node->nodeID << endl;
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

//////////////////////////////////////////////////////////////

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
int* dataIDs=table->getWrittenRows(&numrows);

 if(dataIDs){
     
   for(int i=0;i<numrows-1;i++){
     char dataString[1024];
     ostrstream os(dataString,1024);
     os<<" dataID="<<dataIDs[i]<<ends;
     if(table->IsBinary()){
        Db<<"Delete from bytes where "<<dataString<<endsql;
      } else {
        Db<<"Delete from "<<table->getCstrName()<<" where "<<dataString<<endsql;
      }

     Db.Release();
     if(table->IsIndexed()){
        char nodeID[10];
        ostrstream nos(nodeID,10); nos<<table->getNodeID()<<ends;
        Db<<"delete from dataIndex where nodeID="<<nodeID<<" AND ";
        Db<<dataString<<endsql;
     }
   }
    Db.Release();
 }

table->commitData();
 
return true;
}

/////////////////////////////////////////////////////////////////

void
mysqlAccessor::selectDb(const char* dbName, StDbType type, StDbDomain domain){

  //  if(mdbName) cout << " Switching from db="<<mdbName;
  //  cout << " Selecting Database="<<dbName << endl;
 
mdbType = type;
mdbDomain = domain;
if(mdbName) delete [] mdbName;
mdbName=new char[strlen(dbName)+1];
strcpy(mdbName,dbName);
Db.setDefaultDb(mdbName);
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

return Db.IsConnected();
}










