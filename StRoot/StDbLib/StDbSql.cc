/***************************************************************************
 *
 * $Id: StDbSql.cc,v 1.10 2001/08/02 17:37:19 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Implementation class of StDataBaseI in (My)SQL
 *
 ***************************************************************************
 *
 * $Log: StDbSql.cc,v $
 * Revision 1.10  2001/08/02 17:37:19  porter
 * fixed problem in fetch by where-clause used in online in StDbSql.cc.
 * also got rid of warning comparing unsigned int to int.
 *
 * Revision 1.9  2001/07/23 16:39:30  porter
 * removed an extraneous "cout" left in by mistake
 *
 * Revision 1.8  2001/04/23 19:24:31  porter
 * fixed row limit & initial buffer contents for query by where clause
 *
 * Revision 1.7  2001/03/30 18:48:26  porter
 * modified code to keep Insure from wigging-out on ostrstream functions.
 * moved some messaging into a StDbSql method.
 *
 * Revision 1.6  2001/02/23 18:35:47  porter
 * cleaned up a warning messages when compiled under HP's aCC
 *
 * Revision 1.5  2001/02/22 23:01:56  porter
 * Re-introduced many-to-one name-to-table capability
 * & robustness for query errors
 *
 * Revision 1.4  2001/02/09 23:06:25  porter
 * replaced ostrstream into a buffer with ostrstream creating the
 * buffer. The former somehow clashed on Solaris with CC5 iostream (current .dev)
 *
 * Revision 1.3  2001/02/08 23:23:56  porter
 * fixed initialization of schemaID in table & fixed some warnings when
 * compiled with NODEBUG
 *
 * Revision 1.2  2001/01/23 14:38:16  porter
 * fixed bug in parsing flavor string where string contains a list of flavors.
 *
 * Revision 1.1  2001/01/22 18:37:59  porter
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
 *
 **************************************************************************/
#include "StDbSql.hh"
#include "StDbManager.hh"
#include "StDbDefaults.hh"
#include "StDbConfigNodeImpl.hh"
#include "StDbTableIter.hh"
#include "StDbTable.h"

#include <strstream.h>
#include <iostream.h>

#define __CLASS__ "StDbSql"

static const char* qFailed = "Query Failed = ";
static const char* qInfo   = " Query = ";
static const char* qResult = " Query Result = ";

//////////////////////////////////////////////////////////////////

StDbSql::StDbSql(MysqlDb &db, StDbBuffer& buffer) : StDataBaseI(), mretString(0), mtableCatalog(0), mdefaultEndDateTime(0),Db(db), buff(buffer) { mgr=StDbManager::Instance();}

//////////////////////////////////////////////////////////////////

StDbSql::StDbSql(MysqlDb &db, StDbBuffer& buffer, StDbType tpe, StDbDomain dom) : StDataBaseI(tpe, dom), mretString(0), mtableCatalog(0),mdefaultEndDateTime(0), Db(db), buff(buffer) { mgr=StDbManager::Instance();}

//////////////////////////////////////////////////////////////////

StDbSql::StDbSql(MysqlDb &db, StDbBuffer& buffer, const char* tpe, const char* dom) : StDataBaseI(tpe, dom), mretString(0), mtableCatalog(0), mdefaultEndDateTime(0),Db(db), buff(buffer){ mgr=StDbManager::Instance();}

//////////////////////////////////////////////////////////////////

StDbSql::~StDbSql() {  
  if(mretString) delete [] mretString;
  if(mdefaultEndDateTime) delete [] mdefaultEndDateTime;
  deleteDescriptors(); 
}

//////////////////////////////////////////////////////////////////
//
//   Real Work Begins here
//
//////////////////////////////////////////////////////////////////

int
StDbSql::QueryDb(StDbConfigNode* node) {

#define __METHOD__ "QueryDb(StDbConfigNode*)"

 StDbNode curNode;
 curNode.setDbName(mdbName);
 curNode.setDbType(mdbType);
 curNode.setDbDomain(mdbDomain);
 int NodeID;
 int branchID=node->getBranchID();

 if(!((NodeID)=prepareNode(node)))return 0;
 
 // Build node query string

 Db<<"Select subNode.*, NodeRelation.ID as branchID from Nodes ";
 Db<<"LEFT JOIN NodeRelation ON Nodes.ID=NodeRelation.ParentID ";
 Db<<"LEFT JOIN Nodes as subNode ON NodeRelation.NodeID=subNode.ID ";
 Db<<" Where Nodes.ID="<<NodeID;
 Db<<" and NodeRelation.BranchID="<<branchID<<endsql;

 if(!Db.QueryStatus()) 
    return sendMess(qFailed,Db.printQuery(),dbMWarn,__LINE__,__CLASS__,__METHOD__);

 if(!Db.NbRows())
    sendMess(node->printName()," Node has no subnodes",dbMDebug,__LINE__,__CLASS__,__METHOD__);

 sendMess(qInfo,Db.printQuery(),dbMDebug,__LINE__,__CLASS__,__METHOD__);

//
// Loop over rows in Configuration

  while(Db.Output(&buff)){
    curNode.setConfigured(false);
    if(readNodeInfo(&curNode)){

      ostrstream fs;
      fs<<"Found "<<curNode.printNodeType()<<" Node "<<curNode.printName();
      fs<<" of parent "<<node->printName()<<ends;
      sendMess(qResult,fs.str(),dbMDebug,__LINE__,__CLASS__,__METHOD__);
      fs.freeze(0);

      if(strcmp(curNode.printNodeType(),"table")!=0){ // it is a ConfigNode

        StDbConfigNode* child= new StDbConfigNodeImpl(node,curNode);
        if(!child->isDbNode())readConfigNodeInfo(child);

      } else {

        StDbTable* table = node->addTable(&curNode);
        readTableInfo(table);
      }
    } 
    buff.Raz(); 
  }
  Db.Release();
  return 1;
#undef __METHOD__
}

//////////////////////////////////////////////////////////
int
StDbSql::QueryDb(StDbNode* node){
#define __METHOD__ "QueryDb(StDbNode*)"

  if(!prepareNode(node))  return 0; 
  if(node->IsTable()){
    readTableInfo((StDbTable*) node);
  } else {
    readConfigNodeInfo((StDbConfigNode*)node);
  }

return node->getNodeID();
#undef __METHOD__
}

//////////////////////////////////////////////////////////
int
StDbSql::QueryDb(StDbTable* table, unsigned int reqTime){

#define __METHOD__ "QueryDb(table,time)"

  //  Using name & version information in StDbTable + request timestamp
  //  fill data from the database into the StDbTable pointer.
  //
  int retVal=1;
  if(!table) return 0;
  char* tName = table->printName(); 
  table->clearStoreInfo();
  int nodeID;
  if(!((nodeID)=table->getNodeID())){
    if(!(prepareNode((StDbNode*)table)))
    return sendMess(tName," Table not found in DB",dbMErr,__LINE__,__CLASS__,__METHOD__);
 
    readTableInfo(table);
    clear();    
  }
  
  char* checkString=checkTablePrepForQuery(table,true); // returns null if ok
  if(checkString) 
     return sendMess(tName,checkString,dbMErr,__LINE__,__CLASS__,__METHOD__);

 // start preparing the queries

 // common where clause
   ostrstream bs;
   bs<<" Where nodeID="<<nodeID;
 // prepare "flavor" part of query 
   bs<<" AND "<<getFlavorQuery(table->getFlavor());
 // prepare for production time
   bs<<" AND "<<getProdTimeQuery(table->getProdTime()); 
 // terminate the baseString
   bs<<" "<<ends;

   char* baseString = bs.str();
   bs.freeze(0);

 //--> add element ID list part of query
  int numRows;
  int* elementID=table->getElementID(numRows);
  if(!elementID)
    return sendMess(tName,"doesn't have an Element List",dbMErr,__LINE__,__CLASS__,__METHOD__);

  char* elementString=getElementList(elementID,numRows);
  char* dataTable=getDataTable(table,reqTime);

 // Query DB for the endtime -> earliest time of next row 
  Db << " select beginTime + 0 as mendDateTime, ";
  Db << " unix_timestamp(beginTime) as mendTime from "<<dataTable;
  Db << baseString <<" AND beginTime>from_unixtime("<<reqTime<<")"; 
  Db << " And elementID In("<<elementString<<")";
  Db << " Order by beginTime limit 1"<<endsql; 
    
  sendMess(qInfo,Db.printQuery(),dbMDebug,__LINE__,__CLASS__,__METHOD__);

  if(!Db.QueryStatus())
     return sendMess(qFailed,Db.printQuery(),dbMWarn,__LINE__,__CLASS__,__METHOD__);

  if(Db.NbRows()==1 && Db.Output(&buff)){
    char* edTime=0; 
    int eTime;
    buff.ReadScalar(edTime,"mendDateTime");
    buff.ReadScalar(eTime,"mendTime");
    table->setEndTime(eTime);
    table->setEndTime(edTime);
    if(edTime) delete [] edTime;
    clear();
  } else {
    setDefaultEndTime(table);
  }

  //  unsigned int t1=0;
  //  unsigned int bTime=0;
  char* flav=0;
  int eID;

 // --> prep for data query which can be 1 or more queries
   int maxID=1;
   int i;
   for(i=0;i<numRows;i++)if(elementID[i]>maxID)maxID=elementID[i];
   int* idMap = new int[maxID+1];
   int* dataIDList = new int[numRows];
   unsigned int* timeValues = new unsigned int[numRows];
   for(i=0;i<numRows;i++){
        idMap[elementID[i]]=i;
        dataIDList[i]=0;
        timeValues[i]=0;
   }

// --> end of prep <--
  // query is done where elementID is not necessarily distinct.
  // yet the in the query we set limit=numRows.
  //  - we take 1st of instance of each elementID returned.
  //  - we make a new list of those not yet found
  //  - redo query with limit="rowsLeft"
  // continue until we're done

  int rowsLeft=numRows;
  bool done=false;
  while(!done){  //done if all elementIDs are found or (break) if null

   Db <<" select unix_timestamp(beginTime) as bTime,"<<dataTable<<".* from ";
   Db << dataTable << baseString;
   Db <<" AND beginTime<=from_unixtime("<<reqTime<<")";
   Db <<" AND elementID In("<<elementString<<") "; 
   Db <<" Order by beginTime desc limit "<< rowsLeft <<endsql;

   sendMess(qInfo,Db.printQuery(),dbMDebug,__LINE__,__CLASS__,__METHOD__);

  if(!Db.QueryStatus()){
    sendMess(qFailed,Db.printQuery(),dbMWarn,__LINE__,__CLASS__,__METHOD__);
    break;
  }
  
   if(Db.NbRows()==0)break;     

   int numRowsFound=0;
   while(Db.Output(&buff)){
      buff.ReadScalar(eID,"elementID");
    // check to see if this eID is already returned & don't use if so
      if(!dataIDList[idMap[eID]]){
        buff.ReadScalar(dataIDList[idMap[eID]],"dataID");
        buff.ReadScalar(timeValues[idMap[eID]],"bTime");
        buff.ReadScalar(flav,"flavor");
     // check to see if instance is null values: if so return 0 as query result
        if((done)=checkForNull(flav)){
     	  retVal=0;
          delete [] flav;
          break;
        }
        table->setRowNumber(idMap[eID]);
        table->dbStreamer(&buff,true); // stream data into table
	//        if(bTime>t1)t1=bTime;
        delete [] flav;
        numRowsFound++;
     }
     buff.Raz();
   }

   rowsLeft=rowsLeft-numRowsFound;
 // Check for left overs & modify query for repeat
   if(!done && (rowsLeft>0)){
     int* elementsLeft = new int[rowsLeft];
     int j=0;
     for(i=0;i<numRows;i++){
       if(!dataIDList[i]){
	 elementsLeft[j]=elementID[i];
         j++;
       }
     }
     elementString = getElementList(elementsLeft,rowsLeft);
   } else {
     done=true;
   }

  } // --> end of while loop

  if(rowsLeft==numRows){
     sendMess(tName," has No data for query",dbMWarn,__LINE__,__CLASS__,__METHOD__);
     setDefaultBeginTime(table,reqTime);
     retVal=0;
   } else if(rowsLeft>0){
     ostrstream tp;
     tp<<" Not all rows filled from DB, Requested="<<numRows;
     tp<<" Returned="<<numRows-rowsLeft<<" for Table="<<ends;
     mgr->printInfo(tp.str(),tName,dbMWarn,__LINE__,__CLASS__,__METHOD__);
     tp.freeze(0);
   }

  if(retVal){
    table->addWrittenRows(dataIDList,numRows);
    table->setTimeValues(timeValues);
    unsigned int t1=table->getMaxTime();
    table->setBeginTime(t1);
    char* dt=getDateTime(t1);
    table->setBeginTime(dt); delete [] dt;
    table->setRowNumber(); // reset current row to 0
  }

  delete [] idMap;
  delete [] dataIDList;
  delete [] dataTable;

  Db.Release();  
  return retVal;
#undef __METHOD__
} 

////////////////////////////////////////////////////////////
int
StDbSql::QueryDb(StDbTable* table, const char* whereClause){

#define __METHOD__ "QueryDb(StDbTable*, const char* where)"

  unsigned int* timeSet=QueryDbTimes(table,whereClause);
  if(!timeSet)return 0;
  delete [] timeSet;
  return table->GetNRows();
#undef __METHOD__
}

///////////////////////////////////////////////////////////////////
unsigned int*
StDbSql::QueryDbTimes(StDbTable* table, const char* whereClause){

#define __METHOD__ "QueryDb(StDbTable*, const char* where)"

  /*
     rules for # of rows returned (by user request via SetNRows(int nrows);)
     1. table->GetNRows()= 0 or N means no limit or limit N
     2. returned table->GetNRows()=M where M is how many returned

     rules for beginTime & endTime
     1. Non-indexed tables have arbitrary returned times
     2. beginTime="lastest" begin time of returned rows
     3. endTime="earliest" begin time of set of "next" rows
  */

  unsigned int* retVal=0;
  char* tName=table->printName();

  char* checkString=checkTablePrepForQuery(table);
  if(checkString){  // then error message
    sendMess(tName,checkString,dbMErr,__LINE__,__CLASS__,__METHOD__);
    return retVal;
  }

   setDefaultReturnValues(table,0);
   int numRows = table->getRowLimit();
   int numTables;
   int numRowsReturned=0;
   unsigned int t1=0;

   char* columnList=getColumnList(table);
   if(!columnList){
     sendMess(tName," has no elements?",dbMErr,__LINE__,__CLASS__,__METHOD__);
     return retVal;
   }

   table->setElementID((int*)retVal,0); // no rows to begin with
   table->setRowNumber();

   char** dataTables=getDataTables(table,numTables);
   int i;
   for(i=0;i<numTables;i++){

     Db<<" select unix_timestamp(beginTime) as bTime,";
     Db<<" "<<columnList<<" from "<<dataTables[i]<<" "<<whereClause;
     if(numRows)Db<<" limit "<<numRows;
     Db<<endsql;
     sendMess(qInfo,Db.printQuery(),dbMDebug,__LINE__,__CLASS__,__METHOD__);
     if(!Db.QueryStatus()){
      sendMess(qFailed,Db.printQuery(),dbMWarn,__LINE__,__CLASS__,__METHOD__);
      return retVal;
     }

     int retRows=Db.NbRows();
     if(retRows==0) continue;

     int* elements = new int[retRows];
     int* dataIDList = new int[retRows];
     unsigned int* timeList = new unsigned int[retRows];
     table->addNRows(retRows);
     // table->setRowNumber();

     int j=0;
     while(Db.Output(&buff)){
       buff.ReadScalar(timeList[j],"bTime");
       buff.ReadScalar(elements[j],"elementID");
       buff.ReadScalar(dataIDList[j],"dataID");
       table->dbStreamer(&buff,true);
       if(timeList[j]>t1)t1=timeList[j];
       j++;
       buff.Raz();
     }
     table->addNElements(elements,retRows);
     table->addWrittenRows(dataIDList,retRows);
     table->setBeginTime(t1);     

     unsigned int* tmpRet=new unsigned int[numRowsReturned+retRows];
     if(retVal){
       memcpy(tmpRet,retVal,numRowsReturned*sizeof(int));
       delete [] retVal;
     }
     tmpRet+=numRowsReturned;
     memcpy(tmpRet,timeList,retRows*sizeof(int));
     retVal=tmpRet;
     numRowsReturned+=retRows;
     Db.Release();

     if(table->IsIndexed() && t1>0){
       Db<<" select unix_timestamp(beginTime) as eTime from "<<dataTables[i];
       Db<<" where beginTime>from_unixtime("<<t1<<")";
       Db<<" and elementID In("<<getElementList(elements,retRows)<<")";
       Db<<" Order by beginTime desc limit 1"<<endsql;
     
       sendMess(qInfo,Db.printQuery(),dbMDebug,__LINE__,__CLASS__,__METHOD__);

       if(Db.Output(&buff)){
         unsigned int eTime;
 	 buff.ReadScalar(eTime,"endTime");
         if(eTime<table->getEndTime())table->setEndTime(eTime);
         buff.Raz();
       }    
       Db.Release();       
     } 
     delete [] elements;
     delete [] dataIDList;
     delete [] timeList;
   }// loop over tables

   for(i=0;i<numTables;i++) delete [] dataTables[i];
   delete [] dataTables;
   if(retVal){
       char* dateTime=getDateTime(table->getBeginTime());
       table->setBeginTime(dateTime); if(dateTime) delete [] dateTime;
       dateTime=getDateTime(table->getEndTime());
       table->setEndTime(dateTime); if(dateTime) delete [] dateTime;
   }     
   return retVal;
#undef __METHOD__
}

////////////////////////////////////////////////////////////////
int
StDbSql::QueryDbFunction(StDbTable* table, const char* whereClause, char* funcName) {
#define __METHOD__ "QueryDbFunction(table,whereClause,functionName)"
  /* 
     Method to provide access to Max, Min, Ave,... functions where
     the result per storage table is put in a row in the StDbTable.
     This only works for those columns stored in basic Mysql types 
  */

  char* checkString=checkTablePrepForQuery(table); // null is good
  if(checkString)
     return sendMess(table->printName(),checkString,dbMErr,__LINE__,__CLASS__,__METHOD__);

   int numTables;
   int numRowsReturned=0;

   char* columnList=getColumnList(table,funcName);
   if(!columnList)return 0;

   char** dataTables=getDataTables(table,numTables);
   int i;
   for(i=0;i<numTables;i++){

     ostrstream qs;
     Db<<" select "<<columnList<<" from "<<dataTables[i];
     Db<<" "<<whereClause<<endsql;

     sendMess(qInfo,Db.printQuery(),dbMDebug,__LINE__,__CLASS__,__METHOD__);

     if(!Db.QueryStatus())
        return sendMess(qFailed,Db.printQuery(),dbMWarn,__LINE__,__CLASS__,__METHOD__);

     int retRows=Db.NbRows();
     if(retRows==0) continue;
     numRowsReturned+=retRows;
     table->setRowNumber();  // reset to first row.

     while(Db.Output(&buff)){ table->dbStreamer(&buff,true); buff.Raz(); }
     Db.Release();
   }

   for(i=0;i<numTables;i++) delete [] dataTables[i];
   delete [] dataTables;

   return numRowsReturned;
#undef __METHOD__
}; 

////////////////////////////////////////////////////////////////
int
StDbSql::WriteDb(StDbTable* table, unsigned int storeTime){

#define __METHOD__ "WriteDb(StDbTable*,uint storeTime)"

  int retVal=1;
  char* tName=table->printName();

  if(!table->hasData())
    return sendMess(tName," has no data to store",dbMWarn,__LINE__,__CLASS__,__METHOD__)+1;

  int nodeID;
  if(!((nodeID)=table->getNodeID()))
    if(!(prepareNode((StDbNode*)table)))
         return sendMess(tName," Not found in DB",dbMErr,__LINE__,__CLASS__,__METHOD__);

  readTableInfo(table);
  clear();    

  char* dataTable;
  if(!((dataTable)=getDataTable(table,storeTime)))
     return sendMess(tName," has no storage table",dbMErr,__LINE__,__CLASS__,__METHOD__);

  if(table->IsBaseLine() && hasInstance(table)) 
     return sendMess("BaseLine instance already exists",tName,dbMErr,__LINE__,__CLASS__,__METHOD__);

  if(!QueryDescriptor(table))
     return sendMess(tName," doesn't have a descriptor",dbMErr,__LINE__,__CLASS__,__METHOD__);

  table->setRowNumber(); // set to 0
  int numRows;
  int* elements = table->getElementID(numRows);

  if(!elements)
    return sendMess(tName,"doesn't have an Element List",dbMErr,__LINE__,__CLASS__,__METHOD__);

  int* storedData = new int[numRows];
  memset(storedData,0,numRows*sizeof(int));
  char* sTime = getDateTime(storeTime);
  int rowsWritten = 0;

  // write each row & roll back full writes if any fail.
  // - Also, for the time being, write to the old index 
  //   so that one can read with older versions of the code

  table->commitData();
  table->clearStoreInfo();
  table->setDataTable(dataTable);

  for(int i=0;i<numRows;i++){
      clear();
      buff.WriteScalar(nodeID,"nodeID");
      buff.WriteScalar(table->getSchemaID(),"schemaID");
      buff.WriteScalar(sTime,"beginTime");
      buff.WriteScalar(elements[i],"elementID");
      if(!table->defaultFlavor())buff.WriteScalar(table->getFlavor(),"flavor");
      table->dbStreamer(&buff,false);

      if(!Db.Input(dataTable,&buff)){
        deleteRows(dataTable,storedData,i);
        retVal=0;
        break;
      } else {
	storedData[i]=Db.GetLastInsertID();
      }
      clear();
      if(!writeOldIndex(nodeID,table->getSchemaID(),sTime,elements[i],table->getFlavor(),storedData[i])){
	deleteRows(dataTable,storedData,i);
        deleteOldIndex(storedData,i,nodeID);
        retVal=0;
        break;
      }
     rowsWritten++;
   }
  if(rowsWritten==numRows)table->addWrittenRows(storedData,numRows,true);

  delete [] storedData;
  delete [] sTime;

  // same set of writes except - write null endtimes if requested 
  if(table->getEndStoreTime()!=0 && rowsWritten==numRows){ 

    sTime = getDateTime(table->getEndStoreTime());
    storedData = new int[numRows];
    rowsWritten=0;

    for(int i=0;i<numRows;i++){
      clear();
      buff.WriteScalar(nodeID,"nodeID");
      buff.WriteScalar(table->getSchemaID(),"schemaID");
      buff.WriteScalar(sTime,"beginTime");
      buff.WriteScalar(elements[i],"elementID");
      buff.WriteScalar("null","flavor");

      if(!Db.Input(dataTable,&buff)){
        deleteRows(dataTable,storedData,i);
        int numWritten;
        deleteRows(dataTable,table->getWrittenRows(numWritten),numWritten);
        retVal=0;
        break;
      } else {
	storedData[i]=Db.GetLastInsertID();
      }
      clear();
      if(!writeOldIndex(nodeID,table->getSchemaID(),sTime,elements[i],"null", storedData[i])){
	deleteRows(dataTable,storedData,i);
        int numWritten;
        deleteOldIndex(table->getWrittenRows(numWritten),numWritten,nodeID);
        retVal=0;
        break;
      }

      rowsWritten++;
    }
    if(rowsWritten==numRows)
    table->addWrittenRows(storedData,numRows,true);
    delete [] storedData;
    delete [] sTime;  
  }

  table->setRowNumber();
  delete [] dataTable;  

  if(!retVal)sendMess(" Write failed for table=",tName,dbMWarn,__LINE__,__CLASS__,__METHOD__);
  return retVal;
#undef __METHOD__
}

////////////////////////////////////////////////////////////////////
int
StDbSql::QueryDescriptor(StDbTable* table){
#define __METHOD__ "QueryDescriptor(StDbTable*)"

  // Query the database for the elements associated with this
  // tableName and with the schemaID that is added at the 
  // constructure or over-written by an input file ... e.g. requested SchemaID

if(table->hasDescriptor())return 1;

    clear();    

    Db<<" SELECT structure.lastSchemaID, structure.ID from structure left join Nodes on structure.name=Nodes.structName";
    Db<<" WHERE Nodes.name='"<<table->printName() <<"'"<<endsql;
    
    if(!Db.Output(&buff))return 0;
    
    int schemaID;
    int structID;
    buff.ReadScalar(schemaID,"lastSchemaID");
    buff.ReadScalar(structID,"ID");
    clear();

    int requestSchemaID;
    if(!(requestSchemaID=table->getSchemaID())){
      requestSchemaID=schemaID;
      table->setSchemaID(schemaID);
    } 
  
    StDbTableDescriptor* descriptor = getDescriptor(structID,requestSchemaID);
    table->setDescriptor(descriptor);

    if(descriptor->IsValid())return 1;

    Db<<"SELECT  schema.schemaID, schema.name, schema.type, schema.length, ";
    Db<<"schema.position from schema WHERE schema.structID="<<structID;
    Db<<" AND schema.schemaID="<<requestSchemaID;
    Db<<" ORDER by schema.position"<<endsql;

    sendMess(qInfo,Db.printQuery(),dbMDebug,__LINE__,__CLASS__,__METHOD__);

    if(!Db.QueryStatus())
       return sendMess(qFailed,Db.printQuery(),dbMWarn,__LINE__,__CLASS__,__METHOD__);


    if(Db.NbRows()==0) {
      deleteDescriptor(structID,requestSchemaID);
      return 0;
    }

    while(Db.Output(&buff)){
      descriptor->fillElement(&buff,requestSchemaID);
      buff.Raz();
    }

    Db.Release();
    addDescriptor(descriptor);

  return 1;
#undef __METHOD__
}
      
//////////////////////////////////////////////////////////////
int
StDbSql::WriteDb(StDbConfigNode* node, int parentID, int& configID){
#define __METHOD__ "WriteDb(node,parentID,configID)"

  if(!node) return 0;
 
  char* nName = node->printName();

  if(!parentID){

    if(strcmp(node->printNodeType(),"Config")!=0)
      return sendMess("No Config tag for new config=",nName,dbMErr,__LINE__,__CLASS__,__METHOD__);

    if(!node->printVersion())
      return sendMess("No version label for new config=",nName,dbMErr,__LINE__,__CLASS__,__METHOD__);

  } else {
    node->setNodeType("directory");
  }

  int nodeID;
  if(!((nodeID)=storeConfigNode(node)))
      return sendMess(" Could not store ",nName,dbMErr,__LINE__,__CLASS__,__METHOD__);
  
  // write this node
  if(parentID) { 
    insertNodeRelation(configID,parentID,nodeID);
  } else {
    configID=node->getNodeID();
  }

  // do this node's tables
  if(node->hasData()){
    StDbTableIter* itr=node->getStDbTableIter();
    while(!itr->done()){
      int childID=0;
      StDbTable* table= itr->next();
      table->setNodeType("table");
      if(!((childID)=storeTableNode(table)))
	  return sendMess(" Could not store table in Node=",nName,dbMErr,__LINE__,__CLASS__,__METHOD__);
      insertNodeRelation(configID,nodeID,childID);
    }	
  }

return nodeID;
}

////////////////////////////////////////////////////////////////////
bool
StDbSql::writeOldIndex(int nodeID, int schemaID, const char* sTime,int elementID,const char* flavor, int dataID){

  Db<<"insert into dataIndex set nodeID="<<nodeID<<", dataID="<<dataID;
  Db<<", schemaID="<<schemaID<<", beginTime='"<<sTime<<"', elementID=";
  Db<<elementID<<", flavor='"<<flavor<<"'"<<endsql;

  return Db.QueryStatus();
}

////////////////////////////////////////////////////////////////////
void
StDbSql::deleteOldIndex(int* dataIDs, int numRows, int nodeID){
  Db<<" delete from dataIndex where nodeID="<<nodeID;
  Db<<" and dataID In("<<getElementList(dataIDs,numRows)<<")"<<endsql;
}

///////////////////////////////////////////////////////////////
void
StDbSql::deleteRows(const char* tableName, int* rowID, int nrows){

  if(!rowID || nrows==0)return;
  Db<<" delete from "<<tableName;
  Db<<" where dataID In("<<getElementList(rowID,nrows)<<")"<<endsql; 
}

/////////////////////////////////////////////////////////////////
bool
StDbSql::rollBack(StDbNode* node){

  if(!(node->canRollBack()) || !(node->getNodeID())) return false;
  Db<<"delete from Nodes where ID="<<node->getNodeID()<<endsql;
  return Db.QueryStatus();
}

/////////////////////////////////////////////////////////////////
bool
StDbSql::rollBack(StDbTable* table){

  int numRows;
  int* numWrittenRows = table->getWrittenRows(numRows);
  char* dataTable = table->getDataTable();
  char* elementList = getElementList(numWrittenRows,numRows);
  
  Db<<"delete from "<<dataTable<<" where dataID In("<<elementList<<")"<<endsql;
  Db.Release();

  //TEMPORARY
  Db<<" delete from dataIndex where nodeID="<<table->getNodeID();
  Db<<" AND dataID In("<<elementList<<")"<<endsql;

  bool retVal=Db.QueryStatus();
  Db.Release();
  delete [] dataTable;

  return retVal;
}

////////////////////////////////////////////////////////////
int*
StDbSql::selectElements(const char* elementName, StDbElementIndex* inval, int& numElements){
  // if table does not have element rows (elementName=="None") 
  // then will return default elementID=0, numElements=1
  // if table does not have elements based on StDbElementIndex,
  // then will return null pointer & numElements=0 --> NO DATA CAN BE Gotten
  // else, will return element list & numElements based on query

  int* retElements = 0;
  numElements=1;
  if(!elementName) return retElements;
  if(strcmp(elementName,"None")==0){
      retElements = new int[1]; retElements[0]=0;
      return retElements;
  }

  int numIndeces = inval->getNumIndeces();
  clear();

  Db<<" select elementID from "<<elementName<<"IDs";
  if(numIndeces>0){
    Db<<" where "<<inval->printIndexName(0)<<"="<<inval->getIndexVal(0);
    for(int i=1; i<numIndeces; i++)
      Db<<" AND "<<inval->printIndexName(i)<<"="<<inval->getIndexVal(i);
  }
  Db<<endsql;
  numElements=Db.NbRows();

  if(numElements==0) return retElements;
  
  retElements = new int[numElements];
  int j=0;
  while(Db.Output(&buff)){
    buff.ReadScalar(retElements[j],"elementID");
    j++;
    buff.Raz();
  }

  clear();
  return retElements;
}

///////////////////////////////////////////////////////////
char**
StDbSql::getIndexNames( const char* elementName, int& numIndexes){

  Db<<"select * from elementIndexes ";
  Db<<" where elementName='"<<elementName<<"'"<<endsql;

  char** indexNames = 0;
  if(!((numIndexes)=Db.NbRows())) return indexNames;

  indexNames = new char*[numIndexes];
  int i = 0;
  while(Db.Output(&buff)){
      buff.ReadScalar(indexNames[i],"indexName");
      i++;
      buff.Raz();
  }

 clear();
 return indexNames;
} 

///////////////////////////////////////////////////////////
StDbTableDescriptor*
StDbSql::findDescriptor(int structID, int schemaID){

 StDbTableDescriptor* td = 0;
 for(DescList::iterator itr = mdescriptors.begin();
     itr != mdescriptors.end(); ++itr){
   if( ((*itr)->getSchemaID()==schemaID) && ((*itr)->getStructID()==structID)){
     td = *itr;
     break;
   }
 }
 return td;
}

///////////////////////////////////////////////////////////
StDbTableDescriptor*
StDbSql::getDescriptor(int structID, int schemaID){
  StDbTableDescriptor* retVal=findDescriptor(structID,schemaID);
  if(retVal) return new StDbTableDescriptor(*retVal); // make copy 
  retVal=new StDbTableDescriptor(structID,schemaID); // -else- make new
  return retVal;
}

////////////////////////////////////////////////////////////
void
StDbSql::addDescriptor(StDbTableDescriptor* td){ 
  mdescriptors.push_back( new StDbTableDescriptor((*td)) ); // add a copy
};

////////////////////////////////////////////////////////////
void
StDbSql::deleteDescriptors(){
DescList::iterator itr;
StDbTableDescriptor* desc;

  do {
     for(itr=mdescriptors.begin(); itr != mdescriptors.end(); ++itr){
         desc=*itr;
         mdescriptors.erase(itr);
         if(desc)delete desc;
         break;
     }
  } while (mdescriptors.begin() != mdescriptors.end() );

}

////////////////////////////////////////////////////////////
void
StDbSql::deleteDescriptor(int structID, int schemaID) {

  StDbTableDescriptor* desc;
  for(DescList::iterator itr = mdescriptors.begin();
      itr != mdescriptors.end(); ++itr){
      if(structID==(*itr)->getStructID() && schemaID==(*itr)->getSchemaID()){
        desc=(*itr);
	mdescriptors.erase(itr);
        delete [] desc;
        break;
      }
  }
}

////////////////////////////////////////////////////////////
void
StDbSql::setDefaultReturnValues(StDbTable* table, unsigned int reqTime){
  if(!table)return;
  setDefaultBeginTime(table,reqTime);
  setDefaultEndTime(table);
  table->clearStoreInfo();
}

//////////////////////////////////////////////////////////////
void StDbSql::setDefaultBeginTime(StDbTable* table, unsigned int reqTime){
  // set default return times
  char* stime=getDateTime(reqTime);
  table->setBeginTime(reqTime);                   // uint version
  table->setBeginTime(stime);                    // char* version
  if(stime) delete [] stime;
}

//////////////////////////////////////////////////////////////
void StDbSql::setDefaultEndTime(StDbTable* table){
  if(!mdefaultEndDateTime)initEndTime();
  table->setEndTime(mdefaultEndDateTime);
  table->setEndTime(mdefaultEndTime);
}

//////////////////////////////////////////////////////////////
void StDbSql::initEndTime(){
  mdefaultEndTime=StDbDefaults::Instance()->getEndTime();
  mdefaultEndDateTime=getDateTime(mdefaultEndTime);
}

//////////////////////////////////////////////////////////////
int
StDbSql::prepareNode(StDbNode* dbNode){

  //-> takes the input dbNode's name & version and calls the db
  //   (via method queryNode()) for the other information
  //   and sets into dbNode

  if(  (strcmp(dbNode->printNodeType(),"DB")!=0) && 
       (dbNode->IsConfigured())) return dbNode->getNodeID();

  dbNode->setDbName(mdbName);
  dbNode->setDbType(mdbType);
  dbNode->setDbDomain(mdbDomain);

  return queryNode(dbNode);;
}

///////////////////////////////////////////////
int
StDbSql::queryNode(StDbNode* node){

int retVal=0;

    Db<< "Select * from Nodes where Nodes.name='"<<node->printName()<<"'";
    Db<<" AND Nodes.versionKey='"<<node->printVersion()<<"'"<<endsql;

    if(Db.Output(&buff) && readNodeInfo(node))retVal=node->getNodeID();
    if(!retVal) clear();

return retVal;
}

/////////////////////////////////////////////////////////////////
bool
StDbSql::readNodeInfo(StDbNode* node){

  int nodeID;
  char* tmpString;
 
  if(!buff.ReadScalar(nodeID,"ID")) return false;
  if(!buff.ReadScalar(tmpString,"nodeType"))return false;
  node->setNodeType(tmpString);
  delete [] tmpString;

  if(!buff.ReadScalar(tmpString,"name"))return false;
  node->setName(tmpString); delete [] tmpString;

  if(!buff.ReadScalar(tmpString,"versionKey"))return false;
  node->setVersion(tmpString); delete [] tmpString;

  node->setNodeID(nodeID);
  node->setConfigured(true);

  return true;
}

///////////////////////////////////////////////////////////////////
bool
StDbSql::readConfigNodeInfo(StDbConfigNode* node){

  char* iname;
  if(!buff.ReadScalar(iname,"indexName")) return false;
  if(strcmp(iname,"None")!=0){
     int id;
     buff.ReadScalar(id,"indexVal");
     node->setElementIndexInfo(iname,id);
  } 
  delete [] iname;  

  int branchID;
  buff.ReadScalar(branchID,"branchID");
  node->setBranchID(branchID);

  return true;
}

///////////////////////////////////////////////////////////////////
bool
StDbSql::readTableInfo(StDbTable* table){

char* tmpString;

    if(!buff.ReadScalar(tmpString,"structName"))return false;
    table->setCstructName(tmpString); delete [] tmpString;
    
    if(!buff.ReadScalar(tmpString,"indexName")) return false;
    table->setElementName(tmpString);
    delete [] tmpString;

    table->setBaseLine(checkValue("baseLine","Y"));
    table->setBinary(checkValue("isBinary","Y"));
    table->setIndexed(checkValue("isIndexed","Y"));

    table->setDbName(mdbName);
    table->setDbType(mdbType);
    table->setDbDomain(mdbDomain);

return true;
};

/////////////////////////////////////////////////////////////////
char*
StDbSql::insertNodeString(StDbNode* node){

  ostrstream dqs;
  dqs<<"insert into Nodes set name='"<<node->printName()<<"' ";
  if(!StDbDefaults::Instance()->IsDefaultVersion(node->printVersion()))
     dqs<<", versionKey='"<<node->printVersion()<<"' ";

  dqs<<", nodeType='"<<node->printNodeType()<<"'"<<ends;

  return mRetString(dqs);
}

////////////////////////////////////////////////////////////////
int
StDbSql::storeConfigNode(StDbConfigNode* node){

  int retVal=0;

  Db<<insertNodeString((StDbNode*) node);

  if(node->getNumIndeces()){
    char* ename=0; int eid;
    node->getElementIndexInfo(ename,eid);
    Db<<", indexName='"<<ename<<"'";
    Db<<", indexVal="<<eid;
    if(ename) delete [] ename;
  }
  Db<<endsql;

  if(Db.QueryStatus())retVal=Db.GetLastInsertID();
  clear();

  return retVal;
}
    
////////////////////////////////////////////////////////////////
int
StDbSql::storeTableNode(StDbTable* table){

  int retVal=0;
  if(!table->printCstructName()) return retVal;

  Db<<insertNodeString((StDbNode*) table);
  Db<<", structName='"<<table->printCstructName()<<"'";
  
  if(table->IsBaseLine())Db<<", baseLine='Y'";
  if(table->IsBinary())Db<<", isBinary='Y'";
  if(!table->IsIndexed())Db<<", isIndexed='N'";

  Db<<endsql;

  if(Db.QueryStatus())retVal=Db.GetLastInsertID();
  clear();

  return retVal;
}
    
/////////////////////////////////////////////////////////////////
bool
StDbSql::insertNodeRelation(int configID, int parent, int child){

  Db<<" insert into NodeRelation set ParentID="<<parent;
  Db<<", NodeID="<<child<<", ConfigID="<<configID<<endsql;

  bool retVal=Db.QueryStatus();
  Db.Release();

  return retVal;
}

/////////////////////////////////////////////////////////////////
unsigned int
StDbSql::getUnixTime(const char* time){

  unsigned int retVal = 0;
  Db<<"select unix_timestamp('"<<time<<"') as requestTime"<<endsql;
  if(Db.Output(&buff)) buff.ReadScalar(retVal,"requestTime");
  clear();

return retVal;
}

/////////////////////////////////////////////////////////////////
char*
StDbSql::getDateTime(unsigned int time){

  char* retVal=0;
// note the " + 0" part formats result without delimiters 
// e.g. 1999-01-01 00:12:20 becomes 19990101001220

  Db<<"select from_unixtime("<<time<<") + 0 as requestTime"<<endsql;
  if(Db.Output(&buff)) buff.ReadScalar(retVal,"requestTime");

  clear();

return retVal;
}

///////////////////////////////////////////////////////////////////
bool
StDbSql::checkValue(const char* colName, const char* colValue){

 bool retVal = false;
 char* tmpS=0;
 if(buff.ReadScalar(tmpS,colName) && strcmp(tmpS,colValue)==0)retVal=true; 
 if(tmpS) delete [] tmpS;

return retVal;
}

///////////////////////////////////////////////
char*
StDbSql::getFlavorQuery(const char* flavor){

// prepares SQL of " flavor In('flav1','flav2',...)"

 char *id1,*id2,*id3;
 id1 = new char[strlen(flavor)+1];
 strcpy(id1,flavor);
 id3=id1;

 ostrstream fs;
 fs<<" flavor In(";
 while(( (id2)=strstr(id3,"+")) ){
    *id2='\0';
    fs<<"'"<<id3<<"',";
    *id2='+';
    id2++;
    id3=id2;
 }
 fs<<"'"<<id3<<"')"<<ends;
 delete [] id1;

 return mRetString(fs);
}

///////////////////////////////////////////////
char*
StDbSql::getProdTimeQuery(unsigned int prodTime){

// prepares SQL of " entryTime<="
ostrstream pt;
  if(prodTime==0){
    pt<<" deactive=0 "<<ends;
  } else {
    pt<<" (deactive=0 OR deactive>="<<prodTime<<")";
    pt<<" AND unix_timestamp(entryTime)<="<<prodTime<<ends;
  } 
  return mRetString(pt);
}

///////////////////////////////////////////////
char* 
StDbSql::getElementList(int* e, int num){

// prepares comma separated list of integers
 ostrstream es;
 for(int i=0;i<num-1;i++)es<<e[i]<<",";
 es<<e[num-1]<<ends;

 return mRetString(es);
}

////////////////////////////////////////////////
char*
StDbSql::getColumnList(StDbTable* table,char* funcName){

  StTableDescriptorI* desc=table->getDescriptor();
  int numElements=desc->getNumElements();

  ostrstream es; es<<" ";

  int icount=0;
  for(int i=0;i<numElements;i++){
     if(funcName && (desc->getElementLength(i)>1))continue;
     char* name=desc->getElementName(i);
     if(funcName)es<<funcName<<"(";
     es<<name;
     if(funcName)es<<") as "<<name;
     if(i<(numElements-1))es<<", ";
     delete [] name;
     icount++;
  }

 es<<ends;
 if(icount) return mRetString(es);
 return (char*) 0;
}
  
////////////////////////////////////////////////
char*
StDbSql::checkTablePrepForQuery(StDbTable* table, bool checkIndexed){

  if(mretString){ 
     delete [] mretString; 
     mretString=new char[256];
  }

  if(!QueryDescriptor(table))
     return strcpy(mretString," doesn't have a descriptor ");

  if(table->IsBinary()) 
    return strcpy(mretString," Binary Store is currently disabled ");

  if(checkIndexed && !table->IsIndexed())
    return strcpy(mretString," Table is not time indexed");

  delete [] mretString;
  return mretString=0;
}

////////////////////////////////////////////////
bool
StDbSql::checkForNull(const char* src){
  return (strstr(src,"null")) ? true : false; 
}

///////////////////////////////////////////////
char*
StDbSql::getDataTable(StDbTable* table, unsigned int time){

  if(mtableCatalog==0) checkTableCatalog();
  if(mtableCatalog==1) return table->getCstructName();
  char* retVal=0;
  clear();

// as of now, just use name... later maybe more so I've sent in StDbTable*
char* tableName=table->printName();

Db<<" select * from tableCatalog where nodeName='"<<tableName<<"'";
Db<<" AND unix_timestamp(beginTime)<="<<time;
Db<<" Order by beginTime desc limit 1"<<endsql;

if(Db.Output(&buff))buff.ReadScalar(retVal,"tableName");
clear();

return (retVal) ? retVal : table->getCstructName();
}

//////////////////////////////////////////////////
char**
StDbSql::getDataTables(StDbTable* table,int& numTables){

 char** retVal;
 if(mtableCatalog==0) checkTableCatalog();
 if(mtableCatalog==1){
    numTables=1;
    retVal=new char*[1];
    retVal[0]=table->getCstructName();
    return retVal;
 }
 clear();

// as of now, just use name... later maybe more so I've sent in StDbTable*
 char* tableName=table->printName();
 Db<<" select * from tableCatalog where nodeName='"<<tableName<<"'"<<endsql;

 if(!((numTables)=Db.NbRows()) ){ // use tableName as the 1
     numTables++;
     retVal= new char*[numTables];
     retVal[0]=new char[strlen(tableName)+1];
     strcpy(retVal[0],tableName);
     return retVal;
 }

 retVal=new char*[numTables];
 int i=0;
 while(Db.Output(&buff)) if(buff.ReadScalar(retVal[i],"tableName"))i++;
 
 numTables=i;
return retVal;
}

////////////////////////////////////////////////////////////////
bool
StDbSql::hasInstance(StDbTable* table){

  // try and find a data instance of this table 
  // for use in keeping track of baseline instances
 bool retVal=false;
 clear();

 int numTables;
 char** dataTables=getDataTables(table,numTables);
 if(numTables==0) return retVal;

 for(int i=0; i<numTables;i++){

   Db<<" select * from "<<dataTables[i];
   Db<<" where "<<getFlavorQuery(table->getFlavor());
   Db<<" AND "<<getProdTimeQuery(table->getProdTime())<<endsql;

   if(Db.NbRows() !=0 ){
     retVal=true;
     break;
   }
 }
 for(int k=0;k<numTables;k++) delete [] dataTables[k];
 delete [] dataTables;
 return retVal;
}

//////////////////////////////////////////////////////////
void 
StDbSql::checkTableCatalog(){

  //  if(Db.checkForTable("tableCatalog"))mtableCatalog=1
  Db<<"show tables like 'tableCatalog'"<<endsql;
  if(Db.NbRows()==0){
    mtableCatalog=1;
  } else {
    mtableCatalog=2;
  }
  Db.Release();
}

//////////////////////////////////////////////////////////
void StDbSql::setDbUtils(MysqlDb& db, StDbBuffer& buffer){ 
  Db=db; 
  buff=buffer;
};

#undef __CLASS__
