/***************************************************************************
 *
 * $Id: StDbSql.cc,v 1.39 2016/05/25 20:40:01 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Implementation class of StDataBaseI in (My)SQL
 *
 ***************************************************************************
 *
 * $Log: StDbSql.cc,v $
 * Revision 1.39  2016/05/25 20:40:01  dmitry
 * coverity - reverse_inull
 *
 * Revision 1.38  2016/05/24 20:26:48  dmitry
 * coverity - unreachable delete loop suppression
 *
 * Revision 1.37  2015/05/15 19:05:12  dmitry
 * missed instance, assign zero to the pointer after delete
 *
 * Revision 1.36  2015/05/15 19:02:21  dmitry
 * assign zero to the pointer after delete
 *
 * Revision 1.35  2010/05/24 20:44:09  dmitry
 * suppressed excessive output for indexed tables
 *
 * Revision 1.34  2009/12/10 03:47:07  dmitry
 * BETWEEN operator was not constructed properly from elementID array => potential origin of several mysterious problems
 *
 * Revision 1.33  2009/11/03 00:04:54  dmitry
 * restored missing endTime check
 *
 * Revision 1.32  2009/09/25 19:14:09  dmitry
 * number of rows is reset to fetched number of rows, if it is less than total indexed number of rows
 *
 * Revision 1.31  2007/08/20 18:21:30  deph
 * New Version of Load Balancer
 *
 * Revision 1.30  2007/04/21 03:20:33  deph
 * removed extra check for endTime replacement
 *
 * Revision 1.29  2007/03/08 21:54:40  deph
 * Added quotes to identifier `schema` for compatibility to mysql 5.0.x
 *
 * Revision 1.28  2005/11/28 19:09:55  deph
 * small bug fix for a rare condition.
 *
 * Revision 1.27  2005/11/07 14:45:44  deph
 * Changed the SQL operator IN to one of the following: = if row =1 ; between if block of element IDs are contiguous; or IN if they are not
 *
 * Revision 1.26  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.25  2003/12/16 01:30:32  porter
 * additional fixes for change from ostrstream to StString that were not exposed until
 * running in online
 *
 * Revision 1.24  2003/09/26 20:40:37  deph
 * *** empty log message ***
 *
 * Revision 1.23  2003/09/23 04:37:16  porter
 * fixed leak of timeValues array
 *
 * Revision 1.22  2003/09/16 22:44:17  porter
 * got rid of all ostrstream objects; replaced with StString+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.21  2003/09/02 17:57:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.20  2003/04/11 22:47:36  porter
 * Added a fast multi-row write model specifically needed by the daqEventTag
 * writer. Speed increased from about 100Hz to ~3000Hz.  It is only invoked if
 * the table is marked as Non-Indexed (daqTags & scalers). For non-indexed tables
 * which include binary stored data (we don't have any yet), the fast writer  has
 * to invoke a slower buffer so that the rates are a bit slower (~500Hz at 50 rows/insert).
 *
 * Revision 1.19  2003/01/29 03:44:54  porter
 * added setRowNumber in QueryDbFunction method to simplify codes that use
 * this when plotting directly from the database
 *
 * Revision 1.18  2003/01/10 04:19:20  porter
 * added feature of getting timestamp list (but no data) for a table.
 * fixed 2 features sometimes used in online in query-by-whereclause.
 * removed a stray 'cout' in a routine that is rarely accessed
 *
 * Revision 1.17  2002/01/30 15:40:48  porter
 * changed limits on flavor tag & made defaults retrieving more readable
 *
 * Revision 1.16  2001/12/21 22:47:33  porter
 * added DbRelease for new endtime checks - this caused problems with tpcGas
 *
 * Revision 1.15  2001/12/21 04:54:46  porter
 * sped up table definition for emc and changed some ostrstream usage for
 * insure tests
 *
 * Revision 1.14  2001/12/19 20:44:52  porter
 * fixed endTime for run-level queries
 *
 * Revision 1.13  2001/12/05 17:16:35  porter
 * stand-alone make file no longer had "DLINUX" in compile but this is still needed
 * and returned. Also retrieve elementID list  in query by whereClause for plotting
 * many row instances.
 *
 * Revision 1.12  2001/10/26 20:59:46  porter
 * fixed new endtime flag from previous checkin. made StDataBaseI available
 * at root-cli.
 *
 * Revision 1.11  2001/10/24 04:05:20  porter
 * added long long type to I/O and got rid of obsolete dataIndex table
 *
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
#include "StDbFastSqlWriter.h"

#include "stdb_streams.h"

#define __CLASS__ "StDbSql"

static const char* DbQFailed = "Query Failed = ";
static const char* DbQInfo   = " Query = ";
static const char* DbQResult = " Query Result = ";

//////////////////////////////////////////////////////////////////

StDbSql::StDbSql(MysqlDb &db, StDbBuffer& buffer) : StDataBaseI(),Db(db), buff(buffer) { mgr=StDbManager::Instance(); init(); }

//////////////////////////////////////////////////////////////////

StDbSql::StDbSql(MysqlDb &db, StDbBuffer& buffer, StDbType tpe, StDbDomain dom) : StDataBaseI(tpe, dom), Db(db), buff(buffer) { 
mgr=StDbManager::Instance(); init(); 
}

//////////////////////////////////////////////////////////////////

StDbSql::StDbSql(MysqlDb &db, StDbBuffer& buffer, const char* tpe, const char* dom) : StDataBaseI(tpe, dom), Db(db), buff(buffer){ 
  mgr=StDbManager::Instance(); init();
}

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
    return sendMess(DbQFailed,Db.printQuery(),dbMWarn,__LINE__,__CLASS__,__METHOD__);

 if(!Db.NbRows())
    sendMess(node->printName()," Node has no subnodes",dbMDebug,__LINE__,__CLASS__,__METHOD__);

 sendMess(DbQInfo,Db.printQuery(),dbMDebug,__LINE__,__CLASS__,__METHOD__);

//
// Loop over rows in Configuration

  while(Db.Output(&buff)){
    curNode.setConfigured(false);
    if(readNodeInfo(&curNode)){

      StString fs;
      fs<<"Found "<<curNode.printNodeType()<<" Node "<<curNode.printName();
      fs<<" of parent "<<node->printName();
      sendMess(DbQResult,(fs.str()).c_str(),dbMDebug,__LINE__,__CLASS__,__METHOD__);
      

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
   StString bs;
   bs<<" Where nodeID="<<nodeID;
 // prepare "flavor" part of query 
   bs<<" AND "<<getFlavorQuery(table->printFlavor());
 // prepare for production time
   bs<<" AND "<<getProdTimeQuery(table->getProdTime()); 
 // terminate the baseString
   bs<<" ";

   string baseString = bs.str();


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
//  Db << " And elementID In("<<elementString<<")";
  Db << " And elementID "<<elementString;
  Db << " Order by beginTime limit 1"<<endsql; 
    
  sendMess(DbQInfo,Db.printQuery(),dbMDebug,__LINE__,__CLASS__,__METHOD__);

  if(!Db.QueryStatus())
     return sendMess(DbQFailed,Db.printQuery(),dbMWarn,__LINE__,__CLASS__,__METHOD__);

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
   //Db <<" AND elementID In("<<elementString<<") "; 
   Db <<" AND elementID "<<elementString; 
   Db <<" Order by beginTime desc limit "<< rowsLeft <<endsql;

   sendMess(DbQInfo,Db.printQuery(),dbMDebug,__LINE__,__CLASS__,__METHOD__);

  if(!Db.QueryStatus()){
    sendMess(DbQFailed,Db.printQuery(),dbMWarn,__LINE__,__CLASS__,__METHOD__);
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
        table->setRowNumber(idMap[eID]);
        table->dbStreamer(&buff,true); // stream data into table
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
     elementString = getElementListIN(elementsLeft,rowsLeft);
     delete [] elementsLeft;
   } else {
     done=true;
   }

  } // --> end of while loop

  if(rowsLeft==numRows){
     sendMess(tName," has No data for query",dbMWarn,__LINE__,__CLASS__,__METHOD__);
     setDefaultBeginTime(table,reqTime);
     retVal=0;
   } else if(rowsLeft>0){
     StString tp;
     tp<<" Not all rows filled from DB, Requested="<<numRows;
     tp<<" Returned="<<numRows-rowsLeft<<" for Table="<<tName;
     mgr->printInfo((tp.str()).c_str(),dbMDebug,__LINE__,__CLASS__,__METHOD__);
     //     numRows-=rowsLeft;
     //     table->resizeNumRows(numRows);
   }

  if(retVal){
    table->addWrittenRows(dataIDList,numRows);
    if (rowsLeft > 0) {
        StString tp;
        tp<<"Fixing row size, setting it to " << (numRows-rowsLeft) << " for Table = " << tName;
        mgr->printInfo((tp.str()).c_str(),dbMDebug,__LINE__,__CLASS__,__METHOD__);
        table->resizeNumRows(numRows-rowsLeft);
    }
    table->setTimeValues(timeValues);
    unsigned int t1=table->getMaxTime();
    table->setBeginTime(t1);
    char* dt=getDateTime(t1);
    table->setBeginTime(dt); delete [] dt;
    table->setRowNumber(); // reset current row to 0
  } else {
	  	delete [] timeValues;
	 }

  Db.Release();  

  // Dmitry: returned this line, because we skip endTime completely, if we don't do this fix
   if(retVal) retVal=(int)updateEndTime(table,dataTable,reqTime);

  delete [] idMap;
  delete [] dataIDList;
  delete [] dataTable;

 return retVal;
#undef __METHOD__
} 

////////////////////////////////////////////////////////////
bool StDbSql::checkColumn(const char* tableName, const char* columnName){

  bool retVal=false;
  Db<<"show columns from "<<tableName<<" like '"<<columnName<<"'"<<endsql;
  if(Db.NbRows()==1)retVal=true;
  Db.Release();
  return retVal;
}

////////////////////////////////////////////////////////////
bool StDbSql::updateEndTime(StDbTable* table, const char* dataTable, unsigned int requestTime){

   /********************************
     resets the table's endtime based on set endTime (if table has this column)
     instead of the running beginTime timestamp. Returns False if endTime is
     earlier than requestTime
    ********************************/

#define __METHOD__ "updateEndTime(table,table,time)"
  bool retVal = true;
  if(!checkColumn(dataTable,"endTime")) return retVal;
  int nrows;
  int* wrows = table->getWrittenRows(nrows);
  
   StString mpdOut, mpdOut2;


  Db<<" select unix_timestamp(Min(endTime)) as mendTime from "<<dataTable;
 // Db<<" where dataID In("<<getElementList(wrows,nrows)<<")"<<endsql;
  Db<<" where dataID "<<getElementList(wrows,nrows)<<endsql;

     sendMess(DbQInfo,Db.printQuery(),dbMDebug,__LINE__,__CLASS__,__METHOD__);  
  

if( (Db.NbRows()>0) && Db.Output(&buff)){
    unsigned int t1 = table->getEndTime();
    unsigned int t2;
    buff.ReadScalar(t2,"mendTime");
    if(t2<t1)table->setEndTime(t2);
    if(t2<requestTime) retVal=false;
    buff.Raz();  
  }

  Db.Release();
#undef __METHOD__
  return retVal;
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
StDbSql::QueryDbTimes(StDbTable* table, const char* whereClause, int opt){

#define __METHOD__ "QueryDb(StDbTable*, const char* where)"

  /*
     rules for # of rows returned (by user request via setRowLimit(int nrows);)
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


   table->setElementID((int*)retVal,0); // no rows to begin with
   table->setRowNumber();

   char** dataTables=getDataTables(table,numTables);
   int i;
   for(i=0;i<numTables;i++){

    char* columnList=0;
    if(!opt)columnList=getColumnList(table,dataTables[i]);
    if(!opt && !columnList){
      sendMess(tName," has no elements?",dbMErr,__LINE__,__CLASS__,__METHOD__);
      return retVal;
    }

     Db<<" select unix_timestamp("<<dataTables[i]<<".beginTime) as bTime,";
     Db<<" elementID ";
     if(!opt)Db<<","<<columnList;
     Db<<" from "<<dataTables[i]<<" "<<whereClause;
     if(numRows)Db<<" limit "<<numRows;
     Db<<endsql;
     sendMess(DbQInfo,Db.printQuery(),dbMDebug,__LINE__,__CLASS__,__METHOD__);
     if(!Db.QueryStatus()){
      sendMess(DbQFailed,Db.printQuery(),dbMWarn,__LINE__,__CLASS__,__METHOD__);
      return retVal;
     }

     int retRows=Db.NbRows();
     if(retRows==0) continue;

     int* elements = new int[retRows];
     int* dataIDList = new int[retRows];
     unsigned int* timeList = new unsigned int[retRows];
     if(!opt){
       table->addNRows(retRows);
     } else {
       table->resizeElementID(retRows+table->GetNRows());
     }
     // table->setRowNumber();

     int j=0;
     while(Db.Output(&buff)){
       buff.ReadScalar(timeList[j],"bTime");
       buff.ReadScalar(elements[j],"elementID");
       buff.ReadScalar(dataIDList[j],"dataID");
       if(!opt)table->dbStreamer(&buff,true);
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

     if(table->IsIndexed() && t1>0 && !opt){
       Db<<" select unix_timestamp(beginTime) as eTime from "<<dataTables[i];
       Db<<" where beginTime>from_unixtime("<<t1<<")";
       //Db<<" and elementID In("<<getElementList(elements,retRows)<<")";
       Db<<" and elementID "<<getElementList(elements,retRows);
       Db<<" Order by beginTime limit 1"<<endsql;
     
       sendMess(DbQInfo,Db.printQuery(),dbMDebug,__LINE__,__CLASS__,__METHOD__);

       if(Db.Output(&buff)){
         unsigned int eTime=0;
 	 if(buff.ReadScalar(eTime,"eTime") && eTime && eTime<table->getEndTime())table->setEndTime(eTime);
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
       table->setRowNumber();
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

   char** dataTables=getDataTables(table,numTables);
   int i;
   for(i=0;i<numTables;i++){

   char* columnList=getColumnList(table,dataTables[i],funcName);
   if(!columnList)return 0;

     Db<<" select "<<columnList<<" from "<<dataTables[i];
     Db<<" "<<whereClause<<endsql;

     sendMess(DbQInfo,Db.printQuery(),dbMDebug,__LINE__,__CLASS__,__METHOD__);

     if(!Db.QueryStatus())
        return sendMess(DbQFailed,Db.printQuery(),dbMWarn,__LINE__,__CLASS__,__METHOD__);

     int retRows=Db.NbRows();
     if(retRows==0) continue;
     numRowsReturned+=retRows;

     while(Db.Output(&buff)){ table->dbStreamer(&buff,true); buff.Raz(); }
     Db.Release();
   }

   for(i=0;i<numTables;i++) delete [] dataTables[i];
   delete [] dataTables;
   table->setRowNumber();

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


  if(table->IsBaseLine() && hasInstance(table)) 
     return sendMess("BaseLine instance already exists",tName,dbMErr,__LINE__,__CLASS__,__METHOD__);

  if(!QueryDescriptor(table))
     return sendMess(tName," doesn't have a descriptor",dbMErr,__LINE__,__CLASS__,__METHOD__);


  table->setRowNumber(); // set to 0
  if(!table->IsIndexed())return WriteDbNoIndex(table,storeTime);

  char* dataTable;
  if(!((dataTable)=getDataTable(table,storeTime)))
     return sendMess(tName," has no storage table",dbMErr,__LINE__,__CLASS__,__METHOD__);

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

  char* eTime=0;
  if( table->getEndStoreTime() && checkColumn(dataTable,"endTime"))
      eTime=getDateTime(table->getEndStoreTime());

  for(int i=0;i<numRows;i++){
      clear();
      buff.WriteScalar(nodeID,"nodeID");
      buff.WriteScalar(table->getSchemaID(),"schemaID");
      buff.WriteScalar(sTime,"beginTime");
      buff.WriteScalar(elements[i],"elementID");
      if(eTime)buff.WriteScalar(eTime,"endTime");
      if(!table->defaultFlavor())buff.WriteScalar(table->printFlavor(),"flavor");
      table->dbStreamer(&buff,false);

      if(!Db.Input(dataTable,&buff)){
        deleteRows(dataTable,storedData,i);
        retVal=0;
        break;
      } else {
	storedData[i]=Db.GetLastInsertID();
      }
      clear();
      rowsWritten++;
   }
  if(rowsWritten==numRows)table->addWrittenRows(storedData,numRows,true);

  delete [] storedData;
  delete [] sTime;
  if(eTime) delete [] eTime;

  table->setRowNumber();
  delete [] dataTable;  

  if(!retVal)sendMess(" Write failed for table=",tName,dbMWarn,__LINE__,__CLASS__,__METHOD__);
  return retVal;
#undef __METHOD__
}

////////////////////////////////////////////////////////////////////
int
StDbSql::WriteDbNoIndex(StDbTable* table, unsigned int storeTime){
#define __METHOD__ "WriteDbNoIndex(table,storeTime)"
 
  int retVal=0; 
  char* dataTable;
  if(!((dataTable)=getDataTable(table,storeTime)))
     return sendMess(table->printName()," has no storage table",dbMErr,__LINE__,__CLASS__,__METHOD__);

  StString cList;
  cList<<"beginTime,"<<getColumnList(table);

  char* sTime=getDateTime(storeTime);

  int numRows=table->GetNRows();
  char* colList = new char[strlen((cList.str()).c_str())+1];
  strcpy(colList,(cList.str()).c_str());

  int i;
  bool hasBinary=false;
  if(Db.InputStart(dataTable,&buff,colList,numRows,hasBinary)){
 
    if(hasBinary){ // got to go through the buffer

      cout<<" In Binary write???"<<endl;

      for(i=0;i<numRows;i++){
        buff.WriteScalar(sTime,"beginTime");
        table->dbStreamerWrite(&buff); //,false);
        if(!Db.InputRow(&buff,i)) break;
      }
      if( i==numRows && Db.InputEnd() ){
        retVal=1;
        table->commitData();
      }
    } else {
       
      table->setBeginTime(storeTime);
      StString fsql;
      StDbFastSqlWriter writer(fsql);
      writer.ioTable(table);
      Db<<fsql.str();
      if(Db.InputEnd()){
	retVal=1;
        table->commitData();
      }
    }
     
  }

  clear();

  table->setRowNumber();
  delete [] colList;
  delete [] sTime;

  delete [] dataTable;

  if(!retVal)sendMess(" Write failed for Non-Indexed table=",table->printName(),dbMWarn,__LINE__,__CLASS__,__METHOD__);
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

    Db<<"SELECT  `schema`.schemaID, `schema`.name, `schema`.type, `schema`.length, ";
    Db<<"`schema`.position from `schema` WHERE `schema`.structID="<<structID;
    Db<<" AND `schema`.schemaID="<<requestSchemaID;
    Db<<" ORDER by `schema`.position"<<endsql;

    sendMess(DbQInfo,Db.printQuery(),dbMDebug,__LINE__,__CLASS__,__METHOD__);

    if(!Db.QueryStatus())
       return sendMess(DbQFailed,Db.printQuery(),dbMWarn,__LINE__,__CLASS__,__METHOD__);


    if(Db.NbRows()==0) {
      deleteDescriptor(structID,requestSchemaID);
      return 0;
    }

    while(Db.Output(&buff)){
      descriptor->fillElement(&buff,requestSchemaID);
      buff.Raz();
    }
    Db.Release();
    descriptor->endRowPadding();

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
  if(numElements<=0) return retElements;
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
StDbSql::deleteDescriptors() {
  for( auto &it : mdescriptors ) delete it;
  mdescriptors.clear();
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
  tmpString = 0;

  if(!buff.ReadScalar(tmpString,"name"))return false;
  node->setName(tmpString);
  delete [] tmpString;
  tmpString = 0;

  if(!buff.ReadScalar(tmpString,"versionKey"))return false;
  node->setVersion(tmpString);
  delete [] tmpString;
  tmpString = 0;

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
    
    tmpString=0;
    char* hy=0;
    if(buff.ReadScalar(tmpString,"elementID")){
      hy=strstr(tmpString,"-");
      if(hy){
	*hy='\0';
	hy++;
        int first=atoi(tmpString);
        int last = atoi(hy);
        hy--;
        *hy='-';
        int len = last-first+1;
        int * tmpElements = new int[len];
        int j=0;
        int k;
        for(k=first; k<=last;k++){
	  tmpElements[j]=k;
          j++;
	} 
        table->setElementID(tmpElements,len);
        delete [] tmpElements;
        tmpElements = 0;
      } else {
       delete [] tmpString;
	   tmpString = 0;
       if(!buff.ReadScalar(tmpString,"indexName")) return false;
       table->setElementName(tmpString);
      }
      delete [] tmpString;
      tmpString = 0;
    }


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

  StString dqs;
  dqs<<"insert into Nodes set name='"<<node->printName()<<"' ";
  if(!StDbDefaults::Instance()->IsDefaultVersion(node->printVersion()))
     dqs<<", versionKey='"<<node->printVersion()<<"' ";

  dqs<<", nodeType='"<<node->printNodeType()<<"'";

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
	if ( ename ) { 
	    Db<<", indexName='"<<ename<<"'";
    	Db<<", indexVal="<<eid;
    	delete [] ename;
	}
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
  clear();
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

  clear();
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

 StString fs;
 fs<<" flavor In(";
 while(( (id2)=strstr(id3,"+")) ){
    *id2='\0';
    fs<<"'"<<id3<<"',";
    *id2='+';
    id2++;
    id3=id2;
 }
 fs<<"'"<<id3<<"')";
 delete [] id1;

 return mRetString(fs);
}

///////////////////////////////////////////////
char*
StDbSql::getProdTimeQuery(unsigned int prodTime){

// prepares SQL of " entryTime<="
StString pt;
  if(prodTime==0){
    pt<<" deactive=0 ";
  } else {
    pt<<" (deactive=0 OR deactive>="<<prodTime<<")";
    pt<<" AND unix_timestamp(entryTime)<="<<prodTime;
  } 
  return mRetString(pt);
}

///////////////////////////////////////////////
char* 
StDbSql::getElementList(int* e, int num){

// prepares comma separated list of integers
// using the between operator

 StString es;

 if (!e || num == 0) {                                                                                                                                       
   es<<0;                                                                                                                                                    
 } else if (num == 1) {                                                                                                                                      
    es << " = " << e[0];                                                                                                                                      
 } else {                                                                                                                                                    
    // check for ordered list first, if not ordered - use "IN", not "BETWEEN"                                                                                
    bool ordered = true;                                                                                                                                     
    for (int i = 0; i < num; i++) {                                                                                                                          
        if (e[i] != (e[0]+i)) {                                                                                                                              
            ordered = false;                                                                                                                                 
            break;                                                                                                                                           
        }                                                                                                                                                    
    }                                                                                                                                                        
    if (ordered == true) {                                                                                                                                   
        es<<" BETWEEN "<<e[0]<<" AND "<< (e[0] + num - 1);                                                                                                    
    } else {                                                                                                                                                 
        // list is not ordered, probably, our input is some mixed element IDs, use "IN"                                                                      
        return getElementListIN(e,num);                                                                                                                      
    }                                                                                                                                                        
 } 

 return mRetString(es);
}
////////////////////////////////////////////////
char*
StDbSql::getElementListIN(int* e, int num){
// prepares comma separated list of integers
// using the IN operator


 StString es;
 if(!e){ 
   es<<0;
 } else {
   es<<"In (";
   for(int i=0;i<num-1;i++) es<<e[i]<<",";  
   es<<e[num-1];
 }
 es<<")";
 return mRetString(es);
}

////////////////////////////////////////////////
char*
StDbSql::getColumnList(StDbTable* table,char* tableName,char* funcName){

  StTableDescriptorI* desc=table->getDescriptor();
  int numElements=desc->getNumElements();

  StString es; es<<" ";

  int icount=0;
  for(int i=0;i<numElements;i++){
     if(funcName && (desc->getElementLength(i)>1))continue;
     char* name=desc->getElementName(i);
     if(funcName)es<<funcName<<"(";
     if(tableName)es<<tableName<<".";
     es<<name;
     if(funcName)es<<") as "<<name;
     if(i<(numElements-1))es<<", ";
     delete [] name;
     icount++;
  }

 if(icount) return mRetString(es);
 return (char*) 0;
}
  
//////////////////////////////////////////////////
char*
StDbSql::getEmptyString(){

  StString es; es<<" ";
  return mRetString(es);

};

////////////////////////////////////////////////
char*
StDbSql::checkTablePrepForQuery(StDbTable* table, bool checkIndexed){

  if(mretString){ 
     delete [] mretString; 
     mretString=new char[256];
  }else{mretString=new char[256];}

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
   Db<<" where "<<getFlavorQuery(table->printFlavor());
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
