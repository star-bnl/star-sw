/***************************************************************************
 *
 * $Id: StDbBroker.cxx,v 1.38 2002/02/25 17:52:10 porter Exp $
 *
 * Author: S. Vanyashin, V. Perevoztchikov
 * Updated by:  R. Jeff Porter
 ***************************************************************************
 *
 * Description: Offline Interface from the Offline Maker interface to the
 *              Database interface. 
 *
 ***************************************************************************
 *
 * $Log: StDbBroker.cxx,v $
 * Revision 1.38  2002/02/25 17:52:10  porter
 * prodTime check for run level queries
 *
 * Revision 1.37  2002/02/22 22:17:43  porter
 * run test added on runs>=year1
 *
 * Revision 1.36  2002/02/20 04:01:46  porter
 * changed test on runNumber from !=0 to >0 for initiating query by runNumber
 *
 * Revision 1.35  2002/01/15 17:15:36  porter
 * moved timestamp translation to a separate method
 *
 * Revision 1.34  2001/12/21 04:55:31  porter
 * changed some ostrstream usage for insure tests
 *
 * Revision 1.33  2001/10/30 21:59:32  porter
 * same timestamp fix for runlevel query... but works..
 *
 * Revision 1.32  2001/10/30 20:43:29  porter
 * timestamp set for failure on query by runNumber
 *
 * Revision 1.31  2001/10/26 21:40:03  porter
 * added protection for query by runNumber until Victor can implement his side
 *
 * Revision 1.30  2001/10/26 15:44:02  porter
 * add query by runNumber
 *
 * Revision 1.29  2001/10/24 04:05:56  porter
 * added zombie designation per Victor's suggestion
 *
 * Revision 1.28  2001/09/13 16:54:54  porter
 * propogate falvor by table through the brokery
 *
 * Revision 1.27  2001/02/09 23:07:16  porter
 * replaced several ostrstream into buffer for root+solarisCC5 iostream
 * with ostrstream creating the buffer
 *
 * Revision 1.26  2001/01/22 18:40:24  porter
 * Added a wrapper for StMessage so one can use it in StDbLib
 *
 * Revision 1.25  2000/11/03 18:57:53  porter
 * modified sanity check from "IsNode()" to the results of a dynamic_cast
 * this check prevents mistaking a directory for a table
 *
 * Revision 1.24  2000/08/15 22:53:14  porter
 * Added 2 write methods.
 *  - 1 works once "list" is requested from database
 *  - 1 works just by specifying the full path from which
 *    the code extracts the database name.
 *
 * Revision 1.23  2000/06/30 02:00:42  porter
 * fixed memory leak introduced when making sure top level returned to
 * offline is always a database type name
 *
 * Revision 1.22  2000/06/14 13:39:05  didenko
 * Add ClassDef/ClassImp
 *
 * Revision 1.21  2000/06/05 22:13:53  vanyashi
 * added const needed for tableDescriptor
 *
 * Revision 1.20  2000/04/25 18:27:48  porter
 * Added flavor and production time as query fields to pass to db-api
 *
 * Revision 1.19  2000/04/14 14:46:41  fine
 * new method for Victor has been introduced
 *
 * Revision 1.18  2000/04/13 20:22:57  porter
 * - reconnected tableDescriptor that had been broken via St_tableDescriptor.
 * - added unix timestamp as standard
 * - top node returned via InitConfig will be a database type
 *
 * Revision 1.17  2000/04/04 14:04:07  perev
 * table descriptor modif
 *
 * Revision 1.16  2000/03/26 16:47:13  fine
 * Adjusted to ROOT 2.24
 *
 * Revision 1.15  2000/03/04 18:56:20  porter
 * fixed return of endtime, it was returning beginTime for both begin & end
 *
 * Revision 1.14  2000/02/28 15:24:19  porter
 * add more StDbLib methods to broker: this time, StDbManager::closeAllConnections()
 *
 * Revision 1.13  2000/02/14 23:36:36  porter
 * fixed unsigned int <-> int comparison & timestamp string
 *
 * Revision 1.12  2000/01/31 17:11:18  porter
 * fix break caused by the interaction design between
 * 'StRoot/St_base/tableDescriptor.h' & 'StDbBroker::Descriptor'
 * Now  StDbBroker::Descriptor==tableDescriptor_st
 * And  StDbBroker::GetTableDescriptor() returns abstract StTableDescriptorI*
 * Interface to StDbLib is (and was) handle correctly.
 * StDbBroker is now tied to StRoot/St_base via tableDescriptor.h
 * No problems would have occured if St_base interactions were based
 * on StTableDesciptorI in the first place.
 *
 * Revision 1.11  2000/01/27 20:30:40  porter
 * cleaned up dtor & error logic
 *
 * Revision 1.10  2000/01/27 05:56:03  porter
 * update for compiling on CC5+HPUX-aCC+KCC
 *
 * Revision 1.9  2000/01/24 15:12:19  porter
 * verbose check before resetting StDbManager verbose setting
 * + timestamp check from St_db_Maker where time and date are
 * separate integers.
 *
 * Revision 1.8  2000/01/19 20:21:48  porter
 * change of TableIter to StDbTableIter
 *
 * Revision 1.7  2000/01/14 14:49:10  porter
 * set verbose level for checking, added $Id & $Logs, & made node container
 * more robust for interactions with StDbLib
 *
 * Revision 1.6  2000/01/10 20:31:16  porter
 * modified StDbBroker to be an interface to the DB-interface, StDbLib.
 *  - old functionality is retained for the short-term & modifications
 *    are extensions
 *
 *
 **************************************************************************/
#include <strstream.h>
#include <iostream.h>
#include <iomanip.h>
#include <stdlib.h> 

#include "TString.h"

// needed for GetComments
#include "TROOT.h"
#include "TBuffer.h"
#include "TClass.h"
#include "St_Table.h"
#include "TRealData.h"
#include "TDataMember.h"
#include "TDataType.h"

#include "Api.h"
#include "StDbBroker.h"
#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbConfigNode.hh"
#include "StDbLib/StDbTable.h"
#include "dbNodes.h"
#include "StDbLib/StDbTableIter.hh"
#include "StDbLib/StDbBuffer.h"  // for inputting the descriptor
#include "StDbLib/StDbTableDescriptor.h" 
#include "StDbWrappedMessenger.hh"

#define __CLASS__ "StDbBroker"

//
ClassImp(StDbBroker)
//______________________________________________________________________________
//the only remaining St_Table dependence is in this function 
char **StDbBroker::GetComments(St_Table *parentTable)
{
  char **ElementComment = new char*[m_nElements]; 
  if (!parentTable) {
    //    MakeZombie();
    return NULL;
  }
  
  TClass *classPtr = parentTable->GetRowClass();
  if (!classPtr) return NULL;
  
  if (!classPtr->GetListOfRealData()) classPtr->BuildRealData();
  
  TIter next(classPtr->GetListOfDataMembers());
  TDataMember *member = 0;
  UInt_t i=0, j=0;
  while ( (member = (TDataMember *) next()) ) {
    ElementComment[i] = strdup(member->GetTitle());
 // strip trailing blanks from Comments (they are stripped in mysql anyway)
    j=strlen(ElementComment[i]);
    while (j != 0  && ElementComment[i][j-1] == ' ') j--;
    ElementComment[i][j]='\0';
    i++;
  }
  return ElementComment;
}

//_____________________________________________________________________________
StDbBroker::StDbBroker(): m_structName(0), m_tableName(0), m_requestTimeStamp(0), m_tableVersion(0), m_database(0), m_ParentType(0), m_isVerbose(0), m_Nodes(0), m_Tree(0), m_flavor(0), m_prodTime(0) {

  m_runNumber=0;
  mgr=StDbManager::Instance();
  StDbMessService* ms=new StDbWrappedMessenger();
  mgr->setMessenger(ms);

} 

//_____________________________________________________________________________
StDbBroker::~StDbBroker(){
  printStatistics();
  if(m_tableName) delete [] m_tableName;
  if(m_structName) delete [] m_structName;
  if(m_tableVersion) delete [] m_tableVersion;
  if(m_database) delete [] m_database;
  if(m_flavor) delete [] m_flavor;
  if(m_Nodes) delete m_Nodes;
  if(m_Tree) delete m_Tree;
  if(mgr) delete mgr;
}

//_____________________________________________________________________________
void StDbBroker::printStatistics(){
  if(m_Tree)m_Tree->printNumberStats();
  if(mgr) mgr->printTimeStats();
}  
//_____________________________________________________________________________

void StDbBroker::CloseAllConnections(){
  if(mgr)mgr->closeAllConnections();
};

//_____________________________________________________________________________
void StDbBroker::Fill(void * pArray, const char **Comments)
{
  if ( m_nElements==0 ) return;
  //char **Comments = new char*[m_nElements]; 
  //TString Comment;

  UInt_t i;
  for (i=0;i<m_nElements;i++) {
    
    if(m_descriptor[i].fDimensions>1)
      {
	cerr<<"dim>1, can't handle yet"<<endl;
	return;
      }
    
    m_descriptor[i].fColumnName[31]='\0';
  }
  
  UInt_t date, time;
  //VP TDatime::GetDateTime(m_DateTime, date, time);
  date = m_DateTime[0]; time = m_DateTime[1];
  uint datetime[4]={0,0,0,0};
  datetime[0]=date;
  datetime[1]=time;
  
  ::DbFill(datetime, (const char*) m_tableName, (const char*) m_structName, m_nElements,m_descriptor,Comments,m_nRows,m_sizeOfStruct,pArray);
  
  delete [] Comments;
}  

//_____________________________________________________________________________
StTableDescriptorI*
StDbBroker::GetTableDescriptor(){

StDbBuffer buff;
StDbTableDescriptor* descriptor = new StDbTableDescriptor();
unsigned int numElements = mdescriptor->NumberOfColumns();

 for(int i=0;i<(int)numElements;i++){

   buff.WriteScalar(mdescriptor->ColumnName(i),"name");

   // array designation & lengths 
   if(!mdescriptor->Dimensions(i)){
      buff.WriteScalar("1","length");
   } else {
     ostrstream os;
     const unsigned int* index = mdescriptor->IndexArray(i);
     for(int k=0; k<(int)mdescriptor->Dimensions(i)-1;k++) 
       os<<index[k]<<",";
     os<<index[mdescriptor->Dimensions(i)-1]<<ends;
     char* lengthString = os.str(); 
     buff.WriteScalar(lengthString,"length");
     os.freeze(0);
     //     delete [] lengthString;
   }

   // position in struct
   buff.WriteScalar(i+1,"position");

   // Type identification
  switch ((EColumnType)mdescriptor->ColumnType(i)) {
  case kFloat:
    {
      buff.WriteScalar("float","type");
      break;
    }
  case kInt:
    {
      buff.WriteScalar("int","type");
      break;
    }
  case kLong:
    {
      buff.WriteScalar("long","type");
      break;
    }
  case kShort:
    {
      buff.WriteScalar("short","type");
      break;
    }
  case kDouble:
    {
      buff.WriteScalar("double","type");
     break;
    }
  case kUInt:
    {
      buff.WriteScalar("uint","type");
      break;
    }
  case kULong:
    {
      buff.WriteScalar("ulong","type");
      break;
    }
  case kUShort:
    {
      buff.WriteScalar("ushort","type");
      break;
    }
  case kUChar:
    {
      buff.WriteScalar("uchar","type");
      break;
    }
  case kChar:
    {
      buff.WriteScalar("char","type");
      break;
    }
  default:
    {
      break;
    }
  }

  descriptor->fillElement(&buff,0); // 0 means use this ....  don't check 
                                    // about internal schemaID's
  buff.Raz();

 }

return descriptor;
}

//_____________________________________________________________________________
void
StDbBroker::SetDateTime(UInt_t date, UInt_t time)
{
  // 20000127  002502
   m_DateTime[0] = date; 
   m_DateTime[1]= time;
   //   cout<<" Date ="<<date << " & Time = " << time<<endl;

   //   char dateTime[16];
   //   ostrstream ds(dateTime,16);
   //   char timeCheck[7];
   //   ostrstream ts(timeCheck,7);
   char* dateTime;
   ostrstream ds;
   char* timeCheck;
   ostrstream ts;

   ts<<m_DateTime[1]<<ends;
   timeCheck = ts.str();
   int len = strlen(timeCheck);
   ds<<m_DateTime[0];
   for(int i=0;i<6-len;i++)ds<<"0";
   ds<<m_DateTime[1]<<ends;

   dateTime = ds.str();
   mgr->setRequestTime(dateTime);
   ds.freeze(0);
   ts.freeze(0);
   //   delete [] timeCheck;
   //   delete [] dateTime;
}

//____________________________________________________________________________
void StDbBroker::SetProdTime(UInt_t ptime){
  if(m_Tree)m_Tree->setProdTime(ptime);
  m_prodTime = ptime;
}

//____________________________________________________________________________
void StDbBroker::SetFlavor(const char* flavor){

  if(!flavor)return;
  m_flavor = new char[strlen(flavor)+1];
  strcpy(m_flavor,flavor);
  if(m_Tree)m_Tree->setFlavor(m_flavor);

}

//____________________________________________________________________________
void StDbBroker::SetTableFlavor(const char* flavor, int tabID, int parID)
{
  StDbNode* anode = m_Nodes->getNode(tabID);
  StDbTable* node=dynamic_cast<StDbTable*>(anode);

  if(!node) return;
  node->setFlavor(flavor);
}


//____________________________________________________________________________
void * StDbBroker::Use(int tabID, int parID)
{

  // This is an "Offline" requirement of only 31 char per element name 
  // UInt_t i;
  //  for (i=0;i<m_nElements;i++) {
  //      m_descriptor[i].fColumnName[31]='\0';
  //  }
  void* pData = 0;
  m_nRows = 0;
  SetNRows(0);
  SetBeginDate(19950101);
  SetBeginTimeStamp(788918400);
  SetBeginTime(0);
  SetEndDate(20380101);
  SetEndTimeStamp(2145916799);
  SetEndTime(0);
  SetZombie(false);

  StDbNode* anode = m_Nodes->getNode(tabID);
  StDbTable* node=dynamic_cast<StDbTable*>(anode);

  if(!node) return pData;
  if(!node->hasDescriptor())node->setDescriptor(GetTableDescriptor());

  // I would do this in a separate function but this would require
  // redoing it all with

  bool fetchStatus;
  if(node->getDbType()==dbRunLog && 
     node->getDbDomain() != dbStar && 
     m_runNumber>1000000 ){
     fetchStatus=UseRunLog(node);   
  } else {
    fetchStatus=mgr->fetchDbTable(node);
  }

  // success or failure yields an endtime ... so get it.
  char* thisTime;
  m_endTimeStamp = node->getEndTime();
  thisTime = node->getEndDateTime();
  makeDateTime(thisTime,m_EndDate,m_EndTime);

  if(fetchStatus){  

    m_nRows= node->GetNRows();
    pData  = node->GetTableCpy(); // gives the "malloc'd version"

    m_beginTimeStamp = node->getBeginTime();
    thisTime = node->getBeginDateTime();
    makeDateTime(thisTime,m_BeginDate,m_BeginTime);

  } else {
    SetZombie(true);
  }

return pData;
}

//_____________________________________________________________________________
void StDbBroker::makeDateTime(const char* dateTime,UInt_t& iDate,UInt_t& iTime){

    char* tmp1 = new char[strlen(dateTime)+1];
    char* tmp2 = new char[strlen(dateTime)+1];
    strcpy(tmp1,dateTime);
    strcpy(tmp2,tmp1); tmp1[8]='\0';tmp2+=8;

    iDate = (UInt_t)atoi(tmp1);
    iTime = (UInt_t)atoi(tmp2);
    delete [] tmp1; tmp2-=8; delete [] tmp2;

}

//_____________________________________________________________________________
bool StDbBroker::UseRunLog(StDbTable* table){

  unsigned int prodTime=table->getProdTime();    
    ostrstream rq;
    rq<<" where runNumber="<<m_runNumber<<ends;

    if(prodTime==0){
      rq<<"AND deactive=0 "<<ends;
    } else {
      rq<<"AND (deactive=0 OR deactive>="<<prodTime<<")";
      rq<<" AND unix_timestamp(entryTime)<="<<prodTime<<ends;
    } 

    bool fetchStatus=mgr->fetchDbTable(table,rq.str());
    rq.freeze(0);

return fetchStatus;
}

//_____________________________________________________________________________
Int_t StDbBroker::WriteToDb(void* pArray, int tabID){
#define __METHOD__ "WriteToDb(pArray,tabID)"

  char errMessage[256];
  ostrstream em(errMessage,256);
  if(!pArray || tabID==0) {
    em<<" Write Failed -> either data-array or tableID is incomplete"<<ends;
    return mgr->printInfo(errMessage,dbMErr,__LINE__,__CLASS__,__METHOD__);
  }
  if(!m_Nodes){
    em<<"Write Failed -> incomplete table context. Try InitConfig() 1st"<<ends;
    return mgr->printInfo(errMessage,dbMErr,__LINE__,__CLASS__,__METHOD__);
  }
  StDbNode* anode= m_Nodes->getNode(tabID);
  StDbTable* table=dynamic_cast<StDbTable*>(anode);
  if(!table){
    em<<"Write Failed -> tableID="<<tabID<<" is not known " <<ends;
    return mgr->printInfo(errMessage,dbMErr,__LINE__,__CLASS__,__METHOD__);
  }

  if(!table->hasDescriptor())table->setDescriptor(GetTableDescriptor());
  table->SetTable((char*)pArray,m_nRows);

  // WARNING :: A Cludge -> StDbManager has separate 
  //'store' & 'request' times whilr StDbBroker does not
  mgr->setStoreTime(mgr->getUnixCheckTime());
  if(!mgr->storeDbTable(table))return 0;
  return 1;
#undef __METHOD__
}

//_____________________________________________________________________________
Int_t StDbBroker::WriteToDb(void* pArray, const char* fullPath, int* idList){
#define __METHOD__ "WriteToDb(pArray,fullPath,idList)"
  char errMessage[256];
  ostrstream em(errMessage,256);

  if(!pArray || !fullPath) {
    em<<" Write Failed:: either data-array or path is incomplete"<<ends;
    return mgr->printInfo(errMessage,dbMErr,__LINE__,__CLASS__,__METHOD__);
  }

  char* path=new char[strlen(fullPath)+1]; 
  strcpy(path,fullPath);

  // chop off a trailing "/" if found
  char* tmp=path;
  tmp+=strlen(path)-1;
  if(*tmp=='/')*tmp='\0';

  // Algorithm is if path = "A/B/C/D", we try to connect to 
  // databases "D", "C_D" (& "C") , "B_C" ( & "B"), "A_B" (& "A") 
  // quiting when table is found.  Note, by design if database
  // "C_D" does not exist, the DB-API tries to find database "C".

  char* a1=path;
  char* a2;
  char** aword = new char*[20];
  
  int icount=0;
  aword[icount]=a1;
  while((a2=strstr(a1,"/"))){
   *a2='\0'; a2++;
   icount++;
   aword[icount]=a2;
   a1=a2;
  }

   char tmpName[128];
   char* dbName;
   StDbTable* table;
   for(int i=icount;i>0;i--){
     if(i==icount){
       dbName=aword[i];
     } else {
       tmpName[0]='\0';
       ostrstream dbn(tmpName,128);
       dbn<<aword[i-1]<<"_"<<aword[i]<<ends;
       dbName=(char*)tmpName;
     }
     table=findTable(dbName);
     if(table)break;
   }

   if(!table){
     em<<"Write Failed table="<<m_tableName<<" not found in db="<<dbName<<ends;
     delete [] path;
     return mgr->printInfo(errMessage,dbMErr,__LINE__,__CLASS__,__METHOD__);
   }

   table->setDescriptor(GetTableDescriptor());
   table->SetTable((char*)pArray,m_nRows,idList);
   mgr->setStoreTime(mgr->getUnixCheckTime());
   bool iswritten=mgr->storeDbTable(table);
   delete table;

   return (iswritten) ? 1 : 0;
#undef __METHOD__
}

//_____________________________________________________________________________
StDbTable*
StDbBroker::findTable(const char* databaseName){

  StDbTable* table=0;
  StDbConfigNode* node= mgr->initConfig(databaseName);
  StDbTable* tmp=node->addDbTable(m_tableName);  
  if(tmp)table=new StDbTable(*tmp);
  delete node;

  return table;
}
  
//_____________________________________________________________________________
void * StDbBroker::Use()
{
//convert event datetime to date and time
  UInt_t date, time;
//VP TDatime::GetDateTime(m_DateTime, date, time);
  date = m_DateTime[0]; time = m_DateTime[1];
  uint datetime[4]={0,0,0,0};
  datetime[0]=date;
  datetime[1]=time;
  uint nRows=0;

  
  UInt_t i;
  for (i=0;i<m_nElements;i++) {
      m_descriptor[i].fColumnName[31]='\0';
  }

  // Check if request is a "hierarchy" : if so send request to "params" DB
  char* id = strstr(m_tableName,"_hierarchy");
  if(id){
    char* tmpName = new char[strlen(m_tableName)+1];
    strcpy(tmpName,m_tableName);
    char* id2 = strstr(tmpName,"_hierarchy");
    *id2 = '\0';
    if(strcmp(tmpName,"Calib")==0){
      m_database = new char[strlen("Calibrations_tpc")+1];
      strcpy(m_database,"Calibrations_tpc");
    } else if(strstr(tmpName,"Geom")){
     m_database = new char[strlen("Geometry_tpc")+1];
     strcpy(m_database,"Geometry_tpc");
    } else if(strstr(tmpName,"RunParam")){
     m_database = new char[strlen("RunParams_tpc")+1];
     strcpy(m_database,"RunParams_tpc");
    } else {
     m_database = new char[strlen("params")+1];
     strcpy(m_database,"params");
    }
     *id2='_';
    delete [] tmpName;
  }

  void *pDbData;
  
  if(id || strcmp(m_database,"params")==0){
    
    cout << "Looking for Table "<<m_tableName<<" In Db= "<<m_database<<endl;

  pDbData = ::DbUse(&nRows, datetime, (const char*)m_tableName,(const char*)m_structName,m_nElements,m_sizeOfStruct,m_descriptor);

  } else { 

  pDbData = ::DbRead(&nRows, datetime,(const char*)m_tableName,(const char*)m_structName,m_nElements,m_sizeOfStruct,m_descriptor,(const char*)m_database, (const char*)m_tableVersion );

}

  if (pDbData==NULL)
    {
      SetNRows(0);
      SetBeginDate(19950101);
      SetBeginTime(0);
      SetEndDate(20380101);
      SetEndTime(0);
    }
  else
    {
      SetNRows((UInt_t)nRows);
      SetBeginDate((UInt_t)datetime[0]);
      SetBeginTime((UInt_t)datetime[1]);
      SetEndDate((UInt_t)datetime[2]);
      SetEndTime((UInt_t)datetime[3]);
    }

  return pDbData;
}
//_____________________________________________________________________________

dbConfig_st* 
StDbBroker::InitConfig(const char* configName, int& numRows, char* versionName)
{

  if(m_Nodes){ 
    delete m_Nodes;
    m_Nodes = 0;
  }

if(m_Tree) delete m_Tree;

 char* dbTypeName=0;
 char* dbDomainName=0;
if(m_ParentType) delete [] m_ParentType;
m_ParentType = 0;

 if(mgr->getDataBaseInfo(configName,dbTypeName,dbDomainName)){
   if(strcmp(dbDomainName,"Star")!=0){
      int tlen = strlen(dbTypeName);
      m_ParentType = new char[tlen+1];
      strcpy(m_ParentType,dbTypeName);
   }
 }

  delete [] dbTypeName;
  delete [] dbDomainName;

 if(m_isVerbose)mgr->setVerbose(true);
if(!versionName){
  m_Tree=mgr->initConfig(configName,"reconV0",1); // 1=don't get db-descriptors
}else{
  m_Tree=mgr->initConfig(configName,versionName,1);//1=don't get db-descriptors
}

 dbConfig_st* configTable = 0;
 if(!m_Tree) return configTable;

 if(m_prodTime!=0)m_Tree->setProdTime(m_prodTime); 
 if(m_flavor)m_Tree->setFlavor(m_flavor);

 if(m_isVerbose){
 cout << "****************************************************************"<<endl;
 cout << "***    Will Print the Tree "<<endl;
 bool verbCheck = mgr->IsVerbose();
 if(!verbCheck)mgr->setVerbose(true);
 m_Tree->printTree(0);
 if(!verbCheck)mgr->setVerbose(false);
 cout << "***    End Print the Tree "<<endl;
 cout << "****************************************************************"<<endl;
 };
numRows = 0;

if(!buildNodes(m_Tree,0)) return configTable;

//numRows = m_Nodes->getNumNodes()-1;
return buildConfig(numRows);
}

//_____________________________________________________________________________

int
StDbBroker::buildNodes(StDbConfigNode* parentNode, int pID){

if(!parentNode) return 0;
if(!m_Nodes) {
  m_Nodes=new dbNodes;
  m_Nodes->addNode(parentNode,0);
}

int cID;

// check for tables in this Node
   if( (parentNode->hasData()) ){   
      StDbTableIter* itr = parentNode->getStDbTableIter();
      while(!itr->done())cID=m_Nodes->addNode(itr->next(),pID);
      delete itr;
   }

   StDbConfigNode* next;

// check for children of this Node
   if((parentNode->hasChildren())){
     next=parentNode->getFirstChildNode(); 
     cID=m_Nodes->addNode(next,pID);
     if(!buildNodes(next, cID))return 0;
   }

// check for siblings of this Node
   int parID;
   if( (next=parentNode->getNextNode()) ){
      parID=m_Nodes->getParentID(pID);
      cID=m_Nodes->addNode(next,parID);
      if(!buildNodes(next, cID))return 0;
   }

return 1;       
}

//_____________________________________________________________________________
dbConfig_st*
StDbBroker::buildConfig(int& numRows){

dbConfig_st* cTab= 0;
m_Nodes->reset();
int numNodes = m_Nodes->getNumNodes();

StDbNode* node;
StDbNode* parent;
char* parName;
char* nodeName;
char* id;
unsigned int parsize=sizeof(cTab[0].parname)-1;
unsigned int tabsize=sizeof(cTab[0].tabname)-1;
unsigned int typsize=sizeof(cTab[0].tabtype)-1;
int parID;
int cRow;

    numRows = numNodes;

    cTab=(dbConfig_st*)calloc(numRows,sizeof(dbConfig_st));
    node  = m_Nodes->getNode(0);
    strncpy(cTab[0].tabname,node->printName(),tabsize); 
    cTab[0].tabname[tabsize]='\0';  
    strncpy(cTab[0].tabtype,".node",typsize); 
    cTab[0].tabtype[typsize]='\0';
    cTab[0].parID=cTab[0].tabID=0;
    cRow = 1;

 if(m_ParentType){
    strncpy(cTab[0].parname,m_ParentType,parsize); 
 } else {
    strncpy(cTab[0].parname,node->printName(),parsize); 
 }
    cTab[0].parname[parsize]='\0';  

 for(int i=1; i<numNodes;i++){

   node  = m_Nodes->getNode(i);
   parent= m_Nodes->getParent(i);
   parID   = m_Nodes->getParentID(i);

   nodeName = node->printName();
   parName  = parent->printName();

   strncpy(cTab[cRow].parname,parName,parsize); 
    cTab[cRow].parname[parsize]='\0';
   strncpy(cTab[cRow].tabname,nodeName,tabsize); 
    cTab[cRow].tabname[tabsize]='\0';

   id=cTab[cRow].tabtype; *id='.'; id++;
   if(node->IsTable()){
     strcpy(id,((StDbTable*)node)->printCstructName());
   } else {
     strcpy(id,"node");
   }          
   cTab[cRow].tabtype[typsize]='\0';

   cTab[cRow].tabID=i;
   cTab[cRow].parID=parID;
   cRow++;
 }

if(m_isVerbose){
   cout <<"****************************************************"<<endl;
   cout <<"********* Will print dbConfig table "<<endl;
 for(int k=0; k<numRows; k++) {
   cout << "row "<<k<<" name =" << cTab[k].tabname<< " tid= "<< cTab[k].tabID;
   cout << " type = "<<cTab[k].tabtype;
   cout << " parent =" << cTab[k].parname << " pid= " << cTab[k].parID<<endl;
 }   
   cout <<"********* End print dbConfig table "<<endl;
   cout <<"****************************************************"<<endl;
}

return cTab;
}

//_____________________________________________________________________________
int StDbBroker::DbInit(const char * dbName) {  return ::DbInit(dbName) ;}

#undef __CLASS__
