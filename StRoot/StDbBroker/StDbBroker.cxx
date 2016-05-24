/***************************************************************************
 *
 * $Id: StDbBroker.cxx,v 1.62 2016/05/24 17:44:16 dmitry Exp $
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
 * Revision 1.62  2016/05/24 17:44:16  dmitry
 * first batch of fixes for Coverity findings
 *
 * Revision 1.61  2015/05/21 21:51:34  dmitry
 * avoid memleak by moving check to the entrance of the func
 *
 * Revision 1.60  2015/05/21 18:29:06  dmitry
 * small memory leak and type conversion warnings fixed
 *
 * Revision 1.59  2014/07/28 14:58:27  dmitry
 * fixed templated call to make it compliant with gcc 4.8.2
 *
 * Revision 1.58  2011/11/28 17:03:07  dmitry
 * dbv override support in StDbLib,StDbBroker,St_db_Maker
 *
 * Revision 1.57  2011/02/10 17:31:01  dmitry
 * added an option to blacklist domains
 *
 * Revision 1.56  2009/11/10 20:24:00  fisyak
 * Use SafeDelete
 *
 * Revision 1.55  2008/01/17 20:55:00  deph
 * Removed annoying repetative printing of Statistics
 *
 * Revision 1.54  2008/01/15 20:37:44  deph
 * Removed DbFill and corresponding calls from StDbBroker
 *
 * Revision 1.53  2007/12/11 20:31:08  deph
 * moved print statistics call to close db function so only prints upon a real close
 *
 * Revision 1.52  2007/10/10 15:29:10  deph
 * Removed debug statements
 *
 * Revision 1.51  2007/09/10 02:36:08  perev
 * StDbBroker::Release added
 *
 * Revision 1.50  2007/05/16 22:47:54  deph
 * Replaced cerr with LOG_ERROR <<endm; for logger
 *
 * Revision 1.49  2006/01/13 21:09:41  deph
 * Fixed minor memory leak
 *
 * Revision 1.48  2006/01/13 20:44:40  deph
 * Fixed small memory leak
 *
 * Revision 1.47  2005/12/19 15:47:37  deph
 * Grabbing rowsize from St_db_maker and passing it on to StDbTableDescriptor (ssdPadding)
 *
 * Revision 1.46  2005/12/06 21:35:43  deph
 * Cleaned up to remove warnings
 *
 * Revision 1.45  2004/07/14 18:46:51  perev
 * UInt=>Int for new ROOT
 *
 * Revision 1.44  2004/01/14 23:13:08  fisyak
 * Replace ostringstream => StString
 *
 * Revision 1.43  2003/09/15 19:16:39  porter
 * added #include <sstream> to fix rh7.2 compile problem introduced with recent fix
 * for rh8 compilation.
 *
 * Revision 1.42  2003/09/12 01:48:06  porter
 * removed all strstream objects in favor of stringstream+string directly
 *
 * Revision 1.41  2003/09/02 17:55:35  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.40  2003/01/08 19:43:10  perev
 * CleanUp
 *
 * Revision 1.39  2002/02/25 19:21:51  porter
 * removed <<ends from  prodTime protection on run queries
 *
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
#include <StString.h>
#include <Stiostream.h>
#include <stdlib.h> 

#include "TString.h"
#include "StMessMgr.h"

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
  char **ElementComment = new char*[m_nElements]; 

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
  m_node = 0;
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
  SafeDelete(m_Nodes);
  SafeDelete(m_Tree);
  SafeDelete(mgr);
}

//_____________________________________________________________________________
void StDbBroker::printStatistics(){
  if(m_Tree)m_Tree->printNumberStats();
  if(mgr) mgr->printTimeStats();
}  
//_____________________________________________________________________________

void StDbBroker::CloseAllConnections(){
  if(mgr){
           mgr->closeAllConnections();
//           StDbBroker::printStatistics();
         }
//   cout<<"MPD:GOT HERE connection to db closed"<<endl;
};

//_____________________________________________________________________________

void StDbBroker::Release(){
//a general close (cut loose function) for use by
//st_db_maker (Victor did not want to call CloseAllConnections to 
//mainain abstraction) Also could add other utilities here later on.
   StDbBroker::CloseAllConnections();
//   cout<<"MPD:connection to db closed"<<endl;
}

//_____________________________________________________________________________
//void StDbBroker::Fill(void * pArray, const char **Comments)
//{
//  if ( m_nElements==0 ) return;
  //char **Comments = new char*[m_nElements]; 
  //TString Comment;

//  UInt_t i;
//  for (i=0;i<m_nElements;i++) {
    
//    if(m_descriptor[i].fDimensions>1)
//      {
//	LOG_ERROR<<"dim>1, can't handle yet"<<endm;
//	return;
//      }
    
//    m_descriptor[i].fColumnName[31]='\0';
//  }
  
//  Int_t  date, time;
  //VP TDatime::GetDateTime(m_DateTime, date, time);
//  date = m_DateTime[0]; time = m_DateTime[1];
//  uint datetime[4]={0,0,0,0};
//  datetime[0]=date;
//  datetime[1]=time;
  
//  ::DbFill(datetime, (const char*) m_tableName, (const char*) m_structName, m_nElements,m_descriptor,Comments,m_nRows,m_sizeOfStruct,pArray);
  
//  delete [] Comments;
//}  

//_____________________________________________________________________________
const char *StDbBroker::GetFlavor()
{
  return (m_node)? m_node->getFlavor():0;
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
     StString os;
     const unsigned int* index = mdescriptor->IndexArray(i);
     for(int k=0; k<(int)mdescriptor->Dimensions(i)-1;k++) 
       os<<index[k]<<",";
     os<<index[mdescriptor->Dimensions(i)-1];
     const char* lengthString = (os.str()).c_str(); 
     buff.WriteScalar(lengthString,"length");
     //     os.freeze(0);
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

void 
StDbBroker::addBlacklistedDomain(const char* domainName) {
	mgr->blacklistDbDomain(domainName);
}

//_____________________________________________________________________________
void
StDbBroker::SetDateTime(Int_t  date, Int_t  time)
{
  // 20000127  002502
   m_DateTime[0] = date; 
   m_DateTime[1]= time;

   StString ds;
   StString ts;

   ts<<m_DateTime[1];
   int len = (ts.str()).length(); 
   ds<<m_DateTime[0];
   for(int i=0;i<6-len;i++)ds<<"0";
   ds<<m_DateTime[1];

   const char* dateTime = (ds.str()).c_str();
   mgr->setRequestTime(dateTime);
}

//____________________________________________________________________________
void StDbBroker::SetProdTime(UInt_t ptime){
  if(m_Tree)m_Tree->setProdTime(ptime);
  m_prodTime = ptime;
}

//____________________________________________________________________________
void StDbBroker::AddProdTimeOverride(UInt_t ptime, char* dbType, char* dbDomain) {
  if (m_Tree) {
    m_Tree->setProdTimeOverride(ptime, dbType, dbDomain);
    //std::cout << "SPECIAL: StDbBroker - setting override " << dbType << " / " << dbDomain << " = " << ptime << "\n";
  } else {
    //std::cout << "SPECIAL: StDbBroker - override was not set, no m_Tree! \n";
  }
  m_prodTimeOverride.insert( std::make_pair( std::make_pair(dbType,dbDomain), ptime ) );
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
  
  //Store the the TTABLE padded size
    StDbTableDescriptor* TD = new StDbTableDescriptor();
    TD->storeRowSize(m_sizeOfStruct);
    delete TD;

  StDbNode* anode = m_Nodes->getNode(tabID);
  m_node=dynamic_cast<StDbTable*>(anode);

  if(!m_node) return pData;
  if(!m_node->hasDescriptor())m_node->setDescriptor(GetTableDescriptor());

  // I would do this in a separate function but this would require
  // redoing it all with

  bool fetchStatus;
  if(m_node->getDbType()==dbRunLog && 
     m_node->getDbDomain() != dbStar && 
     m_runNumber>1000000 ){
     fetchStatus=UseRunLog(m_node);   
  } else {
    fetchStatus=mgr->fetchDbTable(m_node);
  }

  // success or failure yields an endtime ... so get it.

  if(fetchStatus){  

    char* thisTime;
    m_endTimeStamp = m_node->getEndTime();
    thisTime = m_node->getEndDateTime();
    makeDateTime(thisTime,m_EndDate,m_EndTime);
    m_nRows= m_node->GetNRows();
    pData  = m_node->GetTableCpy(); // gives the "malloc'd version"

    m_beginTimeStamp = m_node->getBeginTime();
    thisTime = m_node->getBeginDateTime();
    makeDateTime(thisTime,m_BeginDate,m_BeginTime);

  } else {
    SetZombie(true);
  }

return pData;
}

//_____________________________________________________________________________
void StDbBroker::makeDateTime(const char* dateTime,Int_t & iDate,Int_t & iTime){

    char* tmp1 = new char[strlen(dateTime)+1];
    char* tmp2 = new char[strlen(dateTime)+1];
    strcpy(tmp1,dateTime);
    strcpy(tmp2,tmp1); tmp1[8]='\0';tmp2+=8;

    iDate = atoi(tmp1);
    iTime = atoi(tmp2);
    delete [] tmp1; tmp2-=8; delete [] tmp2;

}

//_____________________________________________________________________________
bool StDbBroker::UseRunLog(StDbTable* table){

  unsigned int prodTime=table->getProdTime();    
    StString rq;
    rq<<" where runNumber="<<m_runNumber;

    if(prodTime==0){
      rq<<" AND deactive=0 ";
    } else {
      rq<<" AND (deactive=0 OR deactive>="<<prodTime<<")";
      rq<<" AND unix_timestamp(entryTime)<="<<prodTime;
    } 

    bool fetchStatus=mgr->fetchDbTable(table,(char*)(rq.str()).c_str());


return fetchStatus;
}

//_____________________________________________________________________________
Int_t StDbBroker::WriteToDb(void* pArray, int tabID){
#define __METHOD__ "WriteToDb(pArray,tabID)"

  StString em;
  if(!pArray || tabID==0) {
    em<<" Write Failed -> either data-array or tableID is incomplete";
    return mgr->printInfo((em.str()).c_str(),dbMErr,__LINE__,__CLASS__,__METHOD__);
  }
  if(!m_Nodes){
    em<<"Write Failed -> incomplete table context. Try InitConfig() 1st";
    return mgr->printInfo((em.str()).c_str(),dbMErr,__LINE__,__CLASS__,__METHOD__);
  }
  StDbNode* anode= m_Nodes->getNode(tabID);
  StDbTable* table=dynamic_cast<StDbTable*>(anode);
  if(!table){
    em<<"Write Failed -> tableID="<<tabID<<" is not known ";
    return mgr->printInfo((em.str()).c_str(),dbMErr,__LINE__,__CLASS__,__METHOD__);
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
  StString em;

  if(!pArray || !fullPath) {
    em<<" Write Failed:: either data-array or path is incomplete";
    return mgr->printInfo((em.str()).c_str(),dbMErr,__LINE__,__CLASS__,__METHOD__);
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
   char* dbName =0;
   StDbTable* table = 0;
   for(int i=icount;i>0;i--){
     if(i==icount){
       dbName=aword[i];
     } else {
       sprintf(tmpName,"%s_%s",aword[i-1],aword[i]);
       dbName=(char*)tmpName;
     }
     table=findTable(dbName);
     if(table)break;
   }

   if (!table){
     em<<"Write Failed table="<<m_tableName<<" not found in db="<<dbName;
     delete [] path;
     delete [] aword;
     return mgr->printInfo((em.str()).c_str(),dbMErr,__LINE__,__CLASS__,__METHOD__);
   }

   table->setDescriptor(GetTableDescriptor());
   table->SetTable((char*)pArray,m_nRows,idList);
   mgr->setStoreTime(mgr->getUnixCheckTime());
   bool iswritten=mgr->storeDbTable(table);
   delete table;

   delete [] aword;
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
  Int_t  date, time;
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
      SetBeginDate(datetime[0]);
      SetBeginTime(datetime[1]);
      SetEndDate  (datetime[2]);
      SetEndTime  (datetime[3]);
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

 if(m_prodTime!=0) { 
	m_Tree->setProdTime(m_prodTime); 
    for (std::map<std::pair<char*,char*>, unsigned int>::iterator it = m_prodTimeOverride.begin(); it != m_prodTimeOverride.end(); it++ ) {
      //if ( (*it).first.first ) {
        //std::cout << "SPECIAL: StDbBroker - Setting override to m_Tree : " << (*it).first.first << " _ " << (*it).first.second << " = " << (*it).second << "\n";
      //} else {
        //std::cout << "SPECIAL: StDbBroker - Setting override to m_Tree : [all domains] _ " << (*it).first.second << " = " << (*it).second << "\n";
      //}
      m_Tree->setProdTimeOverride((*it).second, (*it).first.first, (*it).first.second);
    }
 }
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
      while( !itr->done() ) { m_Nodes->addNode(itr->next(),pID); }
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
