/***************************************************************************
 *
 * $Id: StDbBroker.cxx,v 1.10 2000/01/27 05:56:03 porter Exp $
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
#include <iostream.h>
#include <iomanip.h>
#include <stdlib.h> 
#include <strstream.h>

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
//
//ClassImp(StDbBroker)
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
StDbBroker::StDbBroker(): m_structName(0), m_tableName(0), m_tableVersion(0), m_database(0), m_isVerbose(0), m_Nodes(0), m_Tree(0) {

  mgr=StDbManager::Instance();

} 


//______________________________________________________________________________
// int StDbBroker::Init(const char *dbname)
// {
//   return DbInit(dbname);
// }
//______________________________________________________________________________

void StDbBroker::Fill(void * pArray, const char **Comments)
{
  if ( m_nElements==0 ) return;
  //char **Comments = new char*[m_nElements]; 
  //TString Comment;

  UInt_t i;
  for (i=0;i<m_nElements;i++) {
    
    if(m_descriptor[i].dimensions>1)
      {
	cerr<<"dim>1, can't handle yet"<<endl;
	return;
      }
    
    m_descriptor[i].name[31]='\0';
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
void
StDbBroker::SetDateTime(UInt_t date, UInt_t time)
{
  // 20000127  002502
   m_DateTime[0] = date; 
   m_DateTime[1]= time;

   char dateTime[16];
   ostrstream os(dateTime,16);
   char timeCheck[7];
   ostrstream ts(timeCheck,7);

   ts<<m_DateTime[1]<<ends;
   int len = strlen(timeCheck);
   os<<m_DateTime[0];
   for(int i=0;i<6-len;i++)os<<"0";
   os<<m_DateTime[1]<<ends;

   mgr->setRequestTime(dateTime);

}



//____________________________________________________________________________
void * StDbBroker::Use(int tabID, int parID)
{

  // This is an "Offline" requirement of only 31 char per element name 
  UInt_t i;
  for (i=0;i<m_nElements;i++) {
      m_descriptor[i].name[31]='\0';
  }

  void* pData = 0;

  StDbNode* anode = m_Nodes->getNode(tabID);
  if(anode && !anode->IsNode()){
    StDbTable* node=(StDbTable*)anode;
    if(!mgr->fetchDbTable(node))return pData;
    m_nRows= node->GetNRows();
    pData  = node->GetTableCpy(); // gives the "malloc'd version"

    char* tmp1 = node->getBeginDateTime();
    char* tmp2 = new char[strlen(tmp1)+1];
    strcpy(tmp2,tmp1);  tmp1[8]='\0'; tmp2+=8;

    m_BeginDate = (UInt_t)atoi(tmp1);
    m_BeginTime = (UInt_t)atoi(tmp2);
    delete [] tmp1; tmp2-=8; delete [] tmp2;

    tmp1 = node->getEndDateTime();
    tmp2 = new char[strlen(tmp1)+1];
    strcpy(tmp2,tmp1); tmp1[8]='\0';tmp2+=8;

    m_EndDate = (UInt_t)atoi(tmp1);
    m_EndTime = (UInt_t)atoi(tmp2);
    delete [] tmp1; tmp2-=8; delete [] tmp2;

  } else {

      SetNRows(0);
      SetBeginDate(19950101);
      SetBeginTime(0);
      SetEndDate(20380101);
      SetEndTime(0);
      return pData;
  }

return pData;
}

//______________________________________________________________________________
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
      m_descriptor[i].name[31]='\0';
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

if(!versionName){
  m_Tree=mgr->initConfig(configName,"reconV0");
}else{
  m_Tree=mgr->initConfig(configName,versionName);
}

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
dbConfig_st* configTable = 0;
numRows = 0;
if(!buildNodes(m_Tree,0)) return configTable;

numRows = m_Nodes->getNumNodes()-1;
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
StDbBroker::buildConfig(int numRows){

dbConfig_st* cTab= 0;
m_Nodes->reset();
int numNodes = m_Nodes->getNumNodes();

if(numNodes-1 != numRows){
  cerr<<"Error::#-mismatch, rows("<<numRows<<") & nodes("<<numNodes<<")"<<endl;
  return cTab;
}

cTab=(dbConfig_st*)calloc(numRows,sizeof(dbConfig_st));

StDbNode* node;
StDbNode* parent;
char* parName;
char* nodeName;
char* typeName;
char* id;
unsigned int parsize=sizeof(cTab[0].parname)-1;
unsigned int tabsize=sizeof(cTab[0].tabname)-1;
unsigned int typsize=sizeof(cTab[0].tabtype)-1;
int parID;

 for(int i=1; i<numNodes;i++){

   node  = m_Nodes->getNode(i);
   parent= m_Nodes->getParent(i);
   parID   = m_Nodes->getParentID(i);

   nodeName = node->getMyName();
   typeName = node->getCstrName();
   parName  = parent->getMyName();

   strncpy(cTab[i-1].parname,parName,parsize); cTab[i-1].parname[parsize]='\0';
   strncpy(cTab[i-1].tabname,nodeName,tabsize); cTab[i-1].tabname[tabsize]='\0';

   id=cTab[i-1].tabtype; *id='.'; id++;

   if(strstr(typeName,"None")){
     strcpy(id,"node");
   } else {
     strncpy(id,typeName,typsize-1); cTab[i-1].tabtype[typsize]='\0';
   }

   cTab[i-1].tabID=i;
   cTab[i-1].parID=parID;

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
int StDbBroker::DbInit(const char * dbName)
{  return ::DbInit(dbName) ;}








