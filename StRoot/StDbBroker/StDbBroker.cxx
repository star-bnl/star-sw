#include <iostream.h>
#include <iomanip.h>
#include <stdlib.h> 

#include <strstream.h>

#include "TString.h"

//needed for GetComments
#include "TROOT.h"
#include "TBuffer.h"
#include "TClass.h"
#include "St_Table.h"
#include "TRealData.h"
#include "TDataMember.h"
#include "TDataType.h"

#include "Api.h"
#include "StDbBroker.h"


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
    
    m_descriptor[i].name[19]='\0';
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
      m_descriptor[i].name[19]='\0';
  }


  // Check if request is a "hierarchy" : if so send request to "params" DB
  char* id = strstr(m_tableName,"_hierarchy");
  if(id){
    char* tmpName = new char[strlen(m_tableName)+1];
    strcpy(tmpName,m_tableName);
    char* id2 = strstr(tmpName,"_hierarchy");
    *id2 = '\0';
    if(strcmp(tmpName,"Calib")==0){
      m_database = new char[strlen("Calibrations")+1];
      strcpy(m_database,"Calibrations");
    } else {
     m_database = new char[strlen(tmpName)+1];
     strcpy(m_database,tmpName);
    }
     *id2='_';
    delete [] tmpName;
  }

  void *pDbData;
  
  if(id || strcmp(m_database,"params")==0){
    
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
//______________________________________________________________________________
int StDbBroker::DbInit(const char * dbName)
{  return ::DbInit(dbName) ;}








