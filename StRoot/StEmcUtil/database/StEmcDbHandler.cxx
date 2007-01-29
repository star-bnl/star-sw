//******************************************************************************

/*! \class StEmcDBHandler
    \author Marcia Maria de Moura
		
		  2003/01/02
		  Class to retrieve Emc database tables
*/		
//******************************************************************************

#include "StEmcDbHandler.h"
#include "TUnixTime.h"
#include "StMessMgr.h"

ClassImp(StEmcDbHandler)

//------------------------------------------------------------------------------
/*! Class constructor */
StEmcDbHandler::StEmcDbHandler()
{
  // Default table node is Calibrations_emc (see .h file) 
  setTableNode();
  setTimeStampLimits();  
  setTimeStamp();
  setMaxEntryTime();
  setFlavor();
}
//------------------------------------------------------------------------------
/*! Class destructor */
StEmcDbHandler::~StEmcDbHandler()
{
}
//------------------------------------------------------------------------------
/*! Retrieving of specified database Emc Table */
StDbTable* StEmcDbHandler::getDbTable()
{
  StDbManager* dbMngr = StDbManager::Instance();
  StDbTable* table = (dbMngr->initConfig(mTableNode.Data()))->addDbTable(mTableName.Data());
  
  //set beginTime
  dbMngr->setRequestTime(mTimeStamp.Data());

  //set flavor
  table->setFlavor(mFlavor.Data());

  //set maxEntryTime
  UInt_t unixEntryTime = (dbMngr->findDb(mTableNode.Data()))->getUnixTime(mMaxTime.Data());
  table->setProdTime(unixEntryTime);

  dbMngr->fetchDbTable(table);

  mTimeStamp = timeToSqlTime(table->getBeginDateTime()); 
  mTable = table;
  
  return mTable;  
}
//------------------------------------------------------------------------------
/*! Return time stamp list of an EMC table within specified limits*/
TString* StEmcDbHandler::getTimeStampList()
{
  
/*  for (UInt_t i=0; i< 1000; i++) mTimeStampList[i]="";
  
  StDbManager* dbMngr = StDbManager::Instance();
  StDbTable* table = (dbMngr->initConfig(mTableNode.Data()))->addDbTable(mTableName.Data());
  
  dbMngr->setStoreTime(mMaxTime.Data());
  
  StDataBaseI* db=dbMngr->findDb(mTableNode.Data());
  
  const char *lStamp, *fStamp, *entryTime;
  char whereClause[256];
  
  fStamp = mFstTimeStamp;
  lStamp = mLstTimeStamp;
  entryTime = mMaxTime;
  sprintf(whereClause, " where flavor='%s' and beginTime<='%s' and beginTime>='%s' and entryTime<='%s' ", mFlavor.Data(), lStamp, fStamp, entryTime); 
  
  char query[256];
//  ostrstream qs(query,256);
//  qs<<whereClause<<ends;
  LOG_INFO <<"\n QUERY = "<<whereClause<<endm;
  
  UInt_t* timeStampList;
  timeStampList = db->QueryDbTimes(table,whereClause,1);  // opt=1 means don't get any data!
  
  int nRows=table->GetNRows();
  int nRows2;
  table->getElementID(nRows2);
  if (nRows != nRows2) {
	  LOG_WARN<<"problem:  nRows!=nRows2"<<endm;
  }
  
  mListSize = nRows;
  
  for (int i = 0; i < nRows ; i++)
  {
    dbMngr->setRequestTime(timeStampList[i]); //let db translate to Human Readable
    mTimeStampList[i]= dbMngr->getDateRequestTime();
    mTimeStampList[i]=mTimeStampList[i](0,4)+"-"+
                      mTimeStampList[i](4,2)+"-"+
		      mTimeStampList[i](6,2)+" "+
		      mTimeStampList[i](8,2)+":"+
		      mTimeStampList[i](10,2)+":"+
		      mTimeStampList[i](12,2);
//	mTimeStampList[i] = timeToSqlTime((dbMngr->findDb(mTableNode.Data()))->getDateTime(timeStampList[i]));
  }
  
  
  delete [] timeStampList;
//  delete node;
  delete dbMngr;
*/  

mTimeStampList[0] = "::getTimeStampList segfaulted when I tested it, so this function is disabled until further notice --Adam";
  return mTimeStampList;
  
}
//-----------------------------------------------------------------------------
/* Set table options */
void StEmcDbHandler::setTableName(char* det, char* param)
{ 
  UInt_t option1, option2;

  if (strcmp(det,  "bemc") == 0) option1 = 0;
  if (strcmp(det,  "bprs") == 0) option1 = 1;
  if (strcmp(det, "bsmde") == 0) option1 = 2;
  if (strcmp(det, "bsmdp") == 0) option1 = 3;

  mDet = option1;
  char* prefix = det;
  
  if (strcmp(param, "calibration") == 0) option2 = 0;
  if (strcmp(param,        "gain") == 0) option2 = 1;
  if (strcmp(param,    "pedestal") == 0) option2 = 2;
  if (strcmp(param,      "status") == 0) option2 = 3;
  

  mParam = option2;
  char* sufix;

  if (mParam == 0) sufix = "Calib";
  if (mParam == 1) sufix = "Gain";
  if (mParam == 2) sufix = "Ped";
  if (mParam == 3) sufix = "Status";

  char name[50];
  sprintf(name,"%s%s",prefix,sufix);

  mTableName = name;
}
//------------------------------------------------------------------------------
/*! Set limits for time stamp list */
void StEmcDbHandler::setTimeStampLimits(char* stamp1, char* stamp2)
{
  mFstTimeStamp = stamp1;
  mLstTimeStamp = stamp2;
}
//------------------------------------------------------------------------------
/*! Convert char* time into sql format time */
TString StEmcDbHandler::timeToSqlTime(const char* time)
{
  TString string=time;
  TString dateString=string(0,4)+ "-" +string(4,2)+ "-" + string(6,2);
  TString timeString=string(8,2)+ ":" +string(10,2)+ ":" + string(12,2);
  TString datetime = dateString+ " " +timeString;
  //cout << " datetime.Data() = " << datetime.Data() << endl;
  return datetime;
  
}
//------------------------------------------------------------------------------









