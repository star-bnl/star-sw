/*
  root.exe lDb.C Fill_MagnetAvg.C+
*/
#include "SqlUtils.h"
//#include <iostream.h>
#include "Riostream.h"
//#include <fstream.h>
#include "TSQLServer.h"
#include "TSQLRow.h"
#include "TSQLResult.h"
#include "OnDb.h"
#include "TMath.h"
#include "TDatime.h"
static Int_t _debug = 0;
TString database_RunLog;;
TString database_mq_collector_Conditions_rhic;
// Writes stuct into database
int write(int nrows,
	  unsigned int runNumber,
	  unsigned int startRunTime,
	  unsigned int time,
	  double  current){
  
  if(nrows == 0) {
    cout << "Run: " << runNumber << " Has No Entries!!! " << endl;
#if 0
  } else {
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("RunLog_onl");
    StDbTable* tdT = node->addDbTable("starMagOnl");
    
    starMagOnl_st* values = new starMagOnl_st[nrows];
    int* iList = new int[nrows];
    
    for(int i = 0; i < nrows ; i++){
      iList[i] = i;
      values[i].runNumber = runNumber;
      values[i].time = time[i];
      values[i].current = current[i];
    }
    cout << endl;
    
    tdT->SetTable((char*)values,nrows,iList);
    mgr->setStoreTime(startRunTime);
    int value = mgr->storeDbTable(tdT);
#endif
  }
  return 0;
}



int Fill_MagnetAvg(unsigned int runNumber,unsigned int startRunTime,unsigned int endRunTime, const TString &Out     ){
  int nrows,nfields;
  char query[600];
  TSQLServer *db = 0;
  TSQLResult *res = 0;
  TSQLRow *row = 0;
  
  
  /* First need to get last entry before startRunTime because rarely does the start run time have a value asociated with it   */    
  unsigned int firstTime = 0;
  double firstCurrent = 0;
  unsigned int firstStatus = 0;
  unsigned int bitMask = 1 << 15; // => 32768 => 100000
  
  //    db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3502/Conditions_rhic","","");
  //    database = OnDb(year,"dev_mainMagnet2,cdev_mainMagnetm");
  //    db = TSQLServer::Connect("mysql://dbbak.starp.bnl.gov:3411/mq_collector_Conditions_rhic","","");
  db = TSQLServer::Connect(database_mq_collector_Conditions_rhic.Data(),"","");
  sprintf(query,
	  //	  "select UNIX_TIMESTAMP(beginTime),mainMagnetCurrent,mainMagnetStatus from starMagnet where beginTime <= from_unixtime(%u) order by 1 desc limit 1",startRunTime);
	  "select UNIX_TIMESTAMP(beginTime),cdev_mainMagnet2,cdev_mainMagnetm,beginTime  from starMagnet where beginTime >= from_unixtime(%u) and beginTime <=  from_unixtime(%u) and cdev_mainMagnetB != ''",startRunTime,endRunTime);
  //  cout << query << endl;
  // new source of magnet data:
  // wfgRamp.mainMagnet.wM,wfgRamp.pttEast.wM,wfgRamp.pttWest.wM,wfgRamp.trimEast.wM,wfgRamp.trimWest.wM
  
  res = db->Query(query);
  nrows = res->GetRowCount();
  if (_debug) {
    cout << "runNumber = " << runNumber << "\tnrows = " << nrows << endl;
  }
  if(nrows==0)
    {
      delete row;
      delete res;
      delete db;
      //	  cout << "No entries before.. Kinda Weird! " << endl;
      return -1;
    }
  double tempCurrent = 0;
  unsigned int tempStatus = 0;
  unsigned int tempTime = 0;
  int writeFields = 1;
  unsigned int time = 0;
  double       current = 0;
  double    xN = 0, x2N = 0;
  double    RMS = 1e5;
  double    RMSold = 1e5;
  double     N = 0;
  for(int n = 1; n <= nrows; n++){
    row = res->Next();
    tempTime = atoul(row->GetField(0));
    tempCurrent = fabs(atod(row->GetField(1)));
    RMSold = RMS;
    if (TMath::Abs(tempCurrent) < 10000) {
      // cdev_mainMagnetm = 32778 => 100012
      //                    32768 => 100000
      tempStatus = atoul(row->GetField(2));
      if( bitMask & tempStatus )  // If 16th bit is set, then current is negative
	tempCurrent *= -1;
      if (RMS < 10) RMS = 10;
      if (n == 1) {
	firstTime = tempTime;
	xN = tempCurrent;
	x2N = tempCurrent*tempCurrent;
	N = 1;
      } else {
	if (TMath::Abs(xN - tempCurrent) < 3*RMS) {
	  N++;
	  xN  = (N-1)*xN/N  + tempCurrent/N;
	  x2N = (N-1)*x2N/N + tempCurrent*tempCurrent/N;
	  Double_t RMS2 = x2N - xN*xN;
	  if (RMS2 < 0) RMS2 = 0;
	  RMS = TMath::Sqrt(RMS2);
	} else {
	  if (N > 10 && RMSold < 4.0) {
	    cout << "Time : " << tempTime << "\t" << row->GetField(3) << "\tCurrent = " << tempCurrent << "\tStatus = " << tempStatus << "\txN = " << xN << "\tx2N = " << x2N << "\tRMS = " << RMS << endl;
	    cout << "Current changed before end of run. Stop averaging, change endRunTime from " << endRunTime << " to " << tempTime << endl;
	    delete row;
	    RMS = RMSold;
	    endRunTime = tempTime;
	    break;
	  }
	}
      }
      cout << "Time : " << tempTime << "\t" << row->GetField(3) << "\tCurrent = " << tempCurrent << "\tStatus = " << tempStatus << "\txN = " << xN << "\tx2N = " << x2N << "\tRMS = " << RMS << endl;
      //     time[writeFields] = tempTime;
      //      current[writeFields] = tempCurrent;
      //    writeFields++;
    }
    delete row;
  }
  if (N > 0 && RMS < 4.0) {
    cout << "write " << Out.Data() << endl;
    ofstream out;
    out.open(Out, ios::out); 
    out << "TDataSet *CreateTable() {" << endl;
    out << "  if (!TClass::GetClass(\"St_starMagAvg\")) return 0;" << endl;
    out << "  starMagAvg_st row = {" << runNumber << "," <<  startRunTime << "," << endRunTime<< "," << (int) N << "," << xN << "," << RMS << "};" << endl;
    out << "  St_starMagAvg *tableSet = new St_starMagAvg(\"starMagAvg\",1);" << endl;
    out << "  tableSet->AddAt(&row.runNumber);" << endl;
    out << "  return (TDataSet *)tableSet;" << endl;
    out << "}" << endl;
    out.close();
  } else {
    cout << "skip " << Out.Data() << "with N = " << N << " and RMS = " << RMS << endl;
  }
  // write values in
  int value = write(writeFields,runNumber,startRunTime,time,current);
  
  delete res; // Delete query and database
  delete db;
  
//  delete current;
//  delete time;
  
  return value;
  
}

// Will Fill Tables for all Runs not filled in yet
void Fill_MagnetAvg(Int_t year=2021){
#if 0
  loadLibs();
#endif    
  int nrows,nfields;
  unsigned int maxRunNumber;
  unsigned int runNumber;
  unsigned int startRunTime;
  unsigned int endRunTime;
  
  char query[600];
  TSQLServer *db = 0;
  TSQLResult *res = 0;
  TSQLRow *row = 0;
  
#if 0  
  ///
    // Get DATABASE RUNLOG_rhic
    db = TSQLServer::Connect("mysql://robinson.star.bnl.gov:3306/RunLog_onl","","");
    
    sprintf(query,"select max(runNumber) from starMagOnl");
    res = db->Query(query);
    // Get Rows and Fields, should only have 1 row and two fields
    nrows = res->GetRowCount();
    nfields = res->GetFieldCount();
    
    if(nrows==0){
      cout << "oops" << endl;
    }
    // Put Fields into startRunTime and startEndTime
    // Use For loop incase multiple entries, will only use time from last one
    
    row = res->Next();
    
    sprintf(query,"%s",row->GetField(0));
    
    if(query[0] == '\0' ){
      maxRunNumber = 22026001;
      cout << "max run = " << maxRunNumber << endl;
    }
    else{
      maxRunNumber = atoul(row->GetField(0));
    }
    
    delete row;
    delete res; // Delete query and database
    delete db;
    
    if ( maxRunNumber < 22028023 ) { maxRunNumber = 22028023; }
    
    cout << "max runNumber is " << maxRunNumber << endl;
#endif
    database_mq_collector_Conditions_rhic = "mysql://";
    database_mq_collector_Conditions_rhic += OnDb(year, "mq_collector_Conditions_rhic");
    database_mq_collector_Conditions_rhic += "/"; database_mq_collector_Conditions_rhic += "mq_collector_Conditions_rhic";
    // Get DATABASE RUNLOG
    //  db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3501/RunLog","","");
    //  db = TSQLServer::Connect("mysql://dbbak.starp.bnl.gov:3411/RunLog","","");
    database_RunLog = "mysql://";
    database_RunLog += OnDb(year, "RunLog");
    database_RunLog += "/"; database_RunLog += "RunLog";
    db = TSQLServer::Connect(database_RunLog.Data(),"","");
    //  sprintf(query,"select runNumber,startRunTime,endRunTime from runDescriptor where runNumber > %u order by 1",maxRunNumber);
    if (year == 2001) {
      sprintf(query,"select runNumber,beginRunTime,endRunTime,beginTime from runDescriptor order by 1");
    } else {
      sprintf(query,"select runNumber,startRunTime,endRunTime,beginTime from runDescriptor order by 1");
    }
    res = db->Query(query);
    
    nrows = res->GetRowCount();
    nfields = res->GetFieldCount();
    
    // Put Fields into startRunTime and startEndTime
    // Use For loop incase multiple entries, will only use time from last one
    for(int i = 0 ; i < nrows ; i++){
      row = res->Next();
      runNumber = atoul(row->GetField(0));
      startRunTime = atoul(row->GetField(1));
      endRunTime = atoul(row->GetField(2));
      if (_debug) {
	cout << "runNumber = " << runNumber << "\tbeginTime = " << row->GetField(3) << endl;
      }
      TDatime tGMT(row->GetField(3));
      TString Out(Form("starMagAvg.%8i.%06i.C",tGMT.GetDate(),tGMT.GetTime()));
      TString OutH = Out;
      OutH += ".HOLD.ofl";
      if (gSystem->AccessPathName(Out) && gSystem->AccessPathName(OutH)) {
	Fill_MagnetAvg(runNumber,startRunTime,endRunTime,Out);
      }
      delete row;
    }
    
    
    delete res; // Delete query and database
    delete db;
    
}
