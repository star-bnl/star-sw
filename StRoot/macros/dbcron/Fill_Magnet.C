#include "SqlUtils.h"
#include <iostream.h>
#include <fstream.h>
#include "TSQLServer.h"
#include "TSQLRow.h"
#include "TSQLResult.h"

unsigned int libsLoaded = 0;

void loadLibs(){
  if( !libsLoaded){
	gROOT->Macro("LoadLogger.C");
	gSystem->Load("St_base.so");
	gSystem->Load("libStDb_Tables.so");
	gSystem->Load("StDbLib.so");
	gSystem->Load("libRMySQL.so");
	libsLoaded = 1;
  }
    
}

// Writes stuct into database
int write(int nrows,
	  unsigned int runNumber,
	  unsigned int startRunTime,
	  unsigned int* time,
	  double* current){

    if(nrows == 0)
	cout << "Run: " << runNumber << " Has No Entries!!! " << endl;

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

    return 0;
}



int Fill_Magnet(unsigned int runNumber,unsigned int startRunTime,unsigned int endRunTime){

    loadLibs();
    
    int nrows,nfields;
    char query[600];
    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;

    
    /* First need to get last entry before startRunTime because rarely does the start run time have a value asociated with it
     */    
    unsigned int firstTime = 0;
    double firstCurrent = 0;
    unsigned int firstStatus = 0;
    unsigned int bitMask = 1 << 15;
    
    db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3502/Conditions_rhic","","");
    sprintf(query,
	  "select UNIX_TIMESTAMP(beginTime),mainMagnetCurrent,mainMagnetStatus from starMagnet where beginTime <= from_unixtime(%u) order by 1 desc limit 1",startRunTime);

	// new source of magnet data:
	// wfgRamp.mainMagnet.wM,wfgRamp.pttEast.wM,wfgRamp.pttWest.wM,wfgRamp.trimEast.wM,wfgRamp.trimWest.wM

    res = db->Query(query);
    nrows = res->GetRowCount();
    
    if(nrows==0)
      {
	  delete row;
	  delete res;
	  delete db;
	  cout << "No entries before.. Kinda Weird! " << endl;
	  return -1;
      }

    row = res->Next();
    firstTime    = atoul(row->GetField(0));      
    firstCurrent = fabs(atod(row->GetField(1)));
    firstStatus  = atoul(row->GetField(2));
    delete row;
    delete res;
    
    if( bitMask & firstStatus )  // If 16th bit is set, then current is negative
	firstCurrent *= -1;
    
    // Now get between times
    // Get DATABASE Condtions_rhic,
    
    sprintf(query,
	    "select unix_timestamp(beginTime),mainMagnetCurrent,mainMagnetStatus from starMagnet where beginTime between from_unixtime(%u) and from_unixtime(%u) order by 1",startRunTime,endRunTime);
    
  res = db->Query(query);
  nrows = res->GetRowCount();

  double* current = new double[nrows + 2];
  unsigned int* time = new unsigned int[nrows + 2];

  current[0] = firstCurrent;
  time[0] = firstTime;

  if(nrows==0)
      {
	  delete row;
	  delete res;
	  delete db;
	  write(1,runNumber,startRunTime,time,current); // mark no entries with 0 time
	  return -1;
      }
  
  nfields = res->GetFieldCount();

  int i=0;
  int writeFields = 1;
    
  double tempCurrent = 0;
  unsigned int tempStatus = 0;
  unsigned int tempTime = 0;
  
  for(i = 0; i < nrows; i++){
      row = res->Next();
      tempTime = atoul(row->GetField(0));
      tempCurrent = fabs(atod(row->GetField(1)));
      tempStatus = atoul(row->GetField(2));
      delete row;

      if( bitMask & tempStatus )  // If 16th bit is set, then current is negative
	      tempCurrent *= -1;
      
      // Only write current is 5% differnet from previous current
      if( TMath::Abs(tempCurrent - firstCurrent) > 50 ){ // If current changes by more than 50 

	  if(firstTime != time[writeFields - 1]){  // If 
	      time[writeFields] = firstTime;
	      current[writeFields] = firstCurrent;
	      writeFields++;
	  }
	      firstTime = tempTime;
	      firstCurrent = tempCurrent;
	      time[writeFields] = tempTime;
	      current[writeFields] = tempCurrent;
	      writeFields++;
      }
      else{
	  time[writeFields-1] = tempTime;
      }
  }

  delete res;
  
  // Now need to check first value after run and see if consistant with last value, if not add it in:
  sprintf(query,
	  "select UNIX_TIMESTAMP(beginTime),mainMagnetCurrent,mainMagnetStatus from starMagnet where beginTime >= from_unixtime(%u) order by 1 limit 1",endRunTime);

    res = db->Query(query);
    nrows = res->GetRowCount();
    
    if(nrows==0)
      {
	  delete row;
	  delete res;
	  delete db;
	  cout << "No entries after.. Kinda Weird! " << endl;
	  write(writeFields,runNumber,startRunTime,time,current);
	  return -1;
      }

    row = res->Next();
    tempTime    = atoul(row->GetField(0));      
    tempCurrent = fabs(atod(row->GetField(1)));
    tempStatus  = atoul(row->GetField(2));
    delete row;
    delete res;
    
    if( bitMask & tempStatus )  // If 16th bit is set, then current is negative
	firstCurrent *= -1;

    if( TMath::Abs(tempCurrent - firstCurrent) > 50 ){
	cout << "Current Changed After Run" << endl;
	time[writeFields] = tempTime;
	current[writeFields] = tempCurrent;
	writeFields++;
    }
	
  // write values in
  int value = write(writeFields,runNumber,startRunTime,time,current);

  delete res; // Delete query and database
  delete db;
  
  delete current;
  delete time;

  return value;

}

// Will Fill Tables for all Runs not filled in yet
void Fill_Magnet(){

    loadLibs();
    
    int nrows,nfields;
    unsigned int maxRunNumber;
    unsigned int runNumber;
    unsigned int startRunTime;
    unsigned int endRunTime;
    
    char query[600];
    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;
    
  
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

   // Get DATABASE RUNLOG
  db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3501/RunLog","","");

  sprintf(query,"select runNumber,startRunTime,endRunTime from runDescriptor where runNumber > %u order by 1",maxRunNumber);
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
      cout << "runNumber = " << runNumber << endl;
      Fill_Magnet(runNumber,startRunTime,endRunTime);
      delete row;
  }
  
  
  delete res; // Delete query and database
  delete db;
    
}
