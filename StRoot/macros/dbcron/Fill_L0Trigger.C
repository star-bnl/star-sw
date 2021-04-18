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
    	  
int write(unsigned int nrows,
	  unsigned int beginTime, void* tmp) {
          L0TriggerInfo_st*   values = (L0TriggerInfo_st*) tmp;
	  
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("RunLog_onl");
    StDbTable* tdT = node->addDbTable("L0TriggerInfo");
    

   int* iList = new int[nrows];
   
   for(int i = 0; i < nrows; i++) {
        iList[i] = i;
//        if(nrows > 1) {
/*
        cout << "now  = " << nrows 
             << "  beginTime = "  << beginTime << endl
             << "runNumber  = " << values[i].runNumber
             << "  daqTrigId  = " << values[i].daqTriggerId
             << "  OffTrigId  = " << values[i].offlineTriggerId
             << "  psL0  = " << values[i].psL0 << endl
             << "name  = " << values[i].name 
             << "  LiveOnBits  = "  << values[i].detectorLiveOnBits
             << "  LiveOffBits = " << values[i].detectorLiveOffBits
             << "  Requests    = " << values[i].detectorRequest << endl<< endl;
*/
//        }

   }

    tdT->SetTable((char*)values,nrows,iList);
    mgr->setStoreTime(beginTime);
    int value = mgr->storeDbTable(tdT);

    return 0;
}

// Prepare for writing 
   Fill_L0Trigger(int runNumber, unsigned int startRunTime) { 

    int nrows,nfields;
    char query[600];
    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;

    db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3501/Conditions_rts","","");

    sprintf(query,"select unix_timestamp(beginTime), idx_rn,idx_trigger,
            offlineBit,L0ps,name,detectorLiveOnBits,detectorLiveOffBits,
            detectorRequest from triggers where idx_rn = %u", runNumber);

    res = db->Query(query);

    nrows = res->GetRowCount();
    nfields = res->GetFieldCount();

//    cout << "Number of entries = " << nrows << " in Run " << runNumber<< endl;    
    L0TriggerInfo_st* values = new L0TriggerInfo_st[nrows];

    for(int i = 0 ; i < nrows ; i++){
      row = res->Next();
      values[i].runNumber  = atoi(row->GetField(1));
      values[i].daqTriggerId = atoi(row->GetField(2));
      values[i].offlineTriggerId  = atoi(row->GetField(3));
      values[i].psL0 = atoi(row->GetField(4));
      strcpy(values[i].name,row->GetField(5));
      values[i].detectorLiveOnBits = (unsigned int) atoi(row->GetField(6));
      values[i].detectorLiveOffBits = (unsigned int) atoi(row->GetField(7));
      values[i].detectorRequest = (unsigned int) atoi(row->GetField(8));

      delete row;
    }
    int value = write(nrows, startRunTime, values);
 
    delete values;
    delete res; // Delete query and pointer of L0TriggerInfo
    delete db;    // Delete database
}

// Will Fill Tables for all Runs not filled in yet
void Fill_L0Trigger(){

    loadLibs();
    
    int nrows,nfields;
    unsigned int maxRunNumber;
    unsigned int runNumber, startRunTime;
          
    char query[600];
    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;
    
// Get DATABASE RUNLOG_rhic
   
  db = TSQLServer::Connect("mysql://robinson.star.bnl.gov:3306/RunLog_onl","","");
//  db = TSQLServer::Connect("mysql://onlsun1.starp.bnl.gov:3309/RunLog_onl","","");

  
  sprintf(query,"select max(runNumber) from L0TriggerInfo");
  res = db->Query(query);
  // Get Rows and Fields, should only have 1 row and two fields
  nrows = res->GetRowCount();
  nfields = res->GetFieldCount();

  if(nrows==0){
      cout << "oops" << endl;
  }

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

  if ( maxRunNumber < 22026001 ) { maxRunNumber = 22026001; }

  cout << "max runNumber is " << maxRunNumber << endl;


   // Get DATABASE RUNLOG
    
//  maxRunNumber = 4000000;
//  maxRunNumber = 4003000;
  db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3501/RunLog","","");

  sprintf(query,"select runNumber,startRunTime from runDescriptor where 
          runNumber > %u",maxRunNumber);
  res = db->Query(query);

  nrows = res->GetRowCount();
  nfields = res->GetFieldCount();
  
  // Put Fields into runNumber and startRunTime
  for(int i = 0 ; i < nrows ; i++){
      row = res->Next();
      runNumber = atoul(row->GetField(0));
      startRunTime = atoul(row->GetField(1));
      cout << "runNumber = " << runNumber << endl;
      Fill_L0Trigger(runNumber,startRunTime);
      delete row;
  }
  
  delete res; // Delete query and database
  delete db;
}

