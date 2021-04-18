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
          trigPrescales_st*   values = (trigPrescales_st*) tmp;
	  
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("RunLog_onl");
    StDbTable* tdT = node->addDbTable("trigPrescales");
    

   int* iList = new int[nrows];
   
   for(int i = 0; i < nrows; i++) {
        iList[i] = i;
   }

    tdT->SetTable((char*)values,nrows,iList);
    mgr->setStoreTime(beginTime);
    int value = mgr->storeDbTable(tdT);

    return 0;
}

// Prepare for writing 
   Fill_Prescales(int runNumber, unsigned int startRunTime) { 

    int nrows,nfields;
    char query[600];
    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;

    db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3501/Conditions_rts","","");

//    sprintf(query,"select unix_timestamp(beginTime), idx_rn,idx_trigger,l1_id,l1ps,l2_id,l2ps,l3_id,l3_ps from triggers where idx_rn = %u", runNumber);
    sprintf(query,"select unix_timestamp(beginTime), idx_rn,idx_trigger,l1_id,l1ps,l2_id,l2ps,l3_id,eff_ps from triggers where idx_rn = %u", runNumber);

    res = db->Query(query);

    nrows = res->GetRowCount();
    nfields = res->GetFieldCount();

//    cout << "Number of entries = " << nrows << " in Run " << runNumber<< endl;    
    trigPrescales_st* values = new trigPrescales_st[nrows*3];

    for(int i = 0 ; i < nrows ; i++){
      row = res->Next();
      values[3*i].runNumber  = atol(row->GetField(1));
      values[3*i].idxTrigger = atol(row->GetField(2));
      values[3*i].idxLevel  = 1;
      values[3*i].id = atol(row->GetField(3));
      values[3*i].ps = 1.0; // atof(row->GetField(4));
      values[3*i+1].runNumber  = atol(row->GetField(1));
      values[3*i+1].idxTrigger = atol(row->GetField(2));
      values[3*i+1].idxLevel  = 2;
      values[3*i+1].id = atol(row->GetField(5));
      values[3*i+1].ps = 1.0; // atof(row->GetField(6));
      values[3*i+2].runNumber  = atol(row->GetField(1));
      values[3*i+2].idxTrigger = atol(row->GetField(2));
      values[3*i+2].idxLevel  = 3;
      values[3*i+2].id = atol(row->GetField(7));
      values[3*i+2].ps = atof(row->GetField(8));

      delete row;
    }
    int value = write(nrows*3, startRunTime, values);
 
    delete values;
    delete res; // Delete query and pointer of trigPrescales
    delete db;    // Delete database
}

// Will Fill Tables for all Runs not filled in yet
void Fill_Prescales(){

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
//  db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3309/RunLog_onl","","");

  
  sprintf(query,"select max(runNumber) from trigPrescales");
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
      maxRunNumber = 20322001;
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
  db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3501/RunLog","","");

  sprintf(query,"select runNumber,startRunTime from runDescriptor where runNumber > %u order by runNumber ASC",maxRunNumber);
  res = db->Query(query);

  nrows = res->GetRowCount();
  nfields = res->GetFieldCount();
  
  // Put Fields into runNumber and startRunTime
  for(int i = 0 ; i < nrows ; i++){
      row = res->Next();
      runNumber = atoul(row->GetField(0));
      startRunTime = atoul(row->GetField(1));
      cout << "runNumber = " << runNumber << endl;
      Fill_Prescales(runNumber,startRunTime);
      delete row;
  }
  
  delete res; // Delete query and database
  delete db;
}

