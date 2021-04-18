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

      triggerID_st* values = (triggerID_st*) tmp;
	if ( !values ) {
		std::cout << "empty values array, cannot write" << std::endl;
	}
	  
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("RunLog_onl");
    StDbTable* tdT = node->addDbTable("triggerID");
    

   int* iList = new int[nrows];
   if (nrows > 1) cout << "---------- nrows > 1 ---------" << endl;
   for(int i = 0; i < nrows; i++) {
        iList[i] = i;
//      if( nrows > 1 ) {
        cout << "nows (nentries of run)  and beginTime =     " << nrows 
             << " " << beginTime << endl;

        cout << "Dumping data: " << "runNumber =     " << values[i].runNumber 
             << " idxTrg =    " << values[i].idxTrg 
             << " daqTrgId =  " << values[i].daqTrgId 
	     << " offTrgId  = " << values[i].offlineTrgId << endl;

         cout << "Nversion =  "  << values[i].trgNameVersion 
	     << " Version =   " << values[i].trgVersion
	     << " thVersion = " << values[i].threashVersion
	     << " psVersion = " << values[i].psVersion << endl;
//      }

    }
    tdT->SetTable((char*)values,nrows,iList);
    mgr->setStoreTime(beginTime);
    int value = mgr->storeDbTable(tdT);
    return 0;
}

// Prepare for writing 
   Fill_Trigger(int runNumber, unsigned int startRunTime) { 

    int nrows,nfields;
    char query[600];
    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;

    db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3501/Conditions_rts","","");

    sprintf(query,"select unix_timestamp(beginTime), idx_rn,idx_trigger,idx_trigger,offlinebit,trgNameVersion,trgVersion,threshVersion,psVersion from triggers where idx_rn = %u", runNumber);

    res = db->Query(query);

    nrows = res->GetRowCount();
    nfields = res->GetFieldCount();

//    cout << "Number of entries = " << nrows << " in Run " << runNumber<< endl;    
    triggerID_st* values = new triggerID_st[nrows];

    for(int i = 0 ; i < nrows ; i++){
      row = res->Next();
      values[i].runNumber  = atoul(row->GetField(1));
      values[i].idxTrg = atoul(row->GetField(2));
      values[i].daqTrgId  = atoul(row->GetField(3));
      values[i].offlineTrgId = atoul(row->GetField(4));
      values[i].trgNameVersion = atoul(row->GetField(5));
      values[i].trgVersion = atoul(row->GetField(6));
      values[i].threashVersion = atoul(row->GetField(7));
      values[i].psVersion = 1; // atoul(row->GetField(8));

      delete row;
    }
    int value = write(nrows, startRunTime, values);
 
    delete values;
    delete res; // Delete query and pointer of triggerID
    delete db;    // Delete database
}

// Will Fill Tables for all Runs not filled in yet
void Fill_Trigger(){

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
    //db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3309/RunLog_onl","","");

  
  sprintf(query,"select max(runNumber) from triggerID");
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
  db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3501/RunLog","","");

  sprintf(query,"select runNumber,startRunTime from runDescriptor where runNumber > %u",maxRunNumber);
  res = db->Query(query);

  nrows = res->GetRowCount();
  nfields = res->GetFieldCount();
  
  // Put Fields into runNumber and startRunTime
  for(int i = 0 ; i < nrows ; i++){
      row = res->Next();
      runNumber = atoul(row->GetField(0));
      startRunTime = atoul(row->GetField(1));
//      cout << "runNumber = " << runNumber << endl;
      Fill_Trigger(runNumber,startRunTime);
      delete row;
  }
  
  delete res; // Delete query and database
  delete db;
}

