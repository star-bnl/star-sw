#include <iostream.h>
#include <fstream.h>
#include "TSQLServer.h"
#include "TSQLRow.h"
#include "TSQLResult.h"
#include "itpcPadGainT0.h"

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

unsigned long atoul(const char * field){
    char * pEnd;
    return  strtoul(field,&pEnd,0);
}

void Migrate_itpcPadGainT0(TSQLRow* row) {

   // Initialize db manager
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Calibrations_tpc");
    StDbTable* dbtable = node->addDbTable("itpcPadGainT0");
    TString storeTime = row->GetField(0); // beginTime timestamp in MySQL format: "YYYY-MM-DD HH:mm:ss"
    mgr->setStoreTime(storeTime.Data());

    // Create your c-struct
    itpcPadGainT0_st table;
    
    // Fill structure with data 
    table.run = atoul(row->GetField(1)); 

    char* dbGain = row->GetField(2);
    ULong_t dbGainSize = row->GetFieldLength(2);

    char* dbT0 = row->GetField(3);
    ULong_t dbT0Size = row->GetFieldLength(3);

    memcpy(table.Gain, dbGain, dbGainSize);
    memcpy(table.T0, dbT0, dbT0Size);

    std::cout << "Great!" << std::endl;

    //	Store data to the StDbTable
    dbtable->SetTable((char*)&table, 1);

    //	Store table to database
    mgr->storeDbTable(dbtable);

}

void Fill_itpcPadGainT0(){

    loadLibs();
    
    int nrows,nfields;
    string unixtime_latest;
          
    std::string query;
    std::ostringstream os;
    
    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;

    db = TSQLServer::Connect("mysql://robinson.star.bnl.gov:3306/Calibrations_tpc","","");
  
  query = "SELECT MAX(UNIX_TIMESTAMP(beginTime)), MAX(beginTime) FROM itpcPadGainT0";
  res = db->Query(query.c_str());
  nrows = res->GetRowCount();
  nfields = res->GetFieldCount();

  if ( nrows > 0 ) {
	row = res->Next();
	unixtime_latest = ( row->GetField(0) == 0 ? "0" : row->GetField(0) );
	delete row;
	std::cout << "last known Offline timestamp: " << " [ " << unixtime_latest << " ]" << std::endl;
  } else {
	std::cout << "error, no rows" << std::endl;
	return;
  }

  delete res; // Delete query and database
  delete db;

  db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3501/Conditions_rts","","");

  os.str("");
  os << "SELECT beginTime, run, Gain, T0 FROM itpcPadGainT0 WHERE beginTime > FROM_UNIXTIME(" << unixtime_latest << ")";
  res = db->Query(os.str().c_str());

  nrows = res->GetRowCount();
  std::cout << "Found " << nrows << " new records in online db" << std::endl;
  
  nfields = res->GetFieldCount();
  
  if (nrows > 0) {
    for (int i = 0; i < nrows; i++) {
        row = res->Next();
	Migrate_itpcPadGainT0(row);
    }
  }  
   
  delete res; // Delete query and database
  delete db;
}

