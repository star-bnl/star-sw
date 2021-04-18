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

unsigned long atoul(const char * field){
    char * pEnd;
    return  strtoul(field,&pEnd,0);
}

void Migrate_tpcPadGainT0B(TSQLRow* row) {

   std::cout << "preparing to parse sql data \n";

   // Initialize db manager
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Calibrations_tpc");
    StDbTable* dbtable = node->addDbTable("tpcPadGainT0B");
    TString storeTime = row->GetField(0); // beginTime timestamp in MySQL format: "YYYY-MM-DD HH:mm:ss"
    mgr->setStoreTime(storeTime.Data());
    
    // Fill structure with data 
    Int_t run = atoul(row->GetField(1)); 

    float* dbGain = row->GetField(2);
    ULong_t dbGainSize = row->GetFieldLength(2);

    float* dbT0 = row->GetField(3);
    ULong_t dbT0Size = row->GetFieldLength(3);

    tpcPadGainT0_st tbT0;
    memcpy(tbT0.Gain, dbGain, dbGainSize);
    memcpy(tbT0.T0, dbT0, dbT0Size);

    std::cout << "filling tableset with data\n";

    const Int_t MAX_DB_INDEX = 24;
    tpcPadGainT0B_st table[MAX_DB_INDEX];

    for (Int_t s = 1; s <= MAX_DB_INDEX; s++) {
	for (Int_t r = 1; r <= 45; r++) {
	    for (Int_t p = 1; p <= 182; p++) {
		table[s-1].run = run;
		table[s-1].Gain[r-1][p-1] = tbT0.Gain[s-1][r-1][p-1];
		table[s-1].T0[r-1][p-1] = tbT0.T0[s-1][r-1][p-1];
	    }
	}
    }
    std::cout << "tableset is populated with data\n";

    //	Store data to the StDbTable
    dbtable->SetTable((char*)&table, MAX_DB_INDEX);

    //	Store table to database
    mgr->storeDbTable(dbtable);
    std::cout << "ready to store table! \n";

}

void Fill_tpcPadGainT0B_mod(){

    loadLibs();
    
    int nrows,nfields;
    string unixtime_latest;
          
    std::string query;
    std::ostringstream os;
    
    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;
    
    db = TSQLServer::Connect("mysql://robinson.star.bnl.gov:3306/Calibrations_tpc","","");

  
  query = "SELECT MAX(UNIX_TIMESTAMP(beginTime)), MAX(beginTime) FROM tpcPadGainT0B";
  res = db->Query(query.c_str());
  nrows = res->GetRowCount();
  nfields = res->GetFieldCount();

  if(nrows==0){
      cout << "oops, something bad happened: no pad/gains found in offline db?" << endl;	
      return;
  }

  row = res->Next();

  unixtime_latest = row->GetField(0);
  std::cout << "last known Offline timestamp: " << row->GetField(1) << " [ " << unixtime_latest << " ]" << std::endl;
  
  delete row;
  delete res; // Delete query and database
  delete db;

  db = TSQLServer::Connect("mysql://db04.star.bnl.gov:3412/Conditions_rts","test","");

  os.str("");
  os << "SELECT beginTime, run, Gain, T0 FROM tpcPadGainT0 WHERE beginTime > FROM_UNIXTIME('2013-10-29 10:00:00')";
//  os << "SELECT beginTime, run, Gain, T0 FROM tpcPadGainT0 ORDER BY beginTime desc LIMIT 1";
  res = db->Query(os.str().c_str());

  nrows = res->GetRowCount();
  std::cout << "Found " << nrows << " new records in online db" << std::endl;
  
  nfields = res->GetFieldCount();
  
  if (nrows > 0) {
    for (int i = 0; i < nrows; i++) {
        row = res->Next();
	Migrate_tpcPadGainT0B(row);
    }
  }  
   
  delete res; // Delete query and database
  delete db;
}

