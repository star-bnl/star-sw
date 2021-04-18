#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <iterator>

#include "pp2ppRPpositions.h"

#include "TSQLServer.h"
#include "TSQLRow.h"
#include "TSQLResult.h"

unsigned int libsLoaded = 0;

template <class T>
std::string toString(T& arg)
{
    std::ostringstream os;
    os << arg;
    return os.str();
}

template<class T>
T fromString(const std::string& s)
{
    std::istringstream stream (s);
    T t;
    stream >> t;
    return t;
}

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

void Migrate_pp2ppRPpositions(long run_number, long run_start, std::string run_start_ts) {
    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;
    int nrows = 0;
    std::string query;

    std::cout << "migrating run number: " << run_number << " with ts: " << run_start << std::endl;

    db = TSQLServer::Connect("mysql://onldb2.starp.bnl.gov:3606/mq_collector_Conditions_cdev","","");
    query = "SELECT beginTime, ";
    query += "`rp-b-left-hz-1-pos.measurementAvgM`, `rp-b-right-hz-1-pos.measurementAvgM`, `rp-b-bot-vert-2-pos.measurementAvgM`, `rp-b-top-vert-2-pos.measurementAvgM`, ";
    query += "`rp-y-left-hz-3-pos.measurementAvgM`, `rp-y-right-hz-3-pos.measurementAvgM`, `rp-y-bot-vert-4-pos.measurementAvgM`, `rp-y-top-vert-4-pos.measurementAvgM`";
    query += " FROM rpPositions WHERE beginTime < FROM_UNIXTIME(";
    query += toString(run_start);
    query += ") ORDER BY beginTime desc limit 1";
    std::cout << query << std::endl;

    res = db->Query(query.c_str());
    nrows = res->GetRowCount();
    if ( nrows == 1 ) {
	row = res->Next();
       // Initialize db manager
	StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Calibrations_pp2pp");
	StDbTable* dbtable = node->addDbTable("pp2ppRPpositions");
    TString storeTime = run_start_ts.c_str(); // beginTime timestamp in MySQL format: "YYYY-MM-DD HH:mm:ss"
	mgr->setStoreTime(storeTime.Data());
        // Create your c-struct
	pp2ppRPpositions_st table;
	table.b_left_W1D = fromString<double>(row->GetField(1));
	table.b_right_W1U = fromString<double>(row->GetField(2));
	table.b_bot_W2D = fromString<double>(row->GetField(3));
	table.b_top_W2U = fromString<double>(row->GetField(4));

	table.y_left_E1D = fromString<double>(row->GetField(5));
	table.y_right_E1U = fromString<double>(row->GetField(6));
	table.y_bot_E2D = fromString<double>(row->GetField(7));
	table.y_top_E2U = fromString<double>(row->GetField(8));
        // Store data to the StDbTable
	dbtable->SetTable((char*)&table, 1);
        // Store table to database
	mgr->storeDbTable(dbtable);

	std::cout << "run " << run_number << " migrated" << std::endl;
	} else {
			std::cout << "no data, skipping run" << std::endl;
    }

    delete row;
    delete res;
    delete db;
}

void Fill_pp2ppRPpositions() {
    loadLibs();

    int nrows = 0, nfields = 0;
    string unixtime_latest = "0";

    std::string query;
    std::ostringstream os;

    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;

    // find out latest entry from offline

    db = TSQLServer::Connect("mysql://robinson.star.bnl.gov:3306/Calibrations_pp2pp","","");

    query = "SELECT MAX(UNIX_TIMESTAMP(beginTime)), MAX(beginTime) FROM pp2ppRPpositions";
    res = db->Query(query.c_str());
    nrows = res->GetRowCount();
    nfields = res->GetFieldCount();

    row = res->Next();

    if ( !row->GetField(0) ) {
	std::cout << "last known Offline timestamp: " << " [ " << unixtime_latest << " ]" << std::endl;
    } else {
	unixtime_latest = row->GetField(0);
	std::cout << unixtime_latest << std::endl;
	std::cout << "last known Offline timestamp: " << row->GetField(1) << " [ " << unixtime_latest << " ]" << std::endl;
    }
    delete row;
    delete res;
    delete db;

    // hack to skip empty runs:
    // 1423766997 = 2015-02-12 18:49:57
    //unixtime_latest = "1423766997";

    // select matching beginTime, run number from online

    db = TSQLServer::Connect("mysql://onldb2.starp.bnl.gov:3501/RunLog","","");

    query = "SELECT runNumber, startRunTime, FROM_UNIXTIME(startRunTime) FROM runDescriptor where startRunTime > ";
    query += unixtime_latest;
    std::cout << query << std::endl;

    res = db->Query(query.c_str());
    nrows = res->GetRowCount();
    nfields = res->GetFieldCount();

    std::cout << "found runs: " << nrows << std::endl;

    if (nrows > 0) {
		for (int i = 0; i < nrows; i++) {
		    row = res->Next();
			std::cout << "trying to migrate run number: " << row->GetField(0) << ", start: " << row->GetField(1) << ", ts: " << row->GetField(2) << std::endl;
	    	Migrate_pp2ppRPpositions(fromString<long>(row->GetField(0)), fromString<long>(row->GetField(1)), row->GetField(2));
			std::cout << "run successfully migrated: " << row->GetField(0) << std::endl;
		}
    }

    delete row;
    delete res;
    delete db;

}