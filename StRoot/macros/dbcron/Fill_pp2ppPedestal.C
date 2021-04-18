#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <iterator>

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

std::vector<std::string> explode(const std::string& str, const char& delim) {
    std::vector<std::string> result;
    std::string token;
    std::istringstream iss(str);
    while ( getline(iss, token, delim) ) {
        result.push_back(token);
    }
    return result;
}

std::string implode(const std::vector<std::string>& v, const std::string& delim)
{
    std::ostringstream o;
    if ( *(v.begin()) == "0.0" || *(v.begin()) == "0." || *(v.begin()) == "0" || *(v.begin()) == "0.00" || *(v.begin()) == "0.000") {
	o << "-1.0";
    } else {
	o << *(v.begin());
    }
    for (std::vector<std::string>::iterator it = v.begin()+1; it != v.end(); ++it) {
	if ( *it == "0.0" || *it == "0." || *it == "0" || *it == "0.000" || *it == "0.00" ) {
	    o << delim << "-1.0";
	} else { 
	    o << delim << *it;
	}
    }
    return o.str();
}

void Migrate_pp2ppPedestal(TSQLRow* row) {

    std::vector<std::string> ped = explode(row->GetField(1), ' ');
    std::vector<std::string> rms = explode(row->GetField(2), ' ');

    std::string ped_final = implode(ped, ",");
    std::string rms_final = implode(rms, ",");

    std::ostringstream query;
    query << "INSERT INTO `Calibrations_pp2pp`.`pp2ppPedestal160` (nodeID, elementID, flavor, schemaID, beginTime, mean, rms) VALUES (";
    query << "3, 0, 'ofl', 1, '" << row->GetField(0) << "', '" << ped_final << "', '" << rms_final << "')";

    TSQLServer *db = 0;
    TSQLResult *res = 0;

    db = TSQLServer::Connect("mysql://robinson.star.bnl.gov:3306/Calibrations_pp2pp","","");
    res = db->Query(query.str().c_str());

    delete res;
    delete db;
}

void Fill_pp2ppPedestal() {
    loadLibs();

    int nrows = 0, nfields = 0;
    string unixtime_latest = "0";

    std::string query;
    std::ostringstream os;

    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;

    db = TSQLServer::Connect("mysql://robinson.star.bnl.gov:3306/Calibrations_pp2pp","","");

    query = "SELECT MAX(UNIX_TIMESTAMP(beginTime)), MAX(beginTime) FROM pp2ppPedestal160";
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

	if ( unixtime_latest < 1487189400 ) { unixtime_latest = 1487189400; }

    db = TSQLServer::Connect("mysql://onldb2.starp.bnl.gov:3502/Conditions_pp2pp","","");

    os.str("");
    os << "SELECT beginTime, ped, rms FROM pp2ppPedestals WHERE beginTime > FROM_UNIXTIME(" << unixtime_latest << ")";
    res = db->Query(os.str().c_str());

    nrows = res->GetRowCount();
    std::cout << "Found " << nrows << " new records in online db" << std::endl;

    nfields = res->GetFieldCount();

  if (nrows > 0) {
    for (int i = 0; i < nrows; i++) {
	row = res->Next();
	Migrate_pp2ppPedestal(row);
    }
  }

  delete res; // Delete query and database
  delete db;

}

