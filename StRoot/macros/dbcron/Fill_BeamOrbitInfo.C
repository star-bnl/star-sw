#include "SqlUtils.h"
#include <iostream.h>
#include <fstream.h>
#include "TSQLServer.h"
#include "TSQLRow.h"
#include "TSQLResult.h"
#include "beamOrbitInfo.h"

unsigned int nbins;
unsigned long * unixTime = 0;
unsigned long * zdcX = 0;
unsigned long * zdcXError = 0;

unsigned int libsLoaded = 0;

void loadLibs() {
  if ( !libsLoaded) {
	gROOT->Macro("LoadLogger.C");
	gSystem->Load("St_base.so");
	gSystem->Load("libStDb_Tables.so");
	gSystem->Load("StDbLib.so");
	gSystem->Load("libRMySQL.so");
	libsLoaded = 1;
  }
}

/*

// Writes stuct into database
int write(unsigned int beginTime,
	  unsigned int runNumber,
	  unsigned int entryTag,
	  char* blueSpecies,
	  unsigned int blueMassNumber,
	  double blueEnergy,
	  double blueIntensity,
	  double blueLifeTime,
	  double blueBunchIntensity,
	  double blueFillNumber,
	  char* yellowSpecies,
	  unsigned int yellowMassNumber,
	  double yellowEnergy,
	  double yellowIntensity,
	  double yellowLifeTime,
	  double yellowBunchIntensity,
	  double yellowFillNumber){
    	  
	  
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("RunLog_onl");
    StDbTable* tdT = node->addDbTable("beamInfo");
    
    beamInfo_st values;
    values.entryTag = entryTag;
    values.runNumber = runNumber;
    strcpy(values.blueSpecies,blueSpecies);
    values.blueMassNumber = blueMassNumber;
    values.blueEnergy = blueEnergy;
    values.blueIntensity = blueIntensity;
    values.blueLifeTime = blueLifeTime;
    values.blueBunchIntensity = blueBunchIntensity;
    values.blueFillNumber = blueFillNumber;
    strcpy(values.yellowSpecies,yellowSpecies);
    values.yellowMassNumber = yellowMassNumber;
    values.yellowEnergy = yellowEnergy;
    values.yellowIntensity = yellowIntensity;
    values.yellowLifeTime = yellowLifeTime;
    values.yellowBunchIntensity = yellowBunchIntensity;
    values.yellowFillNumber = yellowFillNumber;
    
    tdT->SetTable((char*)&values,1);
    mgr->setStoreTime(beginTime);
    int value = mgr->storeDbTable(tdT);

    return 0;
}
*/

// Will Fill Tables for all Runs not filled in yet
void Fill_BeamOrbitInfo(){

    loadLibs();
    
    Int_t xnrows,nfields;
    unsigned int maxRunNumber;
    unsigned int runNumber;
    unsigned int startRunTime;
    unsigned int endRunTime;
          
    char query[2000];

    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;
    
  // get last processed runNumber from offline db
  
  ///
  // Get DATABASE RUNLOG_rhic
  db = TSQLServer::Connect("mysql://robinson.star.bnl.gov:3306/RunLog_onl","","");

  
  sprintf(query,"select max(runNumber) from beamOrbitInfo where runNumber < 21999999");
  res = db->Query(query);
  // Get Rows and Fields, should only have 1 row and two fields
  // cout << "Db ref is " << res << endl;
  // cout << "Expecting " << res->GetRowCount() << endl;

  xnrows = (res->GetRowCount());
  nfields = res->GetFieldCount();

  if ( xnrows==0 ) {
      cout << "oops, no data from max(runNumber) query" << endl;
  	sprintf ( query,"%s", '0' );
  } else {
  	row = res->Next();
  	sprintf ( query,"%s",row->GetField(0) );
  }

  if ( query[0] == '\0' ) {
      maxRunNumber = 22026001;
      cout << "max run = " << maxRunNumber << endl;
  } else{
      maxRunNumber = atoul(row->GetField(0));
  }

  delete row; row = 0;
  delete res; res = 0; // Delete query and database
  delete db; db = 0;

  if ( maxRunNumber < 22026001 ) { maxRunNumber = 22026001; }

  cout << "max runNumber is " << maxRunNumber << endl;

  // get runNumber > lp_runNumber and its beginTime from the online db

  // Get DATABASE RUNLOG
  db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3501/RunLog","","");

//  sprintf(query,"SELECT runNumber,startRunTime,endRunTime FROM runDescriptor WHERE runNumber > %u ORDER BY runNumber ASC LIMIT 1", maxRunNumber);

  sprintf(query,"SELECT runNumber,startRunTime,endRunTime FROM runDescriptor WHERE runNumber > %u and runNumber < %u ORDER BY runNumber ASC LIMIT 1", maxRunNumber, 21999999);
  res = db->Query(query);

  xnrows = res->GetRowCount();
  nfields = res->GetFieldCount();

  if ( xnrows > 0 ) {
      row = res->Next();
      runNumber = atoul(row->GetField(0));
      startRunTime = atoul(row->GetField(1));
      endRunTime = atoul(row->GetField(2));
      cout << "got " << xnrows << " row, runNumber = " << runNumber << ", startRunTime = " << startRunTime << ", endRunTime = " << endRunTime << endl;
      delete row;
  } else {
	std::cout << "beamOrbitInfo: no updates found" << std::endl;
	return;
  }

  delete res; // Delete query and database
  delete db;

  std::cout << "gettting beginTime for the runnumber " << runNumber << std::endl;
  // get data for the beginTime from previous step

  sprintf( query, "SELECT unix_timestamp(beginTime), `rbpm.b-g5-bhx.avgOrbPositionM` as blue_beamPos5_horizontal, `rbpm.b-g5-bvx.avgOrbPositionM` as blue_beamPos5_vertical, `rbpm.b-g6-bhx.avgOrbPositionM` as blue_beamPos6_horizontal, `rbpm.b-g6-bvx.avgOrbPositionM` as blue_beamPos6_vertical, `rbpm.y-g5-bhx.avgOrbPositionM` as yellow_beamPos5_horizontal, `rbpm.y-g5-bvx.avgOrbPositionM` as yellow_beamPos5_vertical, `rbpm.y-g6-bhx.avgOrbPositionM` as yellow_beamPos6_horizontal, `rbpm.y-g6-bvx.avgOrbPositionM` as yellow_beamPos6_vertical, `buckets.blue.filledBucketsS`, `buckets.yellow.filledBucketsS` FROM `beamPositionMonitors` WHERE beginTime BETWEEN FROM_UNIXTIME(%u) AND FROM_UNIXTIME(%u) ORDER BY beginTime ASC", startRunTime, endRunTime);
  //cout << "query = " << query << endl;

  db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3606/mq_collector_Conditions_cdev","","");

  res = db->Query(query);

  xnrows = res->GetRowCount();
  nfields = res->GetFieldCount();

  if ( xnrows <= 0 ) {
	  std::cout << "found no beamOrbitInfo records during run " << runNumber << ", trying last entry" << std::endl;
	  sprintf(query,"SELECT unix_timestamp(beginTime), `rbpm.b-g5-bhx.avgOrbPositionM` as blue_beamPos5_horizontal, `rbpm.b-g5-bvx.avgOrbPositionM` as blue_beamPos5_vertical, `rbpm.b-g6-bhx.avgOrbPositionM` as blue_beamPos6_horizontal, `rbpm.b-g6-bvx.avgOrbPositionM` as blue_beamPos6_vertical, `rbpm.y-g5-bhx.avgOrbPositionM` as yellow_beamPos5_horizontal, `rbpm.y-g5-bvx.avgOrbPositionM` as yellow_beamPos5_vertical, `rbpm.y-g6-bhx.avgOrbPositionM` as yellow_beamPos6_horizontal, `rbpm.y-g6-bvx.avgOrbPositionM` as yellow_beamPos6_vertical, `buckets.blue.filledBucketsS`, `buckets.yellow.filledBucketsS` FROM `beamPositionMonitors` WHERE beginTime < FROM_UNIXTIME(%u) ORDER BY beginTime DESC LIMIT 1", startRunTime );
	  //cout << "query = " << query << endl;
	  res = db->Query(query);
	  xnrows = res->GetRowCount();
	  nfields = res->GetFieldCount();
  } else {
	std::cout << "got beamOrbitInfo records for runNumber = " << runNumber << std::endl;
  }

    row = res->Next();
    unsigned int beginTime = startRunTime; // atoul(row->GetField(0));
	int blue_beamPos5_horizontal = atoi(row->GetField(1));
	int blue_beamPos5_vertical = atoi(row->GetField(2));
	int blue_beamPos6_horizontal = atoi(row->GetField(3));
	int blue_beamPos6_vertical = atoi(row->GetField(4));
	int yellow_beamPos5_horizontal = atoi(row->GetField(5));
	int yellow_beamPos5_vertical = atoi(row->GetField(6));
	int yellow_beamPos6_horizontal = atoi(row->GetField(7));
	int yellow_beamPos6_vertical = atoi(row->GetField(8));
	int blue_filledBuckets = atoi(row->GetField(9));
	int yellow_filledBuckets = atoi(row->GetField(10));

    delete row;
	delete res; // Delete query and database
	delete db;

	std::cout << "DATA: blue_beamPos5_horizontal = " << blue_beamPos5_horizontal << ", blue_beamPos5_vertical = " << blue_beamPos5_vertical
		<< ", blue_beamPos6_horizontal = " << blue_beamPos6_horizontal << ", blue_beamPos6_vertical = " << blue_beamPos6_vertical
		<< ", yellow_beamPos5_horizontal = " << yellow_beamPos5_horizontal << ", yellow_beamPos5_vertical = " << yellow_beamPos5_vertical
		<< ", yellow_beamPos6_horizontal = " << yellow_beamPos6_horizontal << ", yellow_beamPos6_vertical = " << yellow_beamPos6_vertical
		<< ", blue_filledBuckets = " << blue_filledBuckets << ", yellow_filledBuckets = " << yellow_filledBuckets << std::endl;

//    gSystem->Setenv("DB_ACCESS_MODE","write");
    StDbManager* mgr = StDbManager::Instance();

	std::cout << "trying to access robinson" << std::endl;

    StDbConfigNode* node = mgr->initConfig("RunLog_onl");
    StDbTable* tdT = node->addDbTable("beamOrbitInfo");

	std::cout << "saving values for runNumber: " << runNumber << std::endl;

    beamOrbitInfo_st values;

    values.runNumber = runNumber;
	values.blue_beamPos5_horizontal = blue_beamPos5_horizontal;
	values.blue_beamPos5_vertical = blue_beamPos5_vertical;
	values.blue_beamPos6_horizontal = blue_beamPos6_horizontal;
	values.blue_beamPos6_vertical = blue_beamPos6_vertical;
	values.yellow_beamPos5_horizontal = yellow_beamPos5_horizontal;
	values.yellow_beamPos5_vertical = yellow_beamPos5_vertical;
	values.yellow_beamPos6_horizontal = yellow_beamPos6_horizontal;
	values.yellow_beamPos6_vertical = yellow_beamPos6_vertical;
	values.blue_filledBuckets = blue_filledBuckets;
	values.yellow_filledBuckets = yellow_filledBuckets;

    tdT->SetTable((char*)&values,1);
    mgr->setStoreTime(beginTime);
    int value = mgr->storeDbTable(tdT);

}
