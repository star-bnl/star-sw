#include "SqlUtils.h"
#include <iostream.h>
#include <fstream.h>
#include "TSQLServer.h"
#include "TSQLRow.h"
#include "TSQLResult.h"

unsigned int nbins;
unsigned long * unixTime = 0;
unsigned long * zdcX = 0;
unsigned long * zdcXError = 0;

unsigned int libsLoaded = 0;

void loadLibs(){
  if( !libsLoaded){
	gROOT->Macro("LoadLogger.C");
//        cout <<  "table: " << gSystem->Load("libTable.so") << endl;
   gSystem->Load("St_base.so");
	gSystem->Load("libStDb_Tables.so");
//	cout << "base: " << gSystem->Load("St_base.so") << endl;
	gSystem->Load("StDbLib.so");
	gSystem->Load("libRMySQL.so");
	libsLoaded = 1;
  }
    
}

unsigned int speciesToMassNumber(char* spec){

    if( strcmp(spec,"Au") == 0)
	return 197;
    if( strcmp(spec,"Proton") == 0)
	return 1;
    if( strcmp(spec,"d") == 0)
	return 2;
    if( strcmp(spec,"Deuteron") == 0)
	return 2;
    else
	return 0;
}

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



// Will Fill Tables for all Runs not filled in yet
void Fill_BeamInfo(){

    loadLibs();
    
    Int_t xnrows,nfields;
    unsigned int maxRunNumber;
          
    char query[600];
    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;
    
  
  ///
  // Get DATABASE RUNLOG_rhic
  db = TSQLServer::Connect("mysql://robinson.star.bnl.gov:3306/RunLog_onl","","");

  
  sprintf(query,"select max(runNumber) from beamInfo where runNumber < 27999999");
  res = db->Query(query);
  // Get Rows and Fields, should only have 1 row and two fields
  // cout << "Db ref is " << res << endl;
  // cout << "Expecting " << res->GetRowCount() << endl;

  xnrows = (res->GetRowCount());
  nfields = res->GetFieldCount();

  if(xnrows==0){
      cout << "oops" << endl;
  }

  row = res->Next();
  
  sprintf(query,"%s",row->GetField(0));

  if(query[0] == '\0' ){
      maxRunNumber = 22028023;
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

   // Get DATABASE rhicBeam
  db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3501/RunLog","","");

  sprintf(query,"select unix_timestamp(beginTime),runNumber,entryTag,blueEnergy,blueIntensity,blueLifeTime,blueBunchIntensity,blueSpecies,blueFillNumber,yellowEnergy,yellowIntensity,yellowLifeTime,yellowBunchIntensity,yellowSpecies,yellowFillNumber from beamInfo where entryTag In(0,5) and runNumber > %u order by 1 asc",maxRunNumber);

  cout << "query = " << query << endl;
  res = db->Query(query);

  xnrows = res->GetRowCount();
  nfields = res->GetFieldCount();
  unsigned int runNumber;
  unsigned int beginTime;
  unsigned int entryTag;
  unsigned int blueMassNumber;
  unsigned int yellowMassNumber;
  double blueEnergy,blueIntensity,blueLifeTime,blueBunchIntensity,blueFillNumber;
  double yellowEnergy,yellowIntensity,yellowLifeTime,yellowBunchIntensity,yellowFillNumber;
  char* blueSpecies;
  char* yellowSpecies;

  unsigned int lastRunNumber = 0;
  
  cout << "xnrows = " << xnrows << endl;
  for(int i = 0 ; i < xnrows ; i++){
      row = res->Next();
      beginTime = atoul(row->GetField(0));
      runNumber = atoul(row->GetField(1));
      entryTag  = atoul(row->GetField(2));
      blueEnergy = atod(row->GetField(3));
      
      blueIntensity = atod(row->GetField(4));
      blueLifeTime = atod(row->GetField(5));
      blueBunchIntensity = atod(row->GetField(6));
      blueSpecies = row->GetField(7);
      blueMassNumber = speciesToMassNumber(blueSpecies);
      blueFillNumber = atod(row->GetField(8));

      yellowEnergy = atod(row->GetField(9));
      yellowIntensity = atod(row->GetField(10));
      yellowLifeTime = atod(row->GetField(11));
      yellowBunchIntensity = atod(row->GetField(12));
      yellowSpecies = row->GetField(13);
      yellowMassNumber = speciesToMassNumber(yellowSpecies);
      yellowFillNumber = atod(row->GetField(14));

      cout << "RunNumber = " << runNumber << endl;
      if(runNumber != lastRunNumber){
	  write(beginTime,runNumber,entryTag,
		blueSpecies,blueMassNumber,blueEnergy,blueIntensity,
		blueLifeTime,blueBunchIntensity,blueFillNumber,
		yellowSpecies,yellowMassNumber,yellowEnergy,yellowIntensity,
		yellowLifeTime,yellowBunchIntensity,yellowFillNumber);
	  lastRunNumber = runNumber;
      }
      else{
	  cout << "Run Repeated, only writing in first entry in time" << endl;
      }
      delete row;
  }
  
  
  delete res; // Delete query and database
  delete db;
    
}
