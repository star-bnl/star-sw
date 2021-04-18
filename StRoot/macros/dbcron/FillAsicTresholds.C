#include "SqlUtils.h"
#include <iostream.h>
#include <fstream.h>
#include "TSQLServer.h"
#include "TSQLRow.h"
#include "TSQLResult.h"

unsigned int libsLoaded = 0;

void loadLibs() {
  if( !libsLoaded ) {
	gROOT->Macro("LoadLogger.C");
        gSystem->Load("St_base.so");
	gSystem->Load("libStDb_Tables.so");
	gSystem->Load("StDbLib.so");
	gSystem->Load("libRMySQL.so");
	libsLoaded = 1;
  }    
}

void write(int nrows, void* data, unsigned int* times) {

  asic_thresholds_st* gvalues = (asic_thresholds_st*)data;
  
  cout<<" ****** Filling "<<nrows<<" rows of data ****** "<<endl;
  
  for ( int i = 0; i < nrows; i++ ) {

    StDbManager* mgr = 0;
    mgr = StDbManager::Instance();
    if (!mgr) {
	std::cout << "**** ERROR: Cannot get StDbManager, something is deeply wrong! \n";
	return;
    }
    StDbConfigNode* node = 0;
    node = mgr->initConfig("Calibrations_tpc");
    if (!node) {
	std::cout << "**** ERROR: Cannot get StDbConfigNode for Calibrations_tpc, something is deeply wrong! \n";
	return;
    }
    StDbTable* table = 0;
    table = node->addDbTable("asic_thresholds");
    if (!table) {
	std::cout << "**** ERROR: Cannot get StDbTable for Calibrations_tpc/asic_thresholds, something is deeply wrong! \n";
	return;
    }

    mgr->setStoreTime( times[i] );
    
    table->SetTable( (char*)&gvalues[i], 1 );
    
    if ( !mgr->storeDbTable(table) ) {
      cout<<"**** ERROR: Cannot store Calibrations_tpc/asic_thresholds table for Time = " << mgr->getDateStoreTime() << endl;
      return;
    }

    delete node;
  }

}


int MigrateData() {

  char query[256];
  int numRowsPerCall=24;
  unsigned int timeStep=1200; // 20 minute averages
  unsigned int currentTime=(unsigned int)time(NULL);
  unsigned int startRecordTime = 1072656000;// Dec 28, 2003 rhic feed is on
  StDbManager* mgr = 0;
  mgr = StDbManager::Instance();

  //
  //-> get timestamp for last entry int offline db
  //
  StDbConfigNode* offlNode = 0;
  offlNode = mgr->initConfig("Calibrations_tpc");
  if (!offlNode) { return 10; }
  
  StDbTable* offlT=offlNode->addDbTable("asic_thresholds"); 
  if (!offlT) { return 12; }
  
  mgr->setRequestTime(time(NULL));
  
  unsigned int beginTime = 0;
  if (mgr->fetchDbTable(offlT)) { 
    beginTime = offlT->getBeginTime(); 
  }
  if ( (currentTime - beginTime) < timeStep) { 
    return -1;
  }
  if ( beginTime < startRecordTime ) { 
    beginTime = startRecordTime; 
  }

  // 
  //-> connect  get table schema to/from onl db
  //
  StDbConfigNode* onlNode = 0;
  onlNode = mgr->initConfig("Conditions_rts");
  if ( !onlNode ) { return 16; }
  
  StDbTable* onlT = 0;
  onlT = onlNode->addDbTable("dets");
  if ( !onlT ) { return 18; }

  // prepare memory for storing data
  asic_thresholds_st* offlfield = new asic_thresholds_st[numRowsPerCall]; 
  memset(offlfield, 0, numRowsPerCall*sizeof(asic_thresholds_st));
  int row = 0;
  unsigned int* bTimes = new unsigned int[numRowsPerCall];
  memset(bTimes, 0, numRowsPerCall*sizeof(unsigned int));

  // db helper code special to this type of process
  StDataBaseI* dbI = mgr->findDb("Conditions_rhic"); 
  if (!dbI) { return 20; }

  bool done = false;
  int signMask = 1<<15;
  while ( !done ) {

    unsigned int endTime = beginTime + timeStep; // 20 minutes
    sprintf(query," WHERE beginTime > FROM_UNIXTIME(%u) AND beginTime <= FROM_UNIXTIME(%u) ", beginTime, endTime);

    unsigned int* tmpTimes = 0;
    tmpTimes = dbI->QueryDbTimes(onlT, query);

    if(tmpTimes){
      
      int tmpRows = onlT->GetNRows();
      int irow;
      if ( (row + tmpRows) > numRowsPerCall) { 
        tmpRows = numRowsPerCall - row;
      }

      for (int irow = 0; irow < tmpRows; irow++) {
 
        float current = *(float*)onlT->getDataValue("mainMagnetCurrent", irow);         
	int mstatus   = *(int*)onlT->getDataValue("mainMagnetStatus", irow);
        float signoffield = 1.0;
        if (mstatus & signMask) { 
	    signoffield=-1.0;
	}
        offlfield[row+irow].ScaleFactor = signoffield*floor(1000*(fabs(current)/4500.0))/1000.0;
        bTimes[row+irow] = tmpTimes[irow];        

      }   

      row += tmpRows;
    }
    beginTime = endTime;
    if( (row >= numRowsPerCall) || ((beginTime + timeStep) > currentTime)) {
	done=true;
    }

  }

  write(row, offlfield, bTimes);

  delete offlNode;
  delete onlNode;

  return 0;
};
  

void FillAsicTresholds() {

  loadLibs();

  int retVal = MigrateData();

    if ( retVal == -1 ) {
      std::cout << "Result:: Nothing to be Done \n";
    } else if ( retVal == 0 ) {
      std::cout << "Result:: Success \n";
    } else if ( retVal <15 ) {
      std::cout << "Result:: Error accessing offl db data \n";
    } else if ( retVal <21 ) {
      std::cout << "Result:: Error accessing onl db data \n"; 
    }

};






