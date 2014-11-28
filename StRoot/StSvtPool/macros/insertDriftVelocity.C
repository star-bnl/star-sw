//
// macro to transfer data from a ASCII file to data base
//

#define NUMBER_OF_HYBRIDS 432
#define MEAN_VELOC 695934
#include <fstream.h>
#include "/afs/rhic/star/packages/DEV/StRoot/StDbLib/StDbDefs.hh"

void insertDriftVelocity(char* unixTime = 0, Bool_t write = kFALSE)
{
  // DB-specific libs
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker"); 
      
  ifstream file("driftVeloc_01_23_04.txt");
  ofstream fileout("outTest.txt");

  float veloc, time, sum;
  int index, i=0;
  // define IDs
  int* rowIDs=new int[NUMBER_OF_HYBRIDS];
  // define new table to be stored
  svtDriftVelAvg_st *driftVelocity = new svtDriftVelAvg_st[NUMBER_OF_HYBRIDS];    

  while (kTRUE) {

    // read drift velocity file
    file >> index >> veloc >> time;

    if (file.eof()) break;

    fileout  << index << "  " <<  veloc << "  " <<  time << endl;
 
    if ((veloc > MEAN_VELOC*1.50) || (veloc < MEAN_VELOC*0.50)) {
      cout << "Bad drift velocity : " << veloc << endl;
      veloc = MEAN_VELOC;
    }
    else {
      sum += veloc;
      i++;
    }
    rowIDs[index]=index;
    driftVelocity[index].averageDriftVelocity = veloc;
   
  }

  /*
  for (int i=0;i<432;i++) {
    rowIDs[i]=i;
    driftVelocity[i].averageDriftVelocity = 675000;
  }
  */

  cout << "Mean drift velocity : " << sum/i << endl;

  if (write) {
    StDbManager* mgr=StDbManager::Instance();
    StDbConfigNode* svtCalibNode = mgr->initConfig(dbCalibrations,dbSvt);           StDbTable* svtCalibTable = svtCalibNode->addDbTable("svtDriftVelAvg");    
    svtCalibTable->SetTable((char*)driftVelocity,NUMBER_OF_HYBRIDS,rowIDs);
    mgr->setStoreTime(unixTime);
    cout<<" Will attempt store with timestamp="<<mgr->getDateStoreTime()<<endl;    mgr->storeDbTable(svtCalibTable);
    delete [] driftVelocity;
  }

}
