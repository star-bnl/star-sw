// macro to upload tofr5 INL tables to database
//
// based on
//  http://www.star.bnl.gov/STAR/comp/db/StoreDbTable.cc.html
//
// Jing Liu, 02/18/2005 
//

 
// #include "StDbLib/StDbManager.hh"
// #include "StDbLib/StDbConfigNode.hh"
// #include "StDbLib/StDbTable.h"
// #include "StDbLib/StDbDefs.hh"

#include <iostream>
#include <fstream>
#include <string>
#include <iomanip>
using namespace std;


void storetofGeomAlign()
//int main(int argc, char *argv[])

{

// year8
//  const Int_t NTRAY = 5;
// year9
  const Int_t NTRAY = 94;
  const Int_t NVPDTRAY = 2;
  const Int_t NMAX = 120;

  //-- load dBase and Table definition libraries
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("St_Tables.so");

  gSystem->Load("StDbLib.so");
  gSystem->Load("libStDb_Tables.so");

  //-- get the singleton manager
  StDbManager* dbManager = StDbManager::Instance();

  //-- connect to the db & get an empty container
  StDbConfigNode* configNode = dbManager->initConfig("Calibrations_tof");

  //----------------------------------------
  TString ZStoreTime = "2007-11-01 00:00:00";
//  TString ZStoreTime = "2008-11-01 00:00:00";

  //-- add table to the container with descriptor given by Database
  StDbTable* tofGeomAlign = configNode->addDbTable("tofGeomAlign");

  //-- fill structures & store times
  tofGeomAlign_st *tofAlign = new tofGeomAlign_st[NMAX];

// year8
  for(int i=0;i<NMAX;i++) {
    int trayId = i+1;
    tofAlign[i].x0 = 0.;
    tofAlign[i].phi0 = 0.;
    tofAlign[i].z0 = 0.;
    tofAlign[i].angle0 = 0.;
    if(trayId<76||trayId>80) continue;
    tofAlign[i].z0 = -0.5;
  }

// year9

  tofGeomAlign->SetTable((char*)tofAlign, NMAX);
  //- set store time
  dbManager->setStoreTime(ZStoreTime.Data());
  //- store table in dBase
  cout<<" here "<<endl;
  dbManager->storeDbTable(tofGeomAlign);
  cout<<"uploaded"<<endl;
      //    return 0;
}
