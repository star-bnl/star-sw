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


void storetofTrgWindow()
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
//  TString ZStoreTime = "2007-11-01 00:00:01";
  TString ZStoreTime = "2009-02-01 00:00:00";

  //-- add table to the container with descriptor given by Database
  StDbTable* tofTrgWindow = configNode->addDbTable("tofTrgWindow");

  //-- fill structures & store times
  tofTrgWindow_st *trg = new tofTrgWindow_st[NMAX+NVPDTRAY];

// year8
/*
  for(int i=0;i<NMAX+NVPDTRAY;i++) {
    trg[i].trgWindow_Min = 0;
    trg[i].trgWindow_Max = 0;
    if(i>=75&&i<80) {
      trg[i].trgWindow_Min = 3270 - 2775;
      trg[i].trgWindow_Max = 3390 - 2775;
    }
    if(i==120) {
      trg[i].trgWindow_Min = 3230 - 2775;
      trg[i].trgWindow_Max = 3340 - 2775;
    }
    if(i==121) {
      trg[i].trgWindow_Min = 3290 - 2775;
      trg[i].trgWindow_Max = 3400 - 2775;
    }
  }
*/
  unsigned short cutlow[122]={22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,
                        22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,
                        22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,
                        22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,22600,
                      22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,
                      22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,
                      22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,
                      22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22700,22600,22700,22700,
                      22600,22600
  };
  unsigned short cuthi[122]={22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,
                     22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,
                     22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,
                     22940,22960,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,22940,
                     23020,23020,23020,23020,23020,23020,23020,23070,23020,23020,23020,23020,23020,23020,23020,
                     23020,23020,23020,23020,23020,23020,23020,23020,23020,23020,23020,23020,23020,23020,23020,
                     23020,23020,23020,23020,23020,23020,23020,23020,23020,23040,23040,23020,23020,23020,23020,
                     23020,23020,23020,23020,23020,23020,23140,23020,23020,23020,23030,23030,23390,23020,23030,
                     23400,23400
  };
// year9
  for(int i=0;i<NMAX+NVPDTRAY;i++) {
    trg[i].trgWindow_Min = cutlow[i];
    trg[i].trgWindow_Max = cuthi[i];
    cout << " tray = " << i+1 << " min = " << trg[i].trgWindow_Min << " max = " << trg[i].trgWindow_Max << endl;
  }


  //- store data in table
  tofTrgWindow->SetTable((char*)trg, NMAX+NVPDTRAY);
  //- set store time
  dbManager->setStoreTime(ZStoreTime.Data());
  //- store table in dBase
  cout<<" here "<<endl;
  dbManager->storeDbTable(tofTrgWindow);
  cout<<"uploaded"<<endl;
      //    return 0;
}

