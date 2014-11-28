// macro fills the tofTrayConfig
//  - tofTrayConfig :  1 rows ---- first time period (year 3)
//
// based on
//  http://www.star.bnl.gov/STAR/comp/db/StoreDbTable.cc.html
//
// Xin Dong Nov. 2003 
//

#include <iostream>
#include <fstream>
#include <string>
#include "iomanip.h"
using namespace std;


void storeTofTrayConfig()
{
  //-- load dBase and Table definition libraries
  // Baseline shared libraries
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
  //-- fill the tofConfig structures (year 3)
  //  TString storeTime = "2002-11-10 00:00:03";
  //year4
  //  TString storeTime = "2003-11-15 00:00:01";
  //year5
  //  TString storeTime = "2004-11-15 00:00:01";
  //year7
  //  TString storeTime = "2006-11-15 00:00:01";
  //year8
  //  TString storeTime = "2007-11-15 00:00:01";
  //year9
  //  TString storeTime = "2008-11-01 00:00:00";
  //year10
  TString storeTime = "2009-11-01 00:00:00";

  //-- add table to the container with descriptor given by Database
  StDbTable* toftrayconfig = configNode->addDbTable("tofTrayConfig");

  //-- fill structures & store times
  // year3
//   const Int_t ntrays = 1;
//   Short_t iTray[ntrays] = {83};
//   Short_t nModules[ntrays] = {20};
  // year4
//   const Int_t ntrays = 1;
//   Short_t iTray[ntrays] = {83};
//   Short_t nModules[ntrays] = {20};
  // year5
//   const Int_t ntrays = 1;
//   Short_t iTray[ntrays] = {93};
//   Short_t nModules[ntrays] = {32};
  // year7
//   const Int_t ntrays = 0;
//   Short_t iTray[ntrays] = {};
//   Short_t iTray[ntrays] = {};
  // year8
//  const Int_t ntrays = 5;
//  Short_t iTray[ntrays] = {76,77,78,79,80};
//  Short_t nModules[ntrays] = {32,32,32,32,32};
  // year9
/*
  const Int_t ntrays = 94;
  Short_t iTray[ntrays] = { 1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
                           11, 12,         15, 16, 17, 18, 19, 20,
                           21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
                           31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
                           41,         44, 45, 46, 47, 48, 49, 50,
                           51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
                           61, 62, 63, 64, 65, 66, 67, 68, 69, 70,
                           71, 72,         75, 76, 77, 78,

                                                   97, 98, 99, 100,
                          101,           104, 105, 106, 107, 108, 109, 110,
                          111, 112, 113, 114, 115, 116, 117, 118, 119, 120};
  Short_t nModules[ntrays];
  for(int i=0;i<ntrays;i++) {
    nModules[i] = 32;
  }
*/
  // year10
  const Int_t ntrays = 120;
  Short_t iTray[ntrays];
  Short_t nModules[ntrays];
  for(int i=0;i<ntrays;i++) {
    iTray[i] = i+1;
    nModules[i] = 32;
  }

  tofTrayConfig_st *toftrayconf = new tofTrayConfig_st[1];

  toftrayconf[0].entries = ntrays;
  for(int i=0;i<120;i++) {
    if(i<ntrays) {
      toftrayconf[0].iTray[i] = iTray[i];
      toftrayconf[0].nModules[i] = nModules[i];
    } else {
      toftrayconf[0].iTray[i] = 0;
      toftrayconf[0].nModules[i] = 0;
    }
  }

  //- store data in table
  toftrayconfig->SetTable((char*)toftrayconf, 1);
  //- set store time
  dbManager->setStoreTime(storeTime.Data());
  //- store table in dBase
  dbManager->storeDbTable(toftrayconfig);

}
