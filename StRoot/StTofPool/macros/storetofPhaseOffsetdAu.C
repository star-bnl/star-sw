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
using namespace std;


void storetofPhaseOffsetdAu()
//int main(int argc, char *argv[])

{

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
  TString ZStoreTime = "2008-01-25 00:00:00";

  //-- add table to the container with descriptor given by Database
  StDbTable* tofPhaseOffset = configNode->addDbTable("tofPhaseOffset");

  //-- fill structures & store times
  tofPhaseOffset_st *phaseDiff = new tofPhaseOffset_st[1];

  phaseDiff[0].T0[0] = -14722.;
  phaseDiff[0].T0[1] = -0.9196;

  //- store data in table
  tofPhaseOffset->SetTable((char*)phaseDiff, 1);
  //- set store time
  dbManager->setStoreTime(ZStoreTime.Data());
  //- store table in dBase
  cout<<" here "<<endl;
  dbManager->storeDbTable(tofPhaseOffset);
  cout<<"uploaded"<<endl;
      //    return 0;
}

