// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
#include "TH1.h"
#include "TChain.h"
#include "TSystem.h"
#include <iostream>

void dbMaker(const char* dbName="test.db", const char* inputList="test.lis") {
    gROOT->LoadMacro("StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << "loding done " << endl;

  StMuDebug::setLevel(0);  // switch of some debug output
  StMuDbReader* db = StMuDbReader::instance();
  db->addDb("dAu200.db");
  db->addDb(dbName);
  StMuDebug::setLevel(0);  // switch of some debug output
  db->createDB(dbName,inputList);
}



