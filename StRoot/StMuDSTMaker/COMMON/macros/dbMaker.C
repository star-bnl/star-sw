// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
#include "TH1.h"
#include "TChain.h"
#include "TSystem.h"
#include <iostream>

void dbMaker(const char* dbName="test.db", const char* inputList="test.lis") {
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StMagF");
  gSystem->Load("StUtilities");  // new addition 22jul99
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities"); 
  gSystem->Load("StMcEvent"); 
  gSystem->Load("StMcEventMaker"); 
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StMcAnalysisMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");
  cout << "loding done " << endl;

  StMuDebug::setLevel(0);  // switch of some debug output
  StMuDbReader* db = StMuDbReader::instance();
  db->addDb(dbName);
  StMuDebug::setLevel(0);  // switch of some debug output
  db->createDB(dbName,inputList);
}



