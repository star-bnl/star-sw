#include "StRoot/StDbLib/Calibrations/tpcDriftVelocity.h"
//
void DbTest(){
  gSystem->Load("St_base"); 
  gSystem->Load("StChain");  
  StChain* mchain = new StChain("StDbTest");
  gSystem->Load("StDbLib.so"); 
  gSystem->Load("StDbMaker.so");  
//
// Construct with configuration="dbtest1"
// that is a key in the StarDb::StarDbKeys table
//
  StDbMaker* mk = new StDbMaker("dbtest1","StarDb");
  mk->SetTime(1);
  mk->Init();
  mk->Make();
//
// could do this with an St_DataSetIter....
//
  St_DataSet* set2 = mk->GetData("StarDb");
  St_DataSet* set3 = set2->Find("Calibrations"); 
  St_DataSet* set4 = set3->Find("tpc");
  St_DataSet* set5 = set4->Find("tpcDriftVelocity");
//
// for purpose of handling with St_DataSet,
// StDb_tpcDriftVelocity inherits from TObject
// & can be put into an St_ObjectSet..
//
  StDb_tpcDriftVelocity* table=(StDb_tpcDriftVelocity*)set5->GetObject();
//
// any user code must compile with the header file
//  ... where to put it? now Calibrations subdir of StDbLib
//
  tpcDriftVelocity* tpd = table->getTable();

  cout << tpd->laserDriftVelocityEast << endl;
  cout << tpd->cathodeDriftVelocityEast << endl;
  cout << tpd->cathodeDriftVelocityWest << endl;
//
}


