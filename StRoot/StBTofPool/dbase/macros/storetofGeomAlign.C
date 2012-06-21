// Upload BTOF/TPC Alignment to database
//
// based on
//  http://www.star.bnl.gov/STAR/comp/db/StoreDbTable.cc.html
//
// Jing Liu, 02/18/2005 
//
#include <iostream>
#include <fstream>
#include <string>
#include <iomanip>
using namespace std;


void storetofGeomAlign() {

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
// Commit log:
//  TString ZStoreTime = "2007-11-01 00:00:00";
//  TString ZStoreTime = "2008-11-01 00:00:00";  // initial for tuning
//  TString ZStoreTime = "2008-11-01 00:00:01";  // y9
//  TString ZStoreTime = "2008-11-01 00:00:02";  // y9 - OK update on localy offset
                                                 // local z window mean shifted to 0
//  TString ZStoreTime = "2009-02-01 00:00:00";  // final numbers with RFF low L data
//  TString ZStoreTime = "2009-11-01 00:00:00";  // initial for Run10
//  TString ZStoreTime = "2009-11-01 00:00:01";  //  Run10 from low luminosity data  
//  TString ZStoreTime = "2009-12-14 00:00:02";  //  Run10 update with final TPC calib (7GeV based) 
//  TString ZStoreTime = "2009-12-14 00:00:03";  //  Run10 200GeV update for production (note: TPC sector 20 problem)
//  TString ZStoreTime = "2010-04-08 15:00:00";  //  Run10 update with final TPC calib 7GeV based) 
//  TString ZStoreTime = "2010-12-20 00:00:01";  //  Run11 update with final TPC calib (19.6GeV based) 
//  TString ZStoreTime = "2009-02-01 00:00:01";  //  Run9, alignment update following major TPC geometry update
  TString ZStoreTime = "2009-05-27 00:00:01";  //  Run9, alignment update following major TPC geometry update

//-- add table to the container with descriptor given by Database
  StDbTable* tofGeomAlign = configNode->addDbTable("tofGeomAlign");

//-- fill structures & store times
  tofGeomAlign_st *tofAlign = new tofGeomAlign_st[NMAX];

// year10+
  float shifty[NMAX], shiftz[NMAX], shiftx[NMAX];
  ifstream inData;
  inData.open("data/run9/alignment/geomAlign_200GeV_120322.txt");
  for(int i=0;i<NMAX;i++) {
    inData >> shifty[i] >> shiftz[i] >> shiftx[i];
  }
  inData.close();

  for(int i=0;i<NMAX;i++) {
    int tray = i+1;
    tofAlign[i].x0 = shiftx[i];
    tofAlign[i].phi0 = shifty[i];
    tofAlign[i].z0 = shiftz[i];
    tofAlign[i].angle0 = 0.;
    cout << " trayId=" << tray << " y0=" << tofAlign[i].phi0 << " z0=" << shiftz[i] << " x0=" << shiftx[i] << endl;
  }

  tofGeomAlign->SetTable((char*)tofAlign, NMAX);
//- set store time
  dbManager->setStoreTime(ZStoreTime.Data());
//- store table in dBase
  cout<<" here "<<endl;
  dbManager->storeDbTable(tofGeomAlign);
  cout<<"uploaded"<<endl;
}
