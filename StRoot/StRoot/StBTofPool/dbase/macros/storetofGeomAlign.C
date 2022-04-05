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
//  TString ZStoreTime = "2009-05-27 00:00:01";  //  Run9-FF, alignment update following major TPC geometry update
//  TString ZStoreTime = "2011-12-20 00:00:01";  //  Run12 update with final TPC calib (193GeV UU based, w/ TPC y2000 geometry)
//  TString ZStoreTime = "2009-02-01 00:00:01";  //  Run9-RFF, alignment update (based on FF)  following major TPC geometry update
//  TString ZStoreTime = "2012-12-20 00:00:01";  // Run-13 aligment
//  TString ZStoreTime = "2013-12-20 00:00:01";  // Run-14 aligment for FastOffline (based on Run-13)
//  TString ZStoreTime = "2013-12-20 00:00:02";  // Run-14 aligment for 14.6GeV preproduction (based on Run-14 preproduction TPC calib.)
//  TString ZStoreTime = "2013-05-09 00:00:00";  // Run-13 aligment for second half of pp510 (after pixel installation)
//TString ZStoreTime = "2013-12-20 00:00:03";  // Run-14 aligment for 14.5GeV final production (based on run-14 hltgood stream)
//TString ZStoreTime = "2018-05-22 00:00:00";  // Run-18 aligment for 27GeV

// get timestamp from the common source
ifstream inTime;
inTime.open("input/timestamp");
string time;
if (inTime.is_open()) {
  getline(inTime, time);
  inTime.close();
} else {
  cout << " Unable to open the TimeStamp file! EXIT! " << endl;
  return;
}
TString ZStoreTime = time;
cout << " Store Time " << ZStoreTime.Data() << endl;


//-- add table to the container with descriptor given by Database
  StDbTable* tofGeomAlign = configNode->addDbTable("tofGeomAlign");

//-- fill structures & store times
  tofGeomAlign_st *tofAlign = new tofGeomAlign_st[NMAX];

// year10+
  float shifty[NMAX], shiftz[NMAX], shiftx[NMAX];
  ifstream inData;
  inData.open("input/geometry.dat");
  if (!inData.is_open()){
    cerr <<" unable to open geometry.dat; bailing out ..." << endl;
    exit(-1);
  }
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
    //cout << " trayId=" << tray << " y0=" << tofAlign[i].phi0 << " z0=" << shiftz[i] << " x0=" << shiftx[i] << endl;
  }

  tofGeomAlign->SetTable((char*)tofAlign, NMAX);
//- set store time
  dbManager->setStoreTime(ZStoreTime.Data());
//- store table in dBase
  cout<<"Storing DB Table ... "<<endl;
  dbManager->storeDbTable(tofGeomAlign);
  cout<<"uploaded"<<endl;
}
