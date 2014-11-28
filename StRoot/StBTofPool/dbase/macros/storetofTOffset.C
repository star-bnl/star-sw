// $Id: storetofTOffset.C,v 1.2 2014/11/24 22:18:54 geurts Exp $
// macro to upload tofTOffset tables to database
//
// based on http://www.star.bnl.gov/STAR/comp/db/StoreDbTable.cc.html
//
// Xin Dong, 06/04/2008 
// ---
// $Log: storetofTOffset.C,v $
// Revision 1.2  2014/11/24 22:18:54  geurts
// Add striciter protection against non-existing files (bail out), and reduce excessive std output
//
// Revision 1.1  2010/12/14 19:27:28  geurts
// *** empty log message ***
//
// ---


#include <iostream>
#include <fstream>
#include <string>
using namespace std;

void storetofTOffset(const Bool_t mTest = 1)
{

  const Int_t mNTray = 120;
  const Int_t mNTOF = 192;
  const Int_t mNModule = 32;
  const Int_t mNCell = 6;

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
//  TString StoreTime = "2008-02-01 00:00:03";
//  TString StoreTime = "2008-03-04 16:00:01";
//  TString StoreTime = "2009-03-15 00:00:00";
//  TString StoreTime = "2009-11-01 00:00:00";  // 200 GeV preliminary
//  TString StoreTime = "2010-03-18 18:00:00";   // 62 GeV preliminary
//  TString StoreTime = "2010-04-08 15:00:00";   // 39 GeV preliminary
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
  TString StoreTime = time;
  cout << " Store Time " << StoreTime.Data() << endl;

  //-- add table to the container with descriptor given by Database
  StDbTable* tofTOffset = configNode->addDbTable("tofTOffset");

  //-- fill structures & store times
  tofTOffset_st *tZero = new tofTOffset_st[mNTray];

  // read in tzero file by file.

  ifstream inData;
  inData.open("input/t0_4DB.dat");
  if (!inData.is_open()){
    cerr <<" unable to open input/t0_4DB.dat; bailing out ..." << endl;
    exit(-1);
  }

  for (Int_t i=0;i<mNTray;i++) {
    for (Int_t j=0;j<mNModule;j++) {
      for (Int_t k=0;k<mNCell;k++) {
	short trayId, moduleId, cellId;
	inData>>trayId>>moduleId>>cellId;
	
	double t0;
	inData >> t0;
	tZero[i].trayId = (Short_t)trayId;
	tZero[i].T0[j*mNCell+k] = (Float_t)t0;
      }
    }
  }
  inData.close();

  // test
  ofstream outData;
  outData.open("TOffset_test.dat");
   for(int i=0;i<mNTray;i++) {
     for(int j=0;j<mNModule;j++) {
       for(int k=0;k<mNCell;k++) {
         short trayId, moduleId, cellId;
         trayId = tZero[i].trayId;
         moduleId = j+1;
         cellId = k+1;
         int index = j*mNCell+k;
 	 outData << " " << trayId << " " << moduleId << " " << cellId << endl;
 	 outData << tZero[i].T0[index] << endl;
       }
     }
   }
   outData.close();

  if(!mTest) {
    cout<<" prepare to upload data to DB"<<endl;
    //- store data in table
    tofTOffset->SetTable((char*)tZero, mNTray);
    //- set store time
    dbManager->setStoreTime(StoreTime.Data());
    //- store table in dBase
    cout<<" here "<<endl;
    dbManager->storeDbTable(tofTOffset);
    cout<<"uploaded"<<endl;
    //    return 0;
  }
}
