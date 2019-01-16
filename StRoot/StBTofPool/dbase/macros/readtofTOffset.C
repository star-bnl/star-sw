// macro to read tofr5 MAP table from database
//
// based on
//  http://www.star.bnl.gov/STAR/comp/db/StoreDbTable.cc.html
//
// Xin Dong, 04/14/2010
//


// #include "StDbLib/StDbManager.hh"
// #include "StDbLib/StDbConfigNode.hh"
// #include "StDbLib/StDbTable.h"
// #include "StDbLib/StDbDefs.hh"

#include <iostream>
#include <fstream>
using namespace std;

void readtofTOffset(string ZReadTime = "2029-12-31 23:59:59")
{
  const int mNTray = 120;
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

  dbManager->setRequestTime(ZReadTime.c_str());

  StDbTable* tofTOffset = configNode->addDbTable("tofTOffset");

  dbManager->fetchDbTable(tofTOffset);

  cout<<tofTOffset->getVersion()<<endl;
  //cout<<tofTOffset->getTableName()<<endl;
  cout<<tofTOffset->getBeginDateTime()<<endl;
  cout<<tofTOffset->getEndDateTime()<<endl;



  tofTOffset_st* tZero = static_cast<tofTOffset_st*>(tofTOffset->GetTable());

  if(!tZero) {
    cout << " ahhhhhh! " << endl;
    return;
  }

  Int_t nRows = tofTOffset->GetNRows();
  cout << " NRows = " << nRows << endl;
  if(nRows!=mNTray) {
    cout << " NRows doesn't match !!! " << endl;
  }

  Double_t mTofTZero[120][32][6];
  for(int i=0;i<120;i++) {
    for(int j=0;j<32;j++) {
      for(int k=0;k<6;k++) {
	mTofTZero[i][j][k] = 0.0;
      }
    }
  }

  cout<<"Read out from DataBase-------------->"<<endl;

  ofstream outData;
  outData.open("TOffset_read.dat");
  for (Int_t i=0;i<mNTray;i++) {
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

  cout<<tofTOffset->getVersion()<<endl;
  //cout<<tofTOffset->getTableName()<<endl;
  cout<<tofTOffset->getBeginDateTime()<<endl;
  cout<<tofTOffset->getEndDateTime()<<endl;



}
