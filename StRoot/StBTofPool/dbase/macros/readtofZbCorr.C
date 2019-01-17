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
#include <string>
#include "iomanip.h"
using namespace std;

void readtofZbCorr(string ZReadTime = "2029-12-31 23:59:59")
{
  const int mNTray = 120;
  const int mNTDIG = 8;
  const int mNVPD = 19;

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

  StDbTable* tofZbCorr = configNode->addDbTable("tofZbCorr");

  dbManager->fetchDbTable(tofZbCorr);

  cout<<tofZbCorr->getVersion()<<endl;
  //cout<<tofZbCorr->getTableName()<<endl;
  cout<<tofZbCorr->getBeginDateTime()<<endl;
  cout<<tofZbCorr->getEndDateTime()<<endl;


  tofZbCorr_st* zcorr = static_cast<tofZbCorr_st*>(tofZbCorr->GetTable());

  if(!zcorr) {
    cout << " ahhhhhh! " << endl;
    return;
  }

  cout<<"Read out from DataBase-------------->"<<endl;

  Int_t nRows = tofZbCorr->GetNRows();
  cout << " NRows = " << nRows << endl;
 // if(nRows!=mNTray*mNTDIG) {
 //   cout << " nRows doesn't match !!! " << endl;
 // }

  ofstream outData;
  outData.open("zCorr_read.dat");
  // for(int i=0;i<mNTray*mNTDIG;i++) {
 for(int i=0;i<nRows;i++) {
   outData << setw(6) << zcorr[i].trayId << setw(6) << zcorr[i].moduleId << setw(6) << zcorr[i].cellId << endl;
    for(int j=0;j<60;j++) {
      if(fabs(zcorr[i].z[j])<1.e-4 && fabs(zcorr[i].corr[j])<1.e-4) continue;
      outData << setw(15) << zcorr[i].z[j];
    }
    outData << endl;
    for(int j=0;j<60;j++) {
      if(fabs(zcorr[i].z[j])<1.e-4 && fabs(zcorr[i].corr[j])<1.e-4) continue;
      outData << setw(15) << zcorr[i].corr[j];
    }
    outData << endl;
  }
  outData.close();

}
