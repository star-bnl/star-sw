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

void readvpdTotCorr(string ZReadTime = "2029-12-31 23:59:59")
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

  StDbTable* vpdTotCorr = configNode->addDbTable("vpdTotCorr");

  dbManager->fetchDbTable(vpdTotCorr);

  cout<<vpdTotCorr->getVersion()<<endl;
  //cout<<vpdTotCorr->getTableName()<<endl;
  cout<<vpdTotCorr->getBeginDateTime()<<endl;
  cout<<vpdTotCorr->getEndDateTime()<<endl;


  vpdTotCorr_st* totcorr = static_cast<vpdTotCorr_st*>(vpdTotCorr->GetTable());

  if(!totcorr) {
    cout << " ahhhhhh! " << endl;
    return;
  }

  cout<<"Read out from DataBase-------------->"<<endl;

  Int_t nRows = vpdTotCorr->GetNRows();
  cout << " NRows = " << nRows << endl;
  if(nRows!=2*mNVPD) {
    cout << " NRows doesn't match !!! " << endl;
  }

  ofstream outData;
  outData.open("vpdTotCorr_read.dat");
  for(int i=0;i<2*mNVPD;i++) {
    outData << setw(6) << totcorr[i].tubeId << endl;
    cout << totcorr[i].tubeId << " " << totcorr[i].corralgo << endl;
    for(int j=0;j<128;j++) {
      if(totcorr[i].tot[j]>1.e-4)
       outData << setw(15) << totcorr[i].tot[j];
    }
    outData << endl;
    for(int j=0;j<128;j++) {
      if(totcorr[i].tot[j]>1.e-4)
        outData << setw(15) << totcorr[i].corr[j];
    }
    outData << endl;
  }
  outData.close();

}
