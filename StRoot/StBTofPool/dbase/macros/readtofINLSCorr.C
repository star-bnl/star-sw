// macro to read tofr5 MAP table from database
//
// based on
//  http://www.star.bnl.gov/STAR/comp/db/StoreDbTable.cc.html
//
// Jing Liu, 03/10/2005
//


// #include "StDbLib/StDbManager.hh"
// #include "StDbLib/StDbConfigNode.hh"
// #include "StDbLib/StDbTable.h"
// #include "StDbLib/StDbDefs.hh"

#include <iostream>
#include <fstream>
using namespace std;

void readtofINLSCorr(string ZReadTime = "2029-12-31 23:59:59")
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

  dbManager->setRequestTime(ZReadTime.c_str());

  StDbTable* tofinlcorr = configNode->addDbTable("tofINLSCorr");

  dbManager->fetchDbTable(tofinlcorr);

  cout<<tofinlcorr->getVersion()<<endl;
  //cout<<tofinlcorr->getTableName()<<endl;
  cout<<tofinlcorr->getBeginDateTime()<<endl;
  cout<<tofinlcorr->getEndDateTime()<<endl;

  tofINLSCorr_st* inlcorr = static_cast<tofINLSCorr_st*>(tofinlcorr->GetTable());

  const Int_t mNTDIGMAX = 1400; //1200
  const Int_t mNChanOnTDIG = 24;
  const Int_t mNChanMAX = 1024;
  const Int_t mNBoardIdMAX = 4800;
  Short_t mINLCorr[mNTDIGMAX][mNChanOnTDIG][mNChanMAX];
  Int_t mBoardId[mNTDIGMAX];

  cout<<"Read out from DataBase-------------->"<<endl;
  Int_t nRows = tofinlcorr->GetNRows();
  cout << " nRows = " << nRows << endl;
  Int_t NTdig = 0;
  Int_t tdigId_old = 0;
  for(int i=0;i<nRows;i++) {
    if(NTdig>=mNTDIGMAX) {
      cout << " exceed the limit! " << endl;
      NTdig = mNTDIGMAX;
      break;
    }

    int tdigId = (Int_t)(inlcorr[i].tdigId);
    int tdcChanId = (Int_t)(inlcorr[i].tdcChanId);
    if(tdigId!=tdigId_old) {
      mBoardId[NTdig] = tdigId;
      NTdig++;
    }

    tdigId_old = tdigId;

    for(Int_t j=0;j<mNChanMAX;j++) {
      Short_t corr = (Short_t)(inlcorr[i].INLCorr[j]);
      mINLCorr[NTdig-1][tdcChanId][j] = corr;
    }
  }

  cout << " Total # of boards read in : " << NTdig << endl;

  // list the TDIG board IDs
  for(int i=0;i<NTdig;i++){cout << mBoardId[i] << endl;}

}
