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

void readtofTDIGOnTray(string ZReadTime = "2029-12-31 23:59:59")
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

  StDbTable* tofmaptable = configNode->addDbTable("tofTDIGOnTray");

  dbManager->fetchDbTable(tofmaptable);

  cout<<tofmaptable->getVersion()<<endl;
  //cout<<tofmaptable->getTableName()<<endl;
  cout<<tofmaptable->getBeginDateTime()<<endl;
  cout<<tofmaptable->getEndDateTime()<<endl;

  tofTDIGOnTray_st* maptable = static_cast<tofTDIGOnTray_st*>(tofmaptable->GetTable());

  cout<<"Read out from DataBase-------------->"<<endl;
  Int_t nRows = tofmaptable->GetNRows();
  cout << " nRows = " << nRows << endl;
  for(int i=0;i<nRows;i++) {
    short trayId = maptable[i].trayId;
    for(int j=0;j<8;j++) {
      cout << maptable[i].tdigId[j] << " ";
    }
    cout << endl;
  }

}
