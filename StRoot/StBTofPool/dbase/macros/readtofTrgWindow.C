// macro to read TOF Trigger Window
//
//

#include <iostream>
#include <fstream>
using namespace std;

void readtofTrgWindow(string ZReadTime = "2029-12-31 23:59:59") {

  const Int_t NTRAY = 120;
  const Int_t NVPDTRAY = 2;
  const Int_t NMAX = 120;
  const Int_t NMTD = 2;

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

  //-- add table to the container with descriptor given by Database
  StDbTable* tofTrgWindow = configNode->addDbTable("tofTrgWindow");

  dbManager->fetchDbTable(tofTrgWindow);

  cout << "version : " << tofTrgWindow->getVersion() <<endl;
  //cout << tofTrgWindow->getTableName()<<endl;
  cout << "begin data/time : "<< tofTrgWindow->getBeginDateTime() <<endl;
  cout << "end  data/time : " << tofTrgWindow->getEndDateTime() <<endl;


  tofTrgWindow_st* trg = static_cast<tofTrgWindow_st*>(tofTrgWindow->GetTable());

  if (!trg) {
    cout << " Oops ... no pointer to table. Exiting" << endl;
    erturn;
  }


  Int_t nRows = tofTrgWindow->GetNRows();
  cout << " NRows = " << nRows << endl;
  if(nRows!=(NTRAY+NVPDTRAY+NMTD)) {
    cout << " NRows doesn't match !!! " << endl;
  }

  //unsigned short cutlow[NTRAY+NVPDTRAY];
  //unsigned short cuthi[NTRAY+NVPDTRAY];
  //for (int i=0;i<NTRAY+NVPDTRAY){
  //  cutlo[i]=0;
  //  cuthi[i]=0;
  //}

  cout << "read out from database ..." << endl;
  ofstream outData;
  outData.open("TrgWindow_read.dat");
  for (int i=0;i<NTRAY+NVPDTRAY+NMTD;i++){
    cout << i << " " << trg[i].trgWindow_Min << " "<< trg[i].trgWindow_Max << endl;
    outData << i << " " << trg[i].trgWindow_Min << " "<< trg[i].trgWindow_Max << endl;
  }
 outData.close();


}
