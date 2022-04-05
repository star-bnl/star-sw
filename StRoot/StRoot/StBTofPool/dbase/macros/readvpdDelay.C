// macro to read VPD delay
//
//

#include <iostream>
#include <fstream>
using namespace std;

void readvpdDelay(string ZReadTime = "2029-12-31 23:59:59") {

  const Int_t NVPD = 38;

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
  StDbTable* vpdDelayTable = configNode->addDbTable("vpdDelay");

  dbManager->fetchDbTable(vpdDelayTable);

  cout << "version : " << vpdDelayTable->getVersion() <<endl;
  cout << "begin data/time : "<< vpdDelayTable->getBeginDateTime() <<endl;
  cout << "end  data/time : " << vpdDelayTable->getEndDateTime() <<endl;


  vpdDelay_st* vpddelay = static_cast<vpdDelay_st*>(vpdDelayTable->GetTable());

  if (!vpddelay) {
    cout << " Oops ... no pointer to table. Exiting" << endl;
    return;
  }


  Int_t nRows = vpdDelayTable->GetNRows();
  cout << " NRows = " << nRows << endl;
  if(nRows!=1) {
    cout << " NRows doesn't match !!! " << endl;
  }


  cout << "read out from database ..." << endl;
  //ofstream outData;
  //outData.open("TrgWindow_read.dat");
  for (int i=0;i<NVPD;i++){
    cout << "tubedId = " << i+1 << " delay = " << vpddelay.delay[i] << endl;
  }


}
