// macro to read tofSimResTables from database
//
// Frank Geurts Nov. 24, 2020

#include <iostream>
#include <fstream>
#include <string>
#include "iomanip.h"
using namespace std;

void readtofSimResParams(string ZReadTime = "2029-12-31 23:59:59")
{
  const int mNTray = 120;
  const int mNCellsPerTray = 192;
  const int mNVPD = 19;
  double params[mNTray][mNCellsPerTray];

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
  StDbTable* tofSimResParams = configNode->addDbTable("tofSimResParams");
  dbManager->fetchDbTable(tofSimResParams);

  cout<<"--- This table ---"<< endl;
  cout<<"version : " << tofSimResParams->getVersion()<<endl;
  cout<<"begin data/time :" << tofSimResParams->getBeginDateTime()<<endl;
  cout<<"end data/time   :" << tofSimResParams->getEndDateTime()<<endl;

  tofSimResParams_st* table = static_cast<tofSimResParams_st*>(tofSimResParams->GetTable());
  if(!table) {
    cout << " No table? arrghhhhhh! " << endl;
    return;
  }

  cout <<"--- Read from table ---"<<endl;
  Int_t nRows = tofSimResParams->GetNRows();
  cout << " NRows = " << nRows << endl;

// store data in text file
  ofstream outData;
  outData.open("tofSimResParams.dat");

for ( int row = 0; row < nRows; row++){
  //cout << "row " << setw(6) << row << ": ";
  for ( int i = 0; i < mNTray; i++ ){ //  nTrays
    cout << ".";
	   for ( int j = 0; j < mNCellsPerTray; j++ ){
       size_t index = i * mNTray + j;
	     params[i][j] =  table[0].resolution[index];
	     outData  << setw(6) << i << setw(6) << j << setw(6) << table[0].resolution[index]  << endl;
     }
   }
   //cout << endl;
}
  outData.close();


}
