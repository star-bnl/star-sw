// macro to upload pvpd delay 
//

#include <iostream>
#include <fstream>
#include <string>
#include <iomanip>
using namespace std;


void storevpdDelay(const Bool_t mTest = 1)
{
  const Int_t NVPD=38;

//----- default delays before Run-9 500GeV finished (run<10107025)
//   float delay[NVPD];
//   memset(delay,0,sizeof(delay));
//   TString ZStoreTime = "2009-01-01 00:00:00"; // start of Run-9
//------


//----- default delays for run9 200 GeV
//   float delay[NVPD]={
//     0,-0.564753,-4.62291,-4.84402,-4.05943,6.32389,-9.4035,-10.3113,-17.0374,-17.3734,-6.04608,-11.9614,-12.7579,8.79609,3.8467,-17.2994,-17.6424,-21.4749,-22.9736,
//     0,-2.1707,  -4.8195, -6.5161, -4.3109, 6.3116, -8.8655,-10.1037,-16.5970,-17.9588,-5.2079, -12.1249,-12.2412,8.4001, 5.5702,-16.5936,-16.4152,-21.3076,-21.1452
//   };

//----- Run 9: 1st set adjusted delay due to trigger timing drift in some boards
//-----        runs 10107025 - 10154044 
//     delay[6]  -= 25.0;
//     delay[7]  -= 25.0;
//     delay[17] -= 25.0;
//     delay[18] -= 25.0;
//     delay[25] -= 25.0;
//     delay[26] -= 25.0;
//     delay[36] -= 25.0;
//     delay[37] -= 25.0;
//   TString ZStoreTime = "2009-04-17 22:30:00";  // start of run 10107025
//------

//----- Run-9: 2nd set, back to default delays
//-----        runs 10154045 - 10313999
//     TString ZStoreTime = "2009-06-03 16:47:00";  // start of run 10154045
//------


//----- Run 10 (valid for all energies, based on 39GeV)
  float delay[NVPD]={
    0.0,       -1.201867,  -0.531776,  -5.477558,  -6.167743,  -5.575552,  -9.801250, -10.958965, -10.591384,  -4.773926,
   -10.268874, -14.588154,  -4.931171,  -4.488882,  -9.838172, -10.241304,  -9.646428, -19.994028, -14.213135,
    0.0,       -0.668213,  -0.479492,  -4.870962,  -6.054838,  -5.567796, -10.370061, -10.805627, -11.034330,  -3.716928,
   -10.197979, -15.191838,  -5.421011,  -5.035720, -10.611328, -10.291655, -10.088323, -20.136475, -14.605093
  };
  TString ZStoreTime = "2009-12-14 00:00:00";  // Run-10 official start of run timestamp
//-----

//---------------------------------------------------------------------------------------------

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

  //-- add table to the container with descriptor given by Database
  StDbTable* vpdDelayTable = configNode->addDbTable("vpdDelay");

  //-- fill structures & store times
  vpdDelay_st vpddelay;

  for(int i=0;i<NVPD;i++) {
    vpddelay.delay[i] = delay[i];
    cout << " tubeId = " << i+1 << " delay = " << vpddelay.delay[i] << endl;
  }

if (!mTest){
  //- store data in table
  vpdDelayTable->SetTable((char*)&vpddelay, 1);
  //- set store time
  dbManager->setStoreTime(ZStoreTime.Data());
  //- store table in dBase
  cout<<" here "<<endl;
  dbManager->storeDbTable(vpdDelayTable);
  cout<<"uploaded"<<endl;
 }
}
