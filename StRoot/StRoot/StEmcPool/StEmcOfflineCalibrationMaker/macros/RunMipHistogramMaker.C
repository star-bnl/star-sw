#include <iostream>
#include <fstream>
#include <set>
using namespace std;

void RunMipHistogramMaker(Int_t nEntries = 1e8,
			 const Char_t* listname="test.list",
			 const Char_t* outfile="testMipFile.root")

{
  gROOT->Macro("LoadLogger.C");
  gROOT->Macro("loadMuDst.C");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDaqLib");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");//***
  gSystem->Load("StEmcSimulatorMaker");//***
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StEpcMaker");
  gSystem->Load("StDbBroker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StEmcOfflineCalibrationMaker");

  //Instantiate StChain
  StChain *chain = new StChain;

  TChain *MipChain = new TChain("calibTree");
  Char_t file[300];
  ifstream filelist(listname,ifstream::in);
  while(1){
    filelist >> file;
    if (!filelist.good()) break;
    cout << file << endl;
    MipChain->Add(file);
  }
  
  StEmcOfflineCalibrationMipAnalysis *mipAnalysis = new StEmcOfflineCalibrationMipAnalysis("StEmcOfflineCalibrationMipAnalysis",outfile,MipChain);
  
  //Initialize chain
  chain->Init();
  cout << "Successful Init" << endl;

  //Loop over all Make() in Chain
  for (Int_t iEntry = 0; iEntry < MipChain->GetEntries(); ++iEntry){
    if (MipChain->GetEvent(iEntry) <= 0)
      break;
    
    if (iEntry%10000 == 0)
      cout << "Working on event: " << iEntry << endl;
    
    chain->Clear();
    Int_t iret = chain->Make(iEntry);
    if(iret){
      cout << "Bad return code" << endl;
      break;
    }
  }
  
  chain->Finish();
}
