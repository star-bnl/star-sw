#include <iostream>
#include <fstream>
#include <set>

using namespace std;

void RunElectronAnalysis(Int_t nEntries = 1e5,
			 const Char_t* listname="test.list",
			 const Char_t* mipFile="/star/u/jkadkins/bemcCalib2012/StRoot/StEmcOfflineCalibrationMaker/macros/mip.gains",
			 const Char_t* geantFile="/star/u/jkadkins/bemcCalib2012/StRoot/StEmcOfflineCalibrationMaker/macros/geant_fits.root",
			 const Char_t* outfile="testElectronFile.root")
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

  TChain *ElectronChain = new TChain("calibTree");
  Char_t file[300];
  ifstream filelist(listname,ifstream::in);
  while(1){
    filelist >> file;
    if (!filelist.good()) break;
    cout << file << endl;
    ElectronChain->Add(file);
  }

  /* Since we're not using MuDst files, the StEmcADCtoEMaker class fails to
     make BEMC detector. We are only using this for access to the BEMC tables,
     so we turn off this annoying message with these two lines */
  StMessMgr *msg = StMessMgr::Instance();
  msg->SwitchOff("Could not make BEMC detector"); 

  St_db_Maker *dbMaker = new St_db_Maker("StarDb","MySQL:StarDb");
  dbMaker->SetDateTime(20120501,200000);
  StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();

  StEmcOfflineCalibrationElectronAnalysis *mipAnalysis = new StEmcOfflineCalibrationElectronAnalysis("StEmcOfflineCalibrationElectronAnalysis",outfile,mipFile,geantFile,ElectronChain);

  //Initialize chain
  chain->Init();
  cout << "Successful Init" << endl;

  //Loop over all Make() in Chain
  for (Int_t iEntry = 0; iEntry < ElectronChain->GetEntries(); ++iEntry){
    if (ElectronChain->GetEvent(iEntry) <= 0){
      cout << "BROKEN!" << endl;
      break;
    }
    
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
