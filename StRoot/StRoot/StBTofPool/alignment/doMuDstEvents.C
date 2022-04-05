#include "iostream.h"

class     StChain;
StChain  *chain=0;
class     St_db_Maker;
St_db_Maker *dbMk =0;

Int_t iEvt=0,istat=0,nEvents=0;
void doMuDstEvents(const Char_t *fileList = "test.lis",
    const Char_t *ntuplename = "test.ntuple.root")
{
  Int_t nEvents = 3000000;
  Int_t nfiles = 1000;

  //
  // First load some shared libraries we need
  //
  if (gClassTable->GetID("TTable") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("libPhysics");
  }  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gSystem->Load("StarMagField");
  gSystem->Load("StMagF");
  //  gSystem->Load("StDbUtilities");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDaqLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("St_db_Maker");

  gSystem->Load("StEvent");
  gSystem->Load("StEventMaker");
  gSystem->Load("StarMagField");

  gSystem->Load("libtpc_Tables");
  gSystem->Load("libGeom");
  gSystem->Load("St_g2t");
  gSystem->Load("geometry");
  gSystem->Load("St_geant_Maker");

  gSystem->Load("StBTofUtil");
  gSystem->Load("StBTofMatchMaker");
  gSystem->Load("StBTofNtupleMaker");

  // Handling depends on whether file is a ROOT file or XDF file
  //
  chain  = new StChain("StChain");

  St_geant_Maker *geantMk = new St_geant_Maker();
  geantMk->LoadGeometry("detp geometry y2010");
  geantMk->SetActive(kFALSE);
  //geantMk->SetDebug(1);

  StMuDstMaker *muDstMaker = new StMuDstMaker(0,0,"",fileList,"MuDst.root",nfiles);
  //muDstMaker->SetDebug(1);

  cout<<endl<<"============  Data Base ========="<<endl;
  dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");
  //dbMk->SetDebug(1);

  StBTofMatchMaker *matchMaker = new StBTofMatchMaker("btofMatch");
  //matchMaker->SetDebug(1);
  matchMaker->setMuDstIn(kTRUE);
  /// start with ideal geometry and change to setAlignFileName in second iteration
  matchMaker->setAlignFileName("/star/u/huck/TofAlignment19GeV/geomAlign1.txt");
  //matchMaker->setIdealGeometry(kTRUE);   
  matchMaker->setCalculateAlign(kTRUE);

  //
  StBTofNtupleMaker *btofNtuple = new StBTofNtupleMaker("btofNtuple", ntuplename);
  btofNtuple->SetDebug(0); 
  btofNtuple->setMuDstIn(kTRUE);

  // Initialize chain
  //
  Int_t iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  chain->PrintInfo();
  //
  // Event loop
  //
  int istat = 0, i = 1;
EventLoop: if (i <= nEvents && istat != 2) {

             cout << endl << "============================ Event " << i
               << " start ============================" << endl;

             chain->Clear();
             istat = chain->Make(i);
             if (istat == 2) 
             {cout << "Last  event processed. Status = " << istat << endl;}
             if (istat == 3) 
             {cout << "Error event processed. Status = " << istat << endl;}

             //   gObjectTable->Print();
             i++;
             goto EventLoop;
           }

           i--;
           cout<<endl<<"============================ Event "<<i<<" finish ============================"<<endl;

           //
           // Chain Finish
           //
           if (nEvents > 1) {
             chain->Finish();
           }

}



