#include "iostream.h"

class     StChain;
StChain  *chain=0;
class     St_db_Maker;
St_db_Maker *dbMk =0;

Int_t iEvt=0,istat=0,nEvents=0;
void doEvents(const Char_t *inname = "/star/institutions/lbl/dongx/tof/Run8/prod/9054010/reco/st_toftpx_9054010_raw_1370001.event.root",
              const Char_t *histname = "test.hist.root",
              const Char_t *ntuplename = "test.ntuple.root");
// ------------------ Here is the actual method -----------------------------------------
void doEvents(const Char_t *inname, const Char_t *histname,  const Char_t *ntuplename)
{
  Int_t nEvents = 10;
  Int_t nfiles = 100;

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
  gSystem->Load("StTpcDb");
//  gSystem->Load("StDbUtilities");
  gSystem->Load("StDaqLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("St_db_Maker");

  gSystem->Load("StEvent");
  gSystem->Load("StEventMaker");
  gSystem->Load("StarMagField");
  gSystem->Load("StTofUtil");
  gSystem->Load("StTofMaker");
 
  gSystem->Load("libtpc_Tables");
  gSystem->Load("libGeom");
  gSystem->Load("St_g2t");
  gSystem->Load("geometry");
  gSystem->Load("St_geant_Maker");

  gSystem->Load("StTofrMatchMaker");
  //    gSystem->Load("StTofpMatchMaker");
  //    gSystem->Load("StTofCalibMaker");
  //    gSystem->Load("StTofFlowMaker");
  gSystem->Load("StTofrNtupleMaker");

    // Handling depends on whether file is a ROOT file or XDF file
    //
    chain  = new StChain("StChain");
    
    StIOMaker* ioMaker = new StIOMaker();
    ioMaker->SetFile(inname);
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");
    ioMaker->SetBranch("eventBranch",0,"r");
    ioMaker->SetDebug(0);
  
    int NwGeant=5000000, IwType=0, NwPaw=0;
    St_geant_Maker *geantMk = new St_geant_Maker("geant",NwGeant,NwPaw,IwType);
    geantMk->LoadGeometry("detp geometry y2008");
    geantMk->SetActive(kFALSE);

   cout<<endl<<"============  Data Base ========="<<endl;
   dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");

   StTofrMatchMaker *matchMaker = new StTofrMatchMaker("tofrMatch");
   matchMaker->SetDebug(0);
   matchMaker->SetMode(1);
   matchMaker->setSaveGeometry(kTRUE);
   matchMaker->setCreateHistoFlag(kTRUE);
   matchMaker->setCreateTreeFlag(kTRUE);
   matchMaker->setHistoFileName(histname);
   //matchMaker->setNtupleFileName("test2.root");
   StTofrNtupleMaker *tofrNtuple = new StTofrNtupleMaker("tofrNtuple", ntuplename);
   tofrNtuple->SetDebug(0); 
   tofrNtuple->setInitGeomFromOther(kTRUE);
    //

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
