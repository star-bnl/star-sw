////////////////////////////////////////////////////////////////
//
// macro: doMuEvent_picoFromEmbedding.C
// author: R. Debbe (2015)
//
// This macro loops over events in muDST files from Mixed embedding, runs the
// BEMC cluster finder with proper gain calibration. It also prepares track pairs
// that satisfy the UPC criterea.
// Finally it outputs a root tree ntuple that includes a branch with cluster information
// 
//
////////////////////////////////////////////////////////////////
#include "TH1.h"
#include "TH2.h"
#include "TChain.h"
#include "TSystem.h"
#include "TFile.h"
#include "iostream.h"


class StMuDstMaker;
StMuDstMaker* reader;
class StChain;
// TMemStat           memory;

StChain            *chain = 0;
int                stat = 0;
int                n = 0;
int                count = 1;



///////////////////////////////////////////////////////////////
// This is the main method in the macro.
// It loads the libraries, creates the chain and
// instantiate the makers.
// This macro works only with MuDST events 
//
// the parameters are:
//     char* list = defines the list of data files
//     Int_t nFiles = defines how many files should be read from the list
void doMuEvent_picoFromEmbedding(char* list = "./file.lis",const char* outFile)
{  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");

  loadSharedLibraries();
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");

  gSystem->Load("libGeom");
  gSystem->Load("St_g2t");
  gSystem->Load("geometry");

  gSystem->Load("St_geant_Maker");
  gSystem->Load("StBTofUtil");
  gSystem->Load("StVpdCalibMaker");
  gSystem->Load("StBTofCalibMaker");



  gSystem->Load("/star/u/ramdebbe/.sl64_gcc447/lib/StPeCMaker");
  gStyle->SetOptStat(0);


  // create chain    
  chain = new StChain("StChain"); 

  StMuDebug::setLevel(2);  
  maker = new StMuDstMaker(0,0,"",list,"",50000);


  StMuDbReader* db = StMuDbReader::instance();


  St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");
  StVpdCalibMaker *vpdCalib = new StVpdCalibMaker();
  vpdCalib->setMuDstIn();
  StBTofCalibMaker * btofCalib = new StBTofCalibMaker();
  btofCalib->setMuDstIn();

  St_geant_Maker *geantMk = new St_geant_Maker("myGeant");
  geantMk->LoadGeometry("detp geometry y2010");
  geantMk->SetActive(kFALSE);


  //instanciate StPeCMaker
  StPeCMaker* UPCmaker = new StPeCMaker(outFile);
  UPCmaker->treeFileName = outFile ;

  UPCmaker->setUseTracks(kTRUE);
  UPCmaker->setUseBemc(kTRUE);
  UPCmaker->setUseTOF(kFALSE);   //this only affect TOFtracks
  UPCmaker->setUseVertex(kTRUE);
  UPCmaker->setRomanPots(kFALSE);
  UPCmaker->setReadStMuDst(kTRUE);
  UPCmaker->setReadStEvent(kFALSE);
  UPCmaker->setReadBothInputs(kFALSE);
  UPCmaker->setTriggerOfInterest("Ignore"); //embedding doesn't have a trigger
  UPCmaker->SetDebug(10);

  chain->Init(); 


  while ( (stat==0 || stat==1)) 
  {
    chain->Clear();
    StMuDst* mudst = maker->muDst();
    UPCmaker->setMuDst(mudst);
    stat = chain->Make();
    if(n%count==0) 
    {
      cout << "Finished processing event number "<<n <<endl;

    }
    n++;
  }     
}

