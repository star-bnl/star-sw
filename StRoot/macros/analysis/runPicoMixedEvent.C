/* **************************************************
 *   Run StPicoMixedEventMaker in different modes
 * --------------------------------------------------
 *   
 * Much work still, maker to produce mixed event background
 * for particle decays as used in the rest of the LBL Pico
 * Analysis Library
 *
 * --------------------------------------------------
 *  Authors:  Xin Dong        (xdong@lbl.gov)
 *            Michael Lomnitz (mrlomnitz@lbl.gov)
 *            Mustafa Mustafa (mmustafa@lbl.gov)
 *
 * **************************************************
 */

#include "TROOT.h"
#include "TSystem.h"
#include "TChain.h"


# ifndef __CINT__

#include "StMaker.h"
#include "StChain.h"

#include "StMuDSTMaker/COMMON/macros/loadSharedLibraries.C"

#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StRoot/StRefMultCorr/StRefMultCorr.h"
#include "StRoot/StRefMultCorr/CentralityMaker.h"
#include "StPicoHFMaker/StPicoHFEvent.h"

#include "StPicoMixedEventMaker/StPicoMixedEventMaker.h"
#include "StMemStat.h"

#else
class StChain;
#endif

class StMaker;
class StChain;
class StPicoDstMaker;
class StPicoMixedEventMaker;
class StRefMultCorr;
StChain *chain;

void runPicoMixedEvent(const Char_t *inputFile="test.list", const Char_t *outputFile="outputBaseName", 
			 const Char_t *badRunListFileName = "picoList_bad_MB.list") { 
  // -- Check STAR Library. Please set SL_version to the original star library used in the production 
  //    from http://www.star.bnl.gov/devcgi/dbProdOptionRetrv.pl
  StMemStat mem;
  string SL_version = "SL15c";
  string env_SL = getenv ("STAR");
  if (env_SL.find(SL_version)==string::npos) {
      cout<<"Environment Star Library does not match the requested library in runPicoMixedEventMaker.C. Exiting..."<<endl;
      exit(1);
  }
  // ========================================================================================
  //   Testing 
  // ========================================================================================
  Int_t nEvents = 10000000;
  //Int_t nEvents = 1000;
	
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  gSystem->Load("StBTofUtil");
  gSystem->Load("StPicoDstMaker");
  gSystem->Load("StPicoPrescales");
  gSystem->Load("StPicoCutsBase");
  gSystem->Load("StPicoHFMaker");
  gSystem->Load("StRefMultCorr");
  gSystem->Load("StPicoMixedEventMaker");
  
  
  chain = new StChain();

  // ========================================================================================
  // Still bone dry
  // ========================================================================================


  TString sInputFile(inputFile);
  TString sInputListHF("");  

  // ========================================================================================
  StPicoDstMaker* picoDstMaker = new StPicoDstMaker(0, sInputFile, "picoDstMaker");
  StRefMultCorr* grefmultCorrUtil  = CentralityMaker::instance()->getgRefMultCorr();
  cout<<"here"<<endl;
  grefmultCorrUtil->setVzForWeight(6, -6.0, 6.0);
  grefmultCorrUtil->readScaleForWeight("StRoot/StRefMultCorr/macros/weight_grefmult_vpd30_vpd5_Run14.txt");
  for(Int_t i=0;i<6;i++){
    cout << i << " " << grefmultCorrUtil->get(i, 0) << endl;
  }

  StPicoMixedEventMaker* picoMixedEventMaker = new StPicoMixedEventMaker("picoMixedEventMaker", picoDstMaker, grefmultCorrUtil, outputFile, sInputListHF);

  // ---------------------------------------------------
  // -- Set Base cuts for HF analysis

  // -- File name of bad run list
  //hfCuts->setBadRunListFileName(badRunListFileName);

  chain->Init();
  cout << "chain->Init();" << endl;

  int total = picoDstMaker->chain()->GetEntries();
  cout << " Total entries = " << total << endl;
  if(nEvents>total) nEvents = total;
  for (Int_t i=0; i<nEvents; i++) {
    if(i%1000==0)
      cout << "Working on eventNumber " << i << endl;
    
    chain->Clear();

    int iret = chain->Make(i);

    if (iret) { cout << "Bad return code!" << iret << endl; break;}
    
    total++;
  }
  
  cout << "****************************************** " << endl;
  cout << "Work done... now its time to close up shop!"<< endl;
  cout << "****************************************** " << endl;
  chain->Finish();
  cout << "****************************************** " << endl;
  cout << "total number of events  " << nEvents << endl;
  cout << "****************************************** " << endl;
  
  delete chain;

}

