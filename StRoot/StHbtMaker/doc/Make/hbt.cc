#include <iostream>

// Hbt stuff
#include "StHbtMaker.h"
#include "StHbtManager.h"
#include "StHbtAnalysis.h"
#include "franksTrackCut.h"
#include "trackCutMonitor_P_vs_Dedx.h"
#include "mikesEventCut.h"
#include "mikesPairCut.h"
#include "StHbtAsciiReader.h"
#include "StHbtBinaryReader.h"
#include "MinvCorrFctn.h"

void wait(int n=1) {
  for ( int i=0; i<n*1e6; i++) { /*no-op*/ }
}
void mess(const char* c="alive") {
  for ( int i=0; i<10; i++) { cout << c << endl; }
}


//==========================================================================================
#ifdef __ROOT__
int hbt(int argc, char* argv[]) {
#else
int main(int argc, char* argv[]) {
#endif

  int nevents;
  char* fileType;
  char* fileName;

  switch (argc) {
  case 4:
    nevents = atoi(argv[1]);
    fileType=argv[2];
    fileName=argv[3];
    break;
  default: 
    cout << "usage: hbt nevents asc/bin filename" << endl;
    return -1;
  }
  cout << " nevents =  " << nevents << endl;
  cout << " fileType = " << fileType << endl;
  cout << " fileName = " << fileName << endl;

  char* fileAppendix = fileName+strlen(fileName) -4 ;
  cout << " fileAppendix = " << fileAppendix << endl;
  // *********
  // Hbt Maker
  // *********
    StHbtMaker* hbtMaker = new StHbtMaker("HBT","title");
  cout << "StHbtMaker instantiated"<<endl;
  // -------------- set up of hbt stuff ----- //
  cout << "StHbtMaker::Init - setting up Reader and Analyses..." << endl;
  StHbtManager* TheManager = hbtMaker->HbtManager();
  
  // ***********************
  // setup HBT event readers   
  // ***********************
  StHbtAsciiReader* ascReader;
  StHbtBinaryReader* binReader;
  if ( !strcmp(fileType,"asc") ) {
    ascReader = new StHbtAsciiReader;
    ascReader->SetFileName(fileName);
    TheManager->SetEventReader(ascReader);
  } 
  else if ( !strcmp(fileType,"bin") ) {
    binReader = new StHbtBinaryReader(0,fileName,0);
    cout << " now parse files " << endl;
    /*
    if ( !strcmp(fileAppendix,".lis") ) {
      binReader->AddFileList(fileName);
      cout << " file list added " << endl;
    }
    else { 
      binReader->SetFileName(fileName);
      cout << " file name set " << endl;
    }
    */
  TheManager->SetEventReader(binReader);
  }
  else {
    cout << "unknown fileType : " << fileType  << endl;
    return -2;
  }
  cout << "READER SET UP.... " << endl;
  
  // define example particle cut and cut monitors to use in the analyses
  // example particle cut
  franksTrackCut* aParticleCut = new franksTrackCut;  // use "frank's" particle cut object
  aParticleCut->SetNSigmaPion(+3.0,1.e5);   // number of Sigma in TPC dEdx away from nominal pion dEdx
  aParticleCut->SetNSigmaKaon(-2.,2.);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
  aParticleCut->SetNSigmaProton(-1.e5,-1.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
  aParticleCut->SetNHits(5,50);            // range on number of TPC hits on the track
  aParticleCut->SetP(0.23,1.0);              // range in P
  aParticleCut->SetPt(0.0,2.0);             // range in Pt
  aParticleCut->SetRapidity(-1.5,1.5);      // range in rapidity
  aParticleCut->SetDCA(0,2.);             // range in Distance of Closest Approach to primary vertex
  aParticleCut->SetCharge(+1);              // want positive kaons
  aParticleCut->SetMass(0.494);             // kaon mass
  // example cut monitor
  trackCutMonitor_P_vs_Dedx* aDedxMoniPos = new trackCutMonitor_P_vs_Dedx(+1,"P_vs_Dedx +","Momentum (GeV/c) vs Energy loss (a.u.)",
									  100,0.,1.2,100,0.,1e-5);
  trackCutMonitor_P_vs_Dedx* aDedxMoniNeg = new trackCutMonitor_P_vs_Dedx(-1,"P_vs_Dedx -","Momentum (GeV/c) vs Energy loss (a.u.)",
									  100,0.,1.2,100,0.,1e-5);
  // now, we define another analysis that runs simultaneously with the previous one.
  // this one looks at K+K- correlations (so NONidentical particles) in invariant mass
  // ****************************************** // 
  // * franks phi analysis - by Frank Laue, OSU //
  // ****************************************** // 
  // 0) now define an analysis...
  StHbtAnalysis* phiAnal = new StHbtAnalysis;
  phiAnal = new StHbtAnalysis;
  // 1) set the Event cuts for the analysis
  mikesEventCut* phiEvcut = new mikesEventCut;  // use "mike's" event cut object
  phiEvcut->SetEventMult(000,100000);      // selected multiplicity range
  phiEvcut->SetVertZPos(-35.0,35.0);    // selected range of vertex z-position
  //eventCutMonitor_Mult* multMoniPass = new eventCutMonitor_Mult();
  //eventCutMonitor_Mult* multMoniFail = new eventCutMonitor_Mult();
  //phiEvcut->AddCutMonitor(multMoniPass, multMoniFail);
  phiAnal->SetEventCut(phiEvcut);          // this is the event cut object for this analsys
  // 2) set the Track (particle) cuts for the analysis
  franksTrackCut* kaonTrkcut = new franksTrackCut( *aParticleCut );  // copy from example
  // new particle cut moni
  trackCutMonitor_P_vs_Dedx* dedxMoniPosPass = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniPos);
  trackCutMonitor_P_vs_Dedx* dedxMoniPosFail = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniPos);
  kaonTrkcut->AddCutMonitor( dedxMoniPosPass, dedxMoniPosFail);
  phiAnal->SetFirstParticleCut(kaonTrkcut);  // this is the track cut for the "first" particle
  // copy second particle cut from first particle cut
  franksTrackCut* antikaonTrkcut = new franksTrackCut( *((franksTrackCut*)phiAnal->FirstParticleCut()) );  
  antikaonTrkcut->SetCharge(-1); 
  phiAnal->SetSecondParticleCut(antikaonTrkcut);  // this is the track cut for the "first" particle
  // new particle cut
  trackCutMonitor_P_vs_Dedx* dedxMoniNegPass = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniNeg);
  trackCutMonitor_P_vs_Dedx* dedxMoniNegFail = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniNeg);
  antikaonTrkcut->AddCutMonitor( dedxMoniNegPass, dedxMoniNegFail);
  // 3) set the Pair cuts for the analysis
  mikesPairCut* phiPairCut = new mikesPairCut;  // use "frank's" pair cut object
  //    phiPairCut->SetAngle(0.,180.);           // opening angle
  phiAnal->SetPairCut(phiPairCut);         // this is the pair cut for this analysis
  // 4) set the number of events to mix (per event)
  phiAnal->SetNumEventsToMix(5); 
  // ********************************************************************
  // 5) now set up the correlation functions that this analysis will make
  // ********************************************************************
  // define example Minv correlation function
  MinvCorrFctn* MinvCF = new MinvCorrFctn("Minv",150,0.95,1.25); 
  phiAnal->AddCorrFctn(MinvCF);   // adds the just-defined correlation function to the analysis
  
  TheManager->AddAnalysis(phiAnal);
  
  // now perform the event loop
  int iret=0;
  iret = hbtMaker->Init();
  for (int iev=0;iev<nevents; iev++) {
    hbtMaker->Clear();
    iret = hbtMaker->Make();
    cout << "StHbtExample -- Working on eventNumber " << iev << endl;
  } // Event Loop
  iret = hbtMaker->Finish();

 ((MinvCorrFctn*)((StHbtAnalysis*)hbtMaker->HbtManager()->Analysis(0))->CorrFctn(0))->Difference()->Draw();

 return 0;
} // main


