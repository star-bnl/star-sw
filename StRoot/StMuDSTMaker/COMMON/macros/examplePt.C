// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
//#include "StRoot/StMuDSTMaker/COMMON/StMuTypes.hh"

#include "TH1.h"
#include "TChain.h"
#include "TSystem.h"
#include "TFile.h"
#include <iostream>

class StMuDstMaker;
StMuDstMaker* maker;


TH1D globalPt("globalPt","globalPt",100,0.,3.);
TH1D primaryPt("primaryPt","primaryPt",100,0.,3.);
TH1D l3Pt("l3Pt","l3Pt",100,0.,3.);

TH2D etaLength("etaLength","etaLength",20,-1,+1,200,0.,200.);

//!extern TSystem* gSystem;

void load() {
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StMagF");
  gSystem->Load("StUtilities");  // new addition 22jul99
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities"); 
  gSystem->Load("StMcEvent"); 
  gSystem->Load("StMcEventMaker"); 
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StMcAnalysisMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StMuDSTMaker");
  //  gSystem->Setenv("JPROF_FLAGS", "JP_START JP_PERIOD=0.001"); 
  //  gSystem->Load("/afs/rhic/star/packages/DEV/.i386_redhat61/lib/libJprof"); 
  cout << "loding done " << endl;
}


void examplePt(const char* dir="", const char* file="/star/u/laue/afsWork/P02gd.lis",const char* filter="st:MuDst.root", const char* outFile="test.root") {
  load();
   StMuDebug::setLevel(0);  

   StMuDbReader* db = StMuDbReader::instance();
   db->addDb("/star/u/laue/afsWork/P02gc.db");
   db->addDb("/star/u/laue/afsWork/P02gd.db");

   int counter=0;
   int iret=0;
  StMuTimer timer;
  timer.start();
   StMuDebug::setLevel(3);  
  maker = new StMuDstMaker(0,0,dir,file,filter,1);   // set up maker in read mode
  cout << "time to load chain: " << timer.elapsedTime() <<endl;
   StMuDebug::setLevel(0);  
  timer.reset();
  timer.start();
  cout << maker->chain()->GetEntries() << " events in chain" << endl;
  while ( !(iret=maker->Make()) ) {
    //    iret = maker->Make();  // read an event 
    StMuDst* mu = maker->muDst();
    cout << mu->event()->l0Trigger()->triggerWord() << "   ";
    int n;
    n= mu->globalTracks()->GetEntries();
    //cout << n << " global tracks " << endl; 
    for (int l=0; l<n; l++) globalPt.Fill( mu->globalTracks(l)->pt() );   
    n= mu->primaryTracks()->GetEntries();
    for (int l=0; l<n; l++) primaryPt.Fill( mu->primaryTracks(l)->pt() );   
    n= mu->l3Tracks()->GetEntries();
    for (int l=0; l<n; l++) l3Pt.Fill( mu->l3Tracks(l)->pt() );   
    if (maker->chain()->GetEntries()) cout << counter++/maker->chain()->GetEntries() << " % done" << endl;
  }
  cout << endl;
  if (counter) cout << "time/event " << timer.elapsedTime()/counter <<endl;
  cout << " # of events:" << counter << endl;

  globalPt.Draw();
  primaryPt.Draw("same");
  l3Pt.Draw("same");

  TFile f(outFile,"RECREATE");
  globalPt.Write();
  primaryPt.Write();
  l3Pt.Write();
  f.Close();
}



