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


void examplePt(const char* dir="", const char* file="/star/u/laue/afsWork/test.list",const char* filter="st:MuDst.root", const 
char* outFile="test.root") {
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

   StMuDebug::setLevel(2);  
//    StMuDbReader* db = StMuDbReader::instance();
//    db->addDb("/star/u/laue/afsWork/P02gc.db");
//    db->addDb("/star/u/laue/afsWork/P02gd.db");

  int counter=0;
  int iret=0;
  StMuTimer timer;
  timer.start();
  maker = new StMuDstMaker(0,0,dir,file,filter,10);   // set up maker in read mode
  cout << "time to load chain: " << timer.elapsedTime() <<endl;
  timer.reset();
  timer.start();
  cout << maker->chain()->GetEntries() << " events in chain" << endl;
  TMemStat memStat("examplePt");
  while ( !(iret=maker->Make()) ) {
    //    iret = maker->Make();  // read an event 
    StMuDst* mu = maker->muDst();
    int n;
    n= mu->globalTracks()->GetEntries();
    for (int l=0; l<n; l++) globalPt.Fill( mu->globalTracks(l)->pt() );   
    n= mu->primaryTracks()->GetEntries();
    for (int l=0; l<n; l++) primaryPt.Fill( mu->primaryTracks(l)->pt() );   
    n= mu->l3Tracks()->GetEntries();
    for (int l=0; l<n; l++) l3Pt.Fill( mu->l3Tracks(l)->pt() );   
    if (maker->chain()->GetEntries()) cout << "event# " << counter << " " << counter++/maker->chain()->GetEntries() << "%";
    cout << " used= "<< memStat.Used();
    cout << " size= "<< memStat.ProgSize();
    cout << endl;
    
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



