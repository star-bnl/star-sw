// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
//#include "StRoot/StMuDSTMaker/COMMON/StMuTypes.hh"

#include "TH1.h"
#include "TChain.h"
#include "TSystem.h"
#include "TFile.h"
#include <iostream>

class StMuIOMaker;
StMuIOMaker* maker;


TH1D globalPt("globalPt","globalPt",100,0.,3.);
TH1D primaryPt("primaryPt","primaryPt",100,0.,3.);
TH1D l3Pt("l3Pt","l3Pt",100,0.,3.);

TH2D etaLength("etaLength","etaLength",20,-1,+1,200,0.,200.);


void exampleStMuIOMaker(const char* file="/star/data14/reco/productionCentral/ReversedFullField/P02ge/2001/324/st_physics_2324001_raw_0006.MuDst.root") {
  gROOT->LoadMacro("StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  StMuDebug::setLevel(0);  

  int counter=0;
  int iret=0;
  maker = new StMuIOMaker("MuDst");
  maker->SetFileName(file);
  TMemStat memStat("examplePt");

  StMuDst* mu; 
  for ( int i=0; i<1000; i++) {
	iret = maker->Make( StUKey(2324001,i) );
	if ( iret == kStOK) {
	    //    iret = maker->Make();  // read an event 
	    StMuDst* mu = maker->muDst();
	    if ( !mu ) continue;
	    int n;
	    n= mu->globalTracks()->GetEntries();
	    for (int l=0; l<n; l++) globalPt.Fill( mu->globalTracks(l)->pt() );   
	    n= mu->primaryTracks()->GetEntries();
	    for (int l=0; l<n; l++) primaryPt.Fill( mu->primaryTracks(l)->pt() );   
	    n= mu->l3Tracks()->GetEntries();
	    for (int l=0; l<n; l++) l3Pt.Fill( mu->l3Tracks(l)->pt() );   
	    cout << "#" << i;
	    cout << " index=" << maker->currentIndex();
	    cout << " eventCounter= "<< maker->eventCounter();
	    cout << " used= "<< memStat.Used();
	    cout << " size= "<< memStat.ProgSize();
	    cout << endl;
	    counter++;
	}
  }
  cout << " # of events:" << counter << endl;

  globalPt.Draw();
  primaryPt.Draw("same");
  l3Pt.Draw("same");

}



