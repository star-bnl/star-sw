
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
TH1D refMult("refMult","refMult",100,0.,100.);

TH2D etaLength("etaLength","etaLength",20,-1,+1,200,0.,200.);


void exampleStMuIOMaker(const char* file="/star/data14/reco/productionCentral/ReversedFullField/P02ge/2001/324/st_physics_2324001_raw_0006.MuDst.root") {
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  StMuDebug::setLevel(0);  

  int counter=0;
  int iret=0;

  StMuIOMaker* maker = new StMuIOMaker("MuDst");  
  maker->SetFileAndOpen(file);
//  maker->SetFileName("st_physics_2313018_raw_0029.MuDst.root");
  TMemStat memStat("exampleMuIO");

  StMuDst* mu;
// evtid is an unique id of an event in a run which may involve several files!
// it is different from sequential index in a run which starts from "0"!
//  for ( int evtid=80673; evtid<80694; evtid++) {
//  for ( int evtid=6300; evtid<6321; evtid++) {
//	iret = maker->Make( StUKey(2271008,evtid) );  
//	iret = maker->Make( StUKey(2313018,evtid) );  
// alterative
for ( int i=0; i<20; i++) {
//	iret = maker->Make();  // read an event in natural sequential 
	iret = maker->Make(i);  // read an event with seqential index 
	if ( iret == kStOK) {
	   StMuDst* mu = maker->muDst();
	   if ( !mu ) continue;
//           if(i%10 != 0) continue;

// take a look at branches of tracks
	   int n;
	   n= mu->globalTracks()->GetEntries();
	   for (int l=0; l<n; l++) globalPt.Fill(mu->globalTracks(l)->pt());   
	   n= mu->primaryTracks()->GetEntries();
	   for (int l=0; l<n; l++) primaryPt.Fill(mu->primaryTracks(l)->pt()); 
	   n= mu->l3Tracks()->GetEntries();
	   for (int l=0; l<n; l++) {l3Pt.Fill( mu->l3Tracks(l)->pt() );}   

// take a look at event branch 
	   StMuEvent* muEvent = mu->event();
	   int referenceMultiplicity = muEvent->refMult();
	   refMult.Fill(referenceMultiplicity);

	   cout << "#" << i << " index=" << maker->currentIndex()
//	   cout << "eventId =" << evtid << " index=" << maker->currentIndex() 
                << " refmult= "<< referenceMultiplicity
	        << " used= "<< memStat.Used()
	        << " size= "<< memStat.ProgSize() << endl;
	   counter++;
	}
  }
  cout << " # of events:" << counter << endl;

  globalPt.Draw();
  primaryPt.Draw("same");
  l3Pt.Draw("same");
 
  TFile f("testMuIO.root","RECREATE");
  globalPt.Write();
  primaryPt.Write();
  l3Pt.Write();
  refMult.Write();
  f.Close();
}
