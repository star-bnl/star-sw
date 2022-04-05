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

TH1D refMult("refMult","refMult",100,0.,100.);

void exampleRefMult(const char* dir="", const char* file="/star/u/laue/afsWork/dAu200.lis",const char* filter="st:MuDst.root", const 
char* outFile="test.root") {
  gROOT->LoadMacro("StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  int counter=0;
  int iret=0;
  StMuTimer timer;
  timer.start();
  StMuDebug::setLevel(0);  
  maker = new StMuDstMaker(0,0,dir,file,filter,1);   // set up maker in read mode
  //                       0,0                        this mean read mode
  //                           dir                    read all files in this directory
  //                               file               bla.lis real all file in this list, if (file!="") dir is ignored
  //                                    filter        apply filter to filenames, multiple filters are separated by ':'
  //                                          10      maximum number of file to read
  cout << "time to load chain: " << timer.elapsedTime() <<endl;
  StMuDebug::setLevel(0);  
  timer.reset();
  timer.start();
  cout << maker->chain()->GetEntries() << " events in chain" << endl;
  TMemStat memStat("exampleRefMult");
  while ( !(iret=maker->Make()) ) { // read an event  
      StMuDst* mu = maker->muDst();   // get a pointer to the StMuDst class, the class that points to all the data
      StMuEvent* muEvent = mu->event(); // get a pointer to the class holding event-wise information
      int referenceMultiplicity = muEvent->refMult(); // get the reference multiplicity
      refMult->Fill(referenceMultiplicity); // fill histogram
      cout << " #" << counter;
      cout << " refMult= "<< referenceMultiplicity;
      cout << " used= "<< memStat.Used();
      cout << " size= "<< memStat.ProgSize();
      cout << endl;
      counter++;
  }
  cout << endl;
  if (counter) cout << "time/event " << timer.elapsedTime()/counter <<endl;
  cout << " # of events:" << counter << endl;

  refMult.Draw();

  TFile f(outFile,"RECREATE");
  refMult.Write();
  f.Close();
}



