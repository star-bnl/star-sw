// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
//#include "StRoot/StMuDSTMaker/COMMON/StMuTypes.hh"

#include "TH1.h"
#include "TChain.h"
#include "TSystem.h"
#include "TFile.h"
#include <iostream>

class StMuDstMaker;
class StMuDstFilterMaker;
StMuDstMaker* maker;
StMuDstFilterMaker* filter;

void exampleMuDstFilter(const char* in="/star/u/laue/afsWork/dAu200.lis", const char* out="test.MuDst.root") {
    gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
    loadSharedLibraries();
    
    int counter=0;
    int iret=0;
    StMuTimer timer;
    timer.start();
    // switch of debug messages 
    StMuDebug::setLevel(0);
    // for faster chain building
    StMuDbReader* dbReader = StMuDbReader::instance();
    dbReader->addDb("dAu200.db");
    // create maker
    maker = new StMuDstMaker(0,0,"",in,"st_physics:MuDst.root",10000);   // set up maker in read mode
    cout << "time to load chain: " << timer.elapsedTime() <<endl;
    
    filter = new StMuDstFilterMaker("filter");
    filter->setMuDstMaker(maker);
    filter->setOutputFileName(out);
    filter->Init();
    
    timer.reset();
    timer.start();
    cout << maker->chain()->GetEntries() << " events in chain" << endl;
    TMemStat memStat("exampleMuDstFilter");
    int iret = maker->Make();  // get first event
    while ( iret==0 ) { // read an event  
	filter->Make();
	cout << " #" << counter;
	cout << " used= "<< memStat.Used();
	cout << " size= "<< memStat.ProgSize();
	cout << endl;
	counter++;
	iret = maker->Make();
    }
    cout << endl;
    if (counter) cout << "time/event " << timer.elapsedTime()/counter <<endl;
    cout << " # of events:" << counter << endl;
    filter->Finish(); // Make sure that output file is written    
}



