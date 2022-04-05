#if 0
#include "Ask.h"
#endif
//________________________________________________________________________________
void RunTbyT(Int_t nevents=999,
	     const char *eventFile1="/star/data98/reco/AuAu200_production/FullField/eval_Sti/2010/030/11030018/st_physics_11030018_raw_1020001.event.root",
	     //"./old/st_physics_11035026_raw_2010001.event.root",
	     const char *eventFile2="/star/data99/reco/AuAu200_production/FullField/eval_Stv/2010/030/11030018/st_physics_11030018_raw_1020001.event.root",
	     //"./new/st_physics_11035026_raw_2010001_1_200.event.root",
	     const char* tFile="TbyT.root") {
  gROOT->LoadMacro("bfc.C");
  TString Chain("StEvent,nodefault");
  bfc(-2,Chain.Data(),0,0,tFile);
  gSystem->Load("StTrackMateMaker");
  cout << "Job will run on    File: " << eventFile1 << endl;
  cout << "Correspondibg new  File: " << eventFile2 << endl;
  gSystem->Load("StIOMaker");
  // 1st IOMaker, for tpt file
  StIOMaker* ioMaker1 = new StIOMaker("IO1","r",eventFile1);//,"bfcTree");
  // 2nd IOMaker, for ittf file
  StIOMaker* ioMaker2 = new StIOMaker("IO2","r",eventFile2);//,"bfcTree");

  StTrackMateMaker*  goodStuff      = new StTrackMateMaker;
  //  chain->SetDEBUG(2);
  // now execute the chain member functions
  
  chain->PrintInfo();
  Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
  if (initStat) chain->Fatal(initStat, "during Init()");
  //  if (nevents > 0) chain->EventLoop(1,nevents);
  Int_t iMake = 0;
  StIOMaker *makers[2]  = {ioMaker1, ioMaker2};
  StEvent   *events[2] = {0,0};
  for (Int_t ev = 0; ev < nevents; ev++) {
    chain->Clear();
    StUKey ukey;
    while ((! events[0] || ! events[1])) {
      for (Int_t m = 0; m < 2; m++) {
	if (! events[m]) {
	  makers[m]->Clear();
	  iMake = makers[m]->MakeRead(ukey);
	  if (iMake%10 == kStEOF || iMake%10==kStFatal)	goto ENDofLOOP;
	  events[m] = (StEvent   *) makers[m]->GetDataSet("StEvent");
	}
      }
      if (! (events[0] && events[1])) continue;
      if (events[0]->runId() != events[1]->runId() || 
	  events[0]->id()    != events[1]->id()) {
	cout << "MisMatched runId/Event\t" 
	     << events[0]->runId() << "/" << events[0]->id() << "\t" 
	     << events[1]->runId() << "/" << events[1]->id() <<  endl;
	if (events[0]->runId() <  events[1]->runId() ||
	    events[0]->runId() == events[1]->runId() && 
	    events[0]->id()    < events[1]->id()) {
	  events[0] = 0; ukey = StUKey(events[1]->runId(),events[1]->id());
	} else {
	    events[1] = 0; ukey = StUKey(events[0]->runId(),events[0]->id());
	}
	continue;
      }
    }
#if 0
    if (! gROOT->IsBatch()) {
      if (Ask()) return;
    } else {_debugAsk = 0;}
#endif
    goodStuff->Make();
    events[0] = events[1] = 0;
  }
 ENDofLOOP:
}
