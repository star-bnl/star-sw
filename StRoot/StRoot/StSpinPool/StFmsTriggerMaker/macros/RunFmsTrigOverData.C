//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M
// 14 July 2012
//
// Macro to run FMS trigger simulator
//

void RunFmsTrigOverData(int nevents = 1000, const char* mudstfile = "/star/data27/reco/pp500_production_2011/ReversedFullField/P11id/2011/056/12056013/st_physics_12056013_raw_5020002.MuDst.root")
{
  // Load shared libraries
  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StFmsTriggerMaker");

  // Create chain
  StChain* chain = new StChain;

  // Instantiate MuDst maker
  StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",mudstfile,".",1000,"MuDst");

  // STAR database
  St_db_Maker* starDb = new St_db_Maker("StarDb","MySQL:StarDb");

  // FMS trigger simulator
  StFmsTriggerMaker* fmstrig = new StFmsTriggerMaker;
  fmstrig->useMuDst();

  // Initialize chain
  chain->Init();

  // Event loop
  for (int iEvent = 1; iEvent <= nevents; ++iEvent) {
    chain->Clear();
    int status = chain->Make(iEvent);
    if (status == kStSkip) continue;
    if (status % 10 == kStEOF || status % 10 == kStFatal) break;

    // Test FMS dijet trigger
    if (fmstrig->FmsDijet()) {
      printf("Run=%d Event=%d - Got FMS dijet trigger\n",chain->GetRunNumber(),chain->GetEventNumber());
    }
  } // Event loop
}
