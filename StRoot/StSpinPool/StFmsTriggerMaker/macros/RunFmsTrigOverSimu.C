//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M
// 14 July 2012
//
// Macro to run FMS trigger simulator
//

void RunFmsTrigOverSimu(int nevents = 1000, const char* mudstfile = "test.MuDst.root", const char* geantfile = "test.geant.root")
{
  // Load shared libraries
  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StFmsDbMaker");
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StFmsSimulatorMaker");
  gSystem->Load("StFmsTriggerMaker");

  // Create chain
  StChain* chain = new StChain;

  // Instantiate MuDst maker
  StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",mudstfile,".",1000,"MuDst");

  // STAR database
  St_db_Maker* stardb = new St_db_Maker("StarDb","MySQL:StarDb","$STAR/StarDb");
  stardb->SetDateTime(20110417,193427);

  // FMS database
  StFmsDbMaker* fmsdb = new StFmsDbMaker;

  // GEANT reader
  StIOMaker* ioMaker = new StIOMaker;
  ioMaker->SetFile(geantfile);
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");             // Deactivate all branches
  ioMaker->SetBranch("geantBranch",0,"r");   // Activate geant branch

  // Monte Carlo event maker
  StMcEventMaker* mcEventMaker = new StMcEventMaker;
  mcEventMaker->doPrintEventInfo  = false;
  mcEventMaker->doPrintMemoryInfo = false;

  // FMS hits simulator
  StFmsSimulatorMaker* fmsSim = new StFmsSimulatorMaker;

  // FMS trigger simulator
  StFmsTriggerMaker* fmstrig = new StFmsTriggerMaker;
  fmstrig->useStEvent();

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
