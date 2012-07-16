//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M
// 14 July 2012
//
// Macro to run FMS trigger simulator
//

void RunFmsTrigOverData(int nevents = 1000, const char* mudstfile = "/star/data27/reco/pp500_production_2011/ReversedFullField/P11id/2011/056/12056013/st_physics_12056013_raw_5020002.MuDst.root", const char* outfile = "fms.root")
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

  // Run
  chain->Init();
  chain->EventLoop(nevents);

  // Save histograms to ROOT file
  TFile* ofile = TFile::Open(outfile,"recreate");
  fmstrig->GetHistList()->Write();
  ofile->Close();
}
