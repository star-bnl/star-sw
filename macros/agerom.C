class StBFChain;
//________________________________________________________________________________
void agerom(Int_t nevents=1,
	    const char *MainFile="/star/data03/daq/2007/120/8120057/st_physics_8120057_raw_1040035.daq") {
  if (gClassTable->GetID("TTable") < 0) {
    gSystem->Load("libTable");
  }
  gROOT->LoadMacro("bfc.C");
  TString Chain("in,ssd_daq,tls,StEvent");
  bfc(-1,Chain.Data(),MainFile,0,rootFile);
  gSystem->Load("StSsdClusterMaker.so");
  StSsdClusterMaker *clMk = new StSsdClusterMaker();
  if (nevents >= 0)   chain->Init();
  chain->EventLoop(1,nevents);
}
