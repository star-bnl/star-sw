//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M
// 18 July 2012
//
// Run bfc.C with FMS fast simulator in the chain to import FPD/FMS hits
// from GEANT to StEvent to MuDst.
//
// Note, as of July 18, 2012, this way of running the FMS simulator is no
// longer necessary. Instead, it is recommended to use the "fmsSim" chain option
// directly with bfc.C:
//
// root4star -q -b bfc.C'(1000,"DbV20110923 tpcRS y2011 fmsDb fmsSim MakeEvent ITTF NoSsdIt NoSvtIt Idst BAna l0 Tree logger Sti VFPPVnoCTB beamLine tpcDB TpcHitMover TpxClu bbcSim btofsim tags emcY2 EEfs evout -dstout IdTruth geantout big fzin MiniMcMk clearmem sdt20110417.193427")'
//

class StBFChain;
StBFChain* chain = 0;

void bfcFms(int nevents = 1000,
	    const char* fzfile = "test.fzd",
	    const char* chainopt = "DbV20110923 tpcRS y2011 fmsDb MakeEvent ITTF NoSsdIt NoSvtIt Idst BAna l0 Tree logger Sti VFPPVnoCTB beamLine tpcDB TpcHitMover TpxClu bbcSim btofsim tags emcY2 EEfs evout -dstout IdTruth geantout big fzin MiniMcMk clearmem sdt20110417.193427")
{
  gROOT->LoadMacro("bfc.C");
  bfc(-1,chainopt,fzfile);
  gSystem->Load("StFmsSimulatorMaker");
  StFmsSimulatorMaker* fmsSim = new StFmsSimulatorMaker;
  chain->AddAfter("fmsDb",fmsSim);
  chain->Init();
  chain->EventLoop(nevents);
}
