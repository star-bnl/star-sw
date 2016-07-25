class St_geant_Maker;
class St_tpcdaq_Maker;
#define SETBIT(n,i)  ((n) |= (1 << i))
class StTrsMiniMaker;
class StBFChain;
StBFChain *chain;
//________________________________________________________________________________
void daq(const Int_t nevents=1,
	 const Char_t *fileIn ="/star/rcf/test/daq/2003/041/st_physics_4041002_raw_0020001.daq",
		 const Char_t* rootFile="st_physics_4041002.root") {
  gROOT->LoadMacro("bfc.C"); 
  TString Chain("");
  Chain += "in,tpc_daq,tpc,fcf";//,evout,GeantOut";
  bfc(-1,Chain.Data(),fileIn,0,rootFile);
  St_geant_Maker *geant = (  St_geant_Maker * ) chain->Maker("geant");
  if (nevents >= 0)   {
    Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
    if (initStat) {
      cout << "Chain initiation has failed" << endl;
      chain->Fatal(initStat, "during Init()");
    }
  }
  chain->EventLoop(1,nevents);
}

