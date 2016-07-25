class St_geant_Maker;
class St_tpcdaq_Maker;
#define SETBIT(n,i)  ((n) |= (1 << i))
class StTrsMiniMaker;
class StBFChain;
StBFChain *chain;
//________________________________________________________________________________
void tonko(const Int_t nevents=1,
#if 1
		 const Char_t *fileIn =0, const Char_t* rootFile="gstar.root"
#else
		 const Char_t *fileIn ="/star/rcf/test/rcf0191_01_380evts.fzd",
		 const Char_t* rootFile="rcf0191_01_380evts2.root"
#endif
		 ) {
  gROOT->LoadMacro("bfc.C"); 
  TString Chain("");
  if (! fileIn) Chain += "gstar,Y2004";
  else          Chain += "fzin"; 
  //  Chain += ",trs,tpc,-tcl,fcf,Cdst,Kalman,Event,-EventQA,-TpcHitMover,FieldOff";//,evout,GeantOut";
  Chain += ",tpc,FieldOff,trsMini,fcf";//,evout,GeantOut";
  bfc(-1,Chain.Data(),fileIn,0,rootFile);
  St_geant_Maker *geant = (  St_geant_Maker * ) chain->Maker("geant");
  //                                        NTRACK ID PTLOW PTHIGH YLOW YHIGH PHILOW PHIHIGH ZLOW ZHIGH
  if (geant && ! fileIn)     {
    geant->Do("gkine 1  6     1000.     1000. 0.     0.     0.    0.    10.   10.;");
    //    geant->Do("detp mfld mflg.Bfield=1.e-5");
    geant->Do("debug on");
    geant->Do("DCAY 0");
    geant->Do("ANNI 0");
    geant->Do("BREM 0");
    geant->Do("COMP 0");
    geant->Do("HADR 0");
    geant->Do("MUNU 0");
    geant->Do("PAIR 0");
    geant->Do("PFIS 0");
    geant->Do("PHOT 0");
    geant->Do("RAYL 0");
    geant->Do("LOSS 4"); // no fluctuations 
    geant->Do("DRAY 0");
    geant->Do("MULS 0");
    geant->Do("STRA 0");
    //         CUTS [ CUTGAM CUTELE CUTHAD CUTNEU CUTMUO BCUTE BCUTM DCUTE DCUTM PPCUTM TOFMAX GCUTS[5]
    geant->Do("CUTS .001 .001 .001 .001 .001  .001 .001 .001 .001 .001 50.e-6");
    geant->Do("gclose all");
  }
  StTrsMiniMaker *trs = (StTrsMiniMaker *) chain->Maker("tpc_raw");
  if (trs && trs->InheritsFrom(StTrsMiniMaker::Class())) {
    Int_t m_Mode = 0;
    SETBIT(m_Mode,StTrsMiniMaker::kPAI); 
    //    SETBIT(m_Mode,StTrsMiniMaker::kGAIN); 
    SETBIT(m_Mode,StTrsMiniMaker::kPedestal);
    //    SETBIT(m_Mode,StTrsMiniMaker::kdEdxCorr);
    //    SETBIT(m_Mode,StTrsMiniMaker::kTree);
    trs->SetMode(m_Mode);
  }
  if (nevents >= 0)   {
    Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
    if (initStat) {
      cout << "Chain initiation has failed" << endl;
      chain->Fatal(initStat, "during Init()");
    }
  }
  chain->EventLoop(1,nevents);
}

