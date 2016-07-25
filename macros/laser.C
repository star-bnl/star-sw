class St_geant_Maker;
class St_tpcdaq_Maker;
#define SETBIT(n,i)  ((n) |= (1 << i))
class StTrsMiniMaker;
class StBFChain;
StBFChain *chain;
class St_geant_Maker;
St_geant_Maker *geant;
//#define OneLaser
//________________________________________________________________________________
void laser(const Int_t nevents=1,
		 const Char_t *fileIn =0, const Char_t* rootFile="laser.root"
		 ) {
  gROOT->LoadMacro("bfc.C"); 
  TString Chain("");
  if (! fileIn) Chain += "gstar,Y2004";
  else          Chain += "fzin"; 
  //  Chain += ",trs,tpc,-tcl,fcf,Cdst,Kalman,Event,-EventQA,-TpcHitMover,FieldOff";//,evout,GeantOut";
  //  Chain += ",tpc,trsMini,fcf,Cdst,Kalman,-TpcHitMover,Event,Mc,McAss,McAna,MiniMcMk";//,evout,GeantOut";
  Chain += ",tpc,trsMini,fcf";//,evout,GeantOut";
  //  Chain += ",tpc,trsMini,Cdst,Kalman,-TpcHitMover,Event,Mc,McAss,McAna,MiniMc";//,evout,GeantOut";
  bfc(-1,Chain.Data(),fileIn,0,rootFile);
  geant = (  St_geant_Maker * ) chain->Maker("geant");
#ifndef OneLaser
  gInterpreter->ProcessLine(".L laserGukine.C+");
#endif
//                                        NTRACK ID PTLOW PTHIGH YLOW YHIGH PHILOW PHIHIGH ZLOW ZHIGH
  if (geant && ! fileIn)     {
    //    geant->Do("gkine 1  6     1000.     1000. 0.     0.     0.    0.    10.   10.;");
    //    geant->Do("detp mfld mflg.Bfield=1.e-5");
  }
  StTrsMiniMaker *trs = (StTrsMiniMaker *) chain->Maker("tpc_raw");
  if (trs && trs->InheritsFrom(StTrsMiniMaker::Class())) {
    Int_t m_Mode = 0;
    SETBIT(m_Mode,StTrsMiniMaker::kPAI); 
    //    SETBIT(m_Mode,StTrsMiniMaker::kGAIN); 
    SETBIT(m_Mode,StTrsMiniMaker::kGAINO);
    //    SETBIT(m_Mode,StTrsMiniMaker::kPedestal);
    SETBIT(m_Mode,StTrsMiniMaker::kAVERAGEPEDESTAL);
    //    SETBIT(m_Mode,StTrsMiniMaker::kdEdxCorr);
    //    SETBIT(m_Mode,StTrsMiniMaker::kTree);
    trs->SetMode(m_Mode);
    trs->SetDebug(1);
    //    trs->SetDebug(12);
    //    trs->SetLaserScale(2.5e9);
  }
  if (nevents >= 0)   {
    Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
    StMaker *mk = chain->Maker("tpcDB"); // simulation mode
    if (! mk) return;
    mk->SetMode(1); 
    if (initStat) {
      cout << "Chain initiation has failed" << endl;
      chain->Fatal(initStat, "during Init()");
    }
  }
  if (geant) {
    gSystem->Load("gstar.so");
    geant->Do("call gstar");
#ifdef OneLaser
    //  geant->Do("gvert     56     0        0.0");
    geant->Do("gkine 1  171   100.     100. 0.     0.    0. 0.  10.   10.;");  
    geant->Do("gvert      0     54       0.0");
    //    geant->Do("gkine 1  170     10.     10. 0.     0.   0. 0.  10.   10.;");  
    //    geant->Do("gkine 1    5     10.     10. 0.     0.   0. 0.  10.   10.;");  
#endif
//     geant->Do("debug on");
//     geant->Do("swit 1 2");
//     geant->Do("swit 2 2");
  }
  chain->EventLoop(1,nevents);
}

