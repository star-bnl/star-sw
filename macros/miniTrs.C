#define SETBIT(n,i)  ((n) |= (1 << i))
void miniTrs(Int_t Nevent=2) {
  gROOT->LoadMacro("bfc.C");
  //  bfc(-1,"gstar,y2004,tpcDb,TrsMini",0,0,Form("TrsMini%i.root",Nevent));
  bfc(-1,"gstar,y2004,tpcDb,Corr2,tpc,TrsMini,fcf",0,0,Form("TrsMini%i.root",Nevent));
  //  bfc(-1,"gstar,tpcDb,Corr2,tpc,TrsMini,fcf",0,0,Form("TrsMini%i.root",Nevent));
  //  bfc(-1,"gstar,y2004",0,0,Form("TrsMini%i.root",Nevent));
#if 0
  StTrsMiniMaker *trs = (StTrsMiniMaker *) chain->Maker("tpc_raw");
  trs->SetDebug(222);
  //  trs->SetMode(200);
  Int_t m_Mode = 0;
  SETBIT(m_Mode,StTrsMiniMaker::kPAI); 
  //  SETBIT(m_Mode,StTrsMiniMaker::kGAINOAtALL); 
  SETBIT(m_Mode,StTrsMiniMaker::kGAIN); 
  SETBIT(m_Mode,StTrsMiniMaker::kNONOISE);
  //  SETBIT(m_Mode,StTrsMiniMaker::kAVERAGEPEDESTAL);
  //  SETBIT(m_Mode,StTrsMiniMaker::kPedestal);
  //  SETBIT(m_Mode,StTrsMiniMaker::kdEdxCorr);
  SETBIT(m_Mode,StTrsMiniMaker::kTree);
  trs->SetMode(m_Mode);
#endif
  if (Nevent >= 0) {
    chain->Init();
    geant = (  St_geant_Maker * ) chain->Maker("geant");
    if (geant) {
      // GVERTEX X_vertex Y_vertex Z_vertex
      // geant->Do("gvertex  -2.50      -197.40        32.00");
      //            NTRACK  ID PTLOW PTHIGH YLOW YHIGH PHILOW PHIHIGH ZLOW ZHIGH
      //  geant->Do("gkine   1  48     1      1    0     0      0.      0    0.    0.;");
      //  geant->Do("gkine   1 170     1      1    0     0      0.      0    0.    0.;");
      //  geant->Do("gkine   1  6     1      1   -1     1      0.   6.28    0.    0.;");
      //   geant->Do("mode  tpce simu 2");
      //   geant->Do("mode  tpce prin 2");
      //   geant->Do("mode  tpce debu 2");
      //   geant->Do("LOSS 1"); // with delta electron above dcute
      //         CUTS [ CUTGAM CUTELE CUTHAD CUTNEU CUTMUO BCUTE BCUTM DCUTE DCUTM PPCUTM TOFMAX GCUTS[5]
      //   geant->Do("CUTS     1e-5   1e-5   .001   .001   .001  .001  .001  1e-5  .001   .001 50.e-6");
      //  geant->Do("detp geometry y2004;");
//       geant->Do("gclose all");
//       geant->Do("debug on");
//       geant->Do("swit 1 2");
//       geant->Do("swit 2 2");
//       //    geant->Do("gkine 1 170 1 1 0 0 0 0");
//       geant->Do("gkine 1  170 1 1 0 0 0 0 40 40");
//       geant->Do("gvertex  60      60.40  0");
//       geant->Do("trig");
      //   geant->Do("user/input tx  laser/lasevt3.txt");
      //   geant->Do("debug on");
      //   geant->Do("swit 1 2");
      //   geant->Do("swit 2 2");
      //      geant->Do("gkine 1 6 1 1 1 1 0 0");
    }
    if (Nevent > 0) chain->EventLoop(1,Nevent);
    //    gObjectTable->Print();
  }
}
