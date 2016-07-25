class St_geant_Maker;
class St_tpcdaq_Maker;
//________________________________________________________________________________
void Tonko(const Int_t nevents=1,
#if 1
		 const Char_t *fileIn =0, const Char_t* rootFile="gstar.root"
#else
		 const Char_t *fileIn ="/star/rcf/test/rcf0191_01_380evts.fzd",
		 const Char_t* rootFile="rcf0191_01_380evts2.root"
#endif
		 ) {
  gROOT->LoadMacro("bfc.C"); 
  TString Chain("");
  if (! fileIn) Chain += "gstar,Y2003X";
  else          Chain += "fzin"; 
  //  Chain += ",trs,tpc,-tcl,fcf,Cdst,Kalman,Event,-EventQA,-TpcHitMover,FieldOff";//,evout,GeantOut";
  Chain += ",trs,tpc,Cdst,Kalman,Event,-EventQA,-TpcHitMover,FieldOff";//,evout,GeantOut";
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
    geant->Do("CUTS .001 .001 .001 .001 .001  .001 .001 .001 .001 .001 50.e-6");
    geant->Do("gclose all");
  }
  //  if (geant && ! fileIn)      geant->Do("gkine 1 6 1. 1. -.2 .2 0. 6.28  0. 0.;");
  St_tpcdaq_Maker *tpcdaq = (St_tpcdaq_Maker *) chain->Maker("tpc_raw");
  tpcdaq->SetCorrection(0x0); // fcf && ! trs => no corrections
  StTrsMaker *trs = (StTrsMaker *) chain->Maker("Trs");
  if (trs) trs->SetDebug(1);
  else     return;
#if 0
  St_tpcdaq_Maker *tpcdaq = ( St_tpcdaq_Maker *) chain->Maker("tpc_raw");
  if (! tpcdaq) return;
  tpcdaq->SetCorrection(0x4); //  bit 2  =   do ASIC_THRESHOLDS
#endif
  if (nevents >= 0)   {
    Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
    if (initStat) chain->Fatal(initStat, "during Init()");
  }
  int istat=0,iev=1;
 EventLoop: if (iev<=nevents && istat!=2) {
   chain->Clear();
   cout << "---------------------- Processing Event : " << iev << " ----------------------" << endl;
   istat = chain->Make(iev); // This should call the Make() method in ALL makers
   if (istat == 2) { cout << "Last  Event Processed. Status = " << istat << endl; }
   if (istat == 3) { cout << "Error Event Processed. Status = " << istat << endl; }
   iev++; goto EventLoop;
 } // Event Loop
}

