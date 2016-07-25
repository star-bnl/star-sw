void Jeff(
#if 1
		 const Int_t nevents=1,
		 const Char_t *fileIn =0, const Char_t* rootFile="gstar_test.root"
#else
		 const Int_t nevents=1,
		 //		 const Char_t *fileIn ="/star/rcf/test/rcf0191_01_380evts.fzd",
		 //		 const Char_t *fileIn ="/star/rcf/test/rcf1198_91_4500evts.fzd",
		 const Char_t *fileIn ="/star/rcf/simu/auau200/hijing/b0_20/inverse/year2001/hadronic_on/gstardata/rcf0191_01_380evts.fzd",
		 const Char_t* rootFile="rcf0191_01_380evts_id2.root"
#endif
		 ) {
  gROOT->LoadMacro("bfc.C"); 
  TString Chain("");
  if (! fileIn) Chain += "gstar,Y2004";
  else          Chain += "fzin";
  //  Chain += ",trs,tpc,Cdst,Kalman,Event,Mc,-EventQA,-TpcHitMover,-EvOut,debug";
  //  Chain += ",tpc,Cdst,trsMini,Kalman,Event,Mc,-EventQA,-TpcHitMover,-EvOut,debug";
  Chain += ",trs,tpc,Cdst,fcf,Kalman,Event,Mc,-EventQA,-TpcHitMover,-EvOut,debug";
  //  Chain += ",trs,tpc,Cdst,Kalman,svtDb,ITTF,Sti,SvtIT,Event,Mc,-EventQA,-TpcHitMover,-EvOut,debug";
  //,evout,GeantOut";

  cout << "Chain : " << Chain.Data() << endl;
  bfc(-1,Chain.Data(),fileIn,0,rootFile);
//   gInterpreter->ProcessLine("typedef St_TpcAdcCorrection TpcAdcCorrection;");
//   gInterpreter->ProcessLine("typedef St_TpcAdcCorrection TpcdEdxCor;");
//   gInterpreter->ProcessLine("typedef St_TpcAdcCorrection TpcDriftDistOxygen;");
//   gInterpreter->ProcessLine("typedef St_TpcAdcCorrection TpcdXCorrection;");
//   gInterpreter->ProcessLine("typedef St_TpcAdcCorrection TpcLengthCorrection;");
//   gInterpreter->ProcessLine("typedef St_TpcAdcCorrection TpcMultiplicity;");
//   gInterpreter->ProcessLine("typedef St_TpcAdcCorrection tpcPressureB;");
  
  St_db_Maker *db = (St_db_Maker *) chain->GetMaker("db");
  db->SetDateTime(20040125,0);
  StMcParameterDB* parameterDB = StMcParameterDB::instance();  
  // TPC
  parameterDB->setXCutTpc(.5); // 5 mm
  parameterDB->setYCutTpc(.5); // 5 mm
  parameterDB->setZCutTpc(.5); // 5 mm
  parameterDB->setReqCommonHitsTpc(3); // Require 3 hits in common for tracks to be associated
  St_geant_Maker *geant = (  St_geant_Maker * ) chain->Maker("geant");
  //                                        NTRACK ID PTLOW PTHIGH YLOW YHIGH PHILOW PHIHIGH ZLOW ZHIGH
  if (geant && ! fileIn)       geant->Do("gkine 10  6     1.     1.  -1     1      0    6.28    0.    0.;");
  //  if (geant && ! fileIn)      geant->Do("gkine 1 6 1. 1. -.2 .2 0. 6.28  0. 0.;");
//   StMaker *trs = chain->Maker("Trs");
//   if (trs) trs->SetDebug(1); // (11);
//   else     return;
  // now execute the chain member functions

  //  chain->PrintInfo();
  if (nevents >= 0)   {
    Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
    if (initStat) {
      cout << "Chain initiation has failed" << endl;
      chain->Fatal(initStat, "during Init()");
    }
  }
  int iMake=0,iev=1;
  Int_t iTotal = 0, iBad = 0;
  TBenchmark evnt;
 EventLoop: if (iev<=nevents && iMake != kStEOF && iMake != kStFatal) {
   evnt.Reset();
   evnt.Start("QAInfo:");
   chain->Clear();
   cout << "---------------------- Processing Event : " << iev << " ----------------------" << endl;
   iMake = chain->Make(iev); // This should call the Make() method in ALL makers
   if (iMake <kStEOF) {
     iTotal++;
     if (iMake == kStErr) { cout << "Error Event Processed. Status = " << iMake << endl; iBad++;}
     evnt.Stop("QAInfo:");
     printf ("QAInfo: Done with Event [no. %d/run %d/evt. %d/Date.Time %d.%d/sta %d] Real Time = %10.2f seconds Cpu Time =  %10.2f seconds \n",
	     iev,chain->GetRunNumber(),chain->GetEventNumber(),chain->GetDate(), chain->GetTime(),
	     iMake,evnt.GetRealTime("QAInfo:"),evnt.GetCpuTime("QAInfo:"));
     iev++; goto EventLoop;
   } // Event Loop
 }
 END:
  fflush(stdout);
  printf ("QAInfo:Run completed ");
  gSystem->Exec("date");
  {
    TDatime t;
    printf ("\nQAInfo:Run is finished at Date/Time %i/%i; Total events processed :%i and not completed: %i\n",
	    t.GetDate(),t.GetTime(),iTotal,iBad);
  }
}

