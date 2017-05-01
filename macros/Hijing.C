void Hijing(Int_t NoEvents = 1000, Int_t runG = 1, const Char_t *frame = "CMS", Float_t rootS = 200,
	    const Char_t *aIn = "Au", const Char_t *bIn = "Au", 
	    Float_t bmin = 0, Float_t bmax = 30) {
  gSystem->Load("libSt_base");                                        //  StMemStat::PrintMem("load St_base");
  Bool_t needLogger  = kFALSE;
  if (gSystem->Load("liblog4cxx") >=  0) {             //  StMemStat::PrintMem("load log4cxx");
    cout << " + liblog4cxx";
    if(gSystem->Load("libStStarLogger") >= 0) {              //  StMemStat::PrintMem("load log4cxx");
      cout << " + libStStarLogger";
      //      gROOT->ProcessLine("StLoggerManager::StarLoggerInit();"); 
      StLoggerManager::StarLoggerInit();
      if (gROOT->IsBatch())  StLoggerManager::setColorEnabled(kFALSE);
    }
  }
  gSystem->Load("libStChain");                                        //  StMemStat::PrintMem("load StChain");
  cout << endl;
  gSystem->Load( "libVMC" );
  gSystem->Load( "libgen_Tables" );
  gSystem->Load( "libsim_Tables" );
  gSystem->Load( "StarGeneratorUtil" );
  gSystem->Load( "StarGeneratorEvent" );
  gSystem->Load( "StarGeneratorBase" );
  gSystem->Load( "libMathMore"   );  
  gSystem->Load( "libHijing1_383");
  StChain *chain = new StChain();
  primary = new StarPrimaryMaker();
  primary -> SetFileName( Form("hijing%s%s%i_%i_%i.gener.root",aIn,bIn,(Int_t) rootS,runG,NoEvents));
  StarHijing *hijing = new StarHijing("hijing");
  hijing->SetTitle("Hijing 1.383");

  // Setup collision frame, energy and beam species
  hijing->SetFrame(frame,rootS);
  hijing->SetBlue(aIn);
  hijing->SetYell(bIn);  
  hijing->SetImpact(bmin, bmax);       // Impact parameter min/max (fm)    0.   30.

  // Setup RNG seed and map all ROOT TRandom here
  StarRandom::seed( runG );
  StarRandom::capture();
  // Configure HIJING simulation
  HiParnt_t &hiparnt = hijing->hiparnt();
  {
    hiparnt.ihpr2(4) = 0;     // Jet quenching (1=yes/0=no)       0
    hiparnt.ihpr2(3) = 0;     // Hard scattering (1=yes/0=no)
    hiparnt.hipr1(10) = 2.0;  //    pT jet
    hiparnt.ihpr2(8)  = 10;   // Max number of jets / nucleon
    hiparnt.ihpr2(11) = 1;    // Set baryon production
    hiparnt.ihpr2(12) = 1;    // Turn on/off decay of particles [1=recommended]
    hiparnt.ihpr2(18) = 0;    // 1=B quark production.  0=C quark production.
    hiparnt.hipr1(7) = 5.35;  // Set B production ???? Not really used... Really ????
  }

  // For more configuration options, see the HIJING manual
  // http://ntc0.lbl.gov/~xnwang/hijing/doc.html

  primary -> AddGenerator(hijing);
  primary -> SetCuts( 1.0E-6); //  , -1., -2.5, +2.5 );
  if (NoEvents >= 0) {
    primary -> Init();
    if (NoEvents > 0) {
      chain->EventLoop(1,NoEvents);
    }
  }
}
