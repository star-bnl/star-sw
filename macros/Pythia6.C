void Pythia6(Int_t NoEvents = 1000, Int_t runG = 1, const Char_t *frame = "CMS", Float_t rootS = 510,
	      const Char_t *aIn = "proton", const Char_t *bIn = "proton", Int_t tune = 320) {
  gROOT->LoadMacro("bfc.C");
  TString Chain("Pythia6_4_28,nodefault");
  bfc(-1,Chain,0,0,0);
  StarPrimaryMaker *primary = (StarPrimaryMaker *) chain->Maker("Generators");
  primary -> SetFileName( Form("pythia6_%s_%s%_i_%i_%i.gener.root",aIn,bIn,(Int_t) rootS,runG,NoEvents));
  StarPythia6 *pythia6 = (StarPythia6 *) chain->Maker("Pythia6");
  pythia6->SetTitle("Pythia6");

  // Setup collision frame, energy and beam species
  pythia6->SetFrame(frame,rootS);
  pythia6->SetBlue(aIn);
  pythia6->SetYell(bIn);  
  if (tune) pythia6->PyTune(tune);

  // Setup RNG seed and map all ROOT TRandom here
  StarRandom::seed( runG );
  StarRandom::capture();
  //  primary -> AddGenerator(pythia6);
  //  primary -> SetCuts( 1.0E-6); //  , -1., -2.5, +2.5 );
  //                    ptmin  ptmax
  primary->SetPtRange  (0.0,  -1.0);         // GeV
  //                    phimin phimax
  primary->SetPhiRange ( 0., TMath::TwoPi() );
  //                    etamin etamax
  //  primary->SetEtaRange ( -3.0, +3.0 );
  // Setup a realistic z-vertex distribution:
  //   x = 0 gauss width = 1mm
  //   y = 0 gauss width = 1mm
  //   z = 0 gauss width = 30cm
  // 
  primary->SetVertex( 0., 0., 0. );
  primary->SetSigma( 0.1, 0.1, 30.0 );
  
  if (NoEvents >= 0) {
    primary -> Init();
    if (NoEvents > 0) {
      chain->EventLoop(1,NoEvents);
    }
  }
}
