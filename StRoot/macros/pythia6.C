Int_t pythia6(TString mode="pp:W:510", Int_t tune=320) {
  
  if (!gROOT->GetClass("St_geant_Maker")) return 0;
  gSystem->Load( "libVMC");
  gSystem->Load( "libgen_Tables");
  gSystem->Load( "StarGeneratorUtil" );
  gSystem->Load( "StarGeneratorEvent" );
  gSystem->Load( "StarGeneratorBase" );

  gSystem->Load( "libMathMore"   );  
  
  // Setup RNG seed and map all ROOT TRandom here
  Int_t rngSeed=1234; // Oposite to TRandom3 rngSeed != 0 means that random number will be generated for each process independently
  StarRandom::seed( rngSeed );
  StarRandom::capture();
  
  //
  // Create the primary event generator and insert it
  // before the geant maker
  //
  //  StarPrimaryMaker *
  StarPrimaryMaker *primary = new StarPrimaryMaker();
  //    primary -> SetFileName( "pythia6.starsim.root");
  if (! StMaker::GetTopChain() ) return kStErr;
  Int_t ok = ((StBFChain *)StMaker::GetTopChain()) -> AddBefore( "geant", primary );
  if (ok != kStOk) return ok;
  if (gSystem->Load( "libPythia6_4_23") < 0) return kStErr;
  gSystem->Load( "StarPythia6"   );

  StarPythia6 *pythia6 = new StarPythia6("pythia6");
  if (mode.Contains("pp",TString::kIgnoreCase)) {
    pythia6->SetBlue("proton");
    pythia6->SetYell("proton");
    if (mode.Contains("W",TString::kIgnoreCase)) {
      // Setup pythia process
      PySubs_t &pysubs = pythia6->pysubs();
      pysubs.msel = 12;
      pysubs.ckin(3)=4.0;
    }
    if (mode.Contains("510")) pythia6->SetFrame("CMS", 510.0 );
    if (mode.Contains("200")) pythia6->SetFrame("CMS", 200.0 );
  } else if ( mode.Contains("ep",TString::kIgnoreCase)) {
    Double_t pblue[]={0.,0.,30.0};
    Double_t pyell[]={0.,0.,-320.0};
    pythia6->SetFrame("3MOM", pblue, pyell );
    pythia6->SetBlue("e-");
    pythia6->SetYell("proton");
  }
  if ( tune ) pythia6->PyTune( tune );
  primary->Init();
  primary->AddGenerator(pythia6);
  return ok;
}
