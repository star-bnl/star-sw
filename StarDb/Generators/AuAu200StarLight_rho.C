TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_geant_Maker")) return 0;
  StBFChain *chain = (StBFChain *) StMaker::GetChain();
  St_geant_Maker *geantMk = chain->GetMaker("geant");
  gSystem->Load( "StarGeneratorUtil" );
  gSystem->Load( "StarGeneratorEvent" );
  gSystem->Load( "StarGeneratorBase" );
  gSystem->Load( "libMathMore"   );  
  gSystem->Load( "libStarLight");
  gSystem->Load( "gstar" );
  geantMk->Do("call gstar");
  // Setup RNG seed and map all ROOT TRandom here
  Int_t rngSeed=0;
#if 0
  TRandom rdnm(0);
  Int_t rngSeed = rdnm.GetSeed();
#endif
  StarRandom::seed( rngSeed );
  StarRandom::capture();
  StarPrimaryMaker *primary = new StarPrimaryMaker();
  primary -> SetFileName( "starlight.starsim.root");
  chain -> AddBefore( "geant", primary );
  //
  // Setup an event generator
  //
  StarLight *starlight = new StarLight("STARlight");
  starlight->SetTitle("StarLight 1.383");

  // Setup collision frame, energy and beam species
  starlight->SetFrame("CMS",39400.0);
  starlight->SetBlue("Au");
  starlight->SetYell("Au");
  starlight->SetBreakupMode(2); 
  starlight->SetProductionPID(113);  
  primary -> AddGenerator(starlight);
  
  //
  // Initialize primary event generator and all sub makers
  //

  primary->SetVertex( 0.32, 0.09, -1. );
  primary->SetSigma( 0.055, 0.02, 30.0);
  primary -> Init();
  geantMk->Do("gkine -4 0");
  TDataSet *tableSet = new TDataSet("StarLight");
  return (TDataSet *)tableSet;
}
