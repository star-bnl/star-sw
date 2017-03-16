TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_geant_Maker")) return 0;
  StBFChain *chain = (StBFChain *) StMaker::GetChain();
  St_geant_Maker *geantMk = chain->GetMaker("geant");
#if 0
  gSystem->Load( "libVMC.so");
#endif
  gSystem->Load( "StarGeneratorUtil" );
  gSystem->Load( "StarGeneratorEvent" );
  gSystem->Load( "StarGeneratorBase" );
  gSystem->Load( "libMathMore"   );  
  gSystem->Load( "libHijing1_383");
  gSystem->Load( "gstar" );
  geantMk->Do("call gstar");
  StarPrimaryMaker *primary = new StarPrimaryMaker();
  primary -> SetFileName( "hijing.starsim.root");
  chain -> AddBefore( "geant", primary );
  //
  // Setup an event generator
  //
  StarHijing *hijing = new StarHijing("hijing");
  hijing->SetTitle("Hijing 1.383");

  // Setup collision frame, energy and beam species
  hijing->SetFrame("CMS",200.0);
  hijing->SetBlue("He3");
  hijing->SetYell("Au");  
  hijing->SetImpact(0.0, 30.0);       // Impact parameter min/max (fm)    0.   30.
  hijing->hiparnt().ihpr2(4) = 0;     // Jet quenching (1=yes/0=no)       0
  hijing->hiparnt().ihpr2(3) = 0;     // Hard scattering (1=yes/0=no)
  hijing->hiparnt().hipr1(10) = 2.0;  //    pT jet
  hijing->hiparnt().ihpr2(8)  = 10;   // Max number of jets / nucleon
  hijing->hiparnt().ihpr2(11) = 1;    // Set baryon production
  hijing->hiparnt().ihpr2(12) = 1;    // Turn on/off decay of particles [1=recommended]
  hijing->hiparnt().ihpr2(18) = 1;    // Turn on/off B production
  hijing->hiparnt().hipr1(7) = 5.35;  // Set B production ???? Not really used... Really ????

  // For more configuration options, see the HIJING manual
  // http://ntc0.lbl.gov/~xnwang/hijing/doc.html

  primary -> AddGenerator(hijing);
  primary -> SetCuts( 1.0E-6 , -1., -2.5, +2.5 );
  
  //
  // Initialize primary event generator and all sub makers
  //
  primary -> Init();
  geantMk->Do("gkine -4 0");
  TDataSet *tableSet = new TDataSet("Hijing");
  return (TDataSet *)tableSet;
}
