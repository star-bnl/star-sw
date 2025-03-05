//usr/bin/env root4star -l -b -q $0'('$1', '$2')'; exit $?
// macro to instantiate the Geant3 from within
// STAR  C++  framework and get the starsim prompt
// To use it do
//  root4star starsim.C

class St_geant_Maker;
St_geant_Maker *geant_maker = 0;

class StarGenEvent;
StarGenEvent   *event       = 0;

class StarPrimaryMaker;
StarPrimaryMaker *_primary = 0;

class StarKinematics;
StarKinematics *kinematics = 0;


TH1F* hMll = 0;
float numParticles = 1;

// ----------------------------------------------------------------------------
void geometry( TString tag, Bool_t agml=true )
{
  TString cmd = "DETP GEOM "; cmd += tag + " field=-5.0";
  if ( !geant_maker ) geant_maker = (St_geant_Maker *)chain->GetMaker("geant");
  geant_maker -> LoadGeometry(cmd);
  //  if ( agml ) command("gexec $STAR_LIB/libxgeometry.so");
}
// ----------------------------------------------------------------------------
void command( TString cmd )
{
  if ( !geant_maker ) geant_maker = (St_geant_Maker *)chain->GetMaker("geant");
  geant_maker -> Do( cmd );
}
// ----------------------------------------------------------------------------
void trig( Int_t n=1 )
{

  
  for ( Int_t i=0; i<n; i++ ) {

    // Clear the chain from the previous event
    chain->Clear();

    //(Momentum, Energy units are Gev/C, GeV)
    Double_t masses[2] = { 0.13957, 0.938} ;

    TGenPhaseSpace genEvent;
    TLorentzVector W;
    // W.SetPtEtaPhiM( 0.0, 100.0, 0, 3.096 );
    W.SetXYZM( 0, 0, 5, 1.11568 );
    genEvent.SetDecay(W, 2, masses);

    TLorentzVector lv;
    for ( int j = 0; j < numParticles; j++ ){
      Double_t weight = genEvent.Generate();
      TLorentzVector *pPion = genEvent.GetDecay(0);
      TLorentzVector *pProton = genEvent.GetDecay(1);
      lv = *pPion + *pProton;

      StarGenParticle *pion;
    pion = kinematics->AddParticle( "pi-" );
    
      pion->SetPx(pPion->Px());
      pion->SetPy(pPion->Py());
      pion->SetPz(pPion->Pz());
      pion->SetMass( masses[0] );

    StarGenParticle *proton;
    proton = kinematics->AddParticle( "p" );

    
      proton->SetPx(pProton->Px());
      proton->SetPy(pProton->Py());
      proton->SetPz(pProton->Pz());
      proton->SetMass( masses[1] );

      hMll->Fill( lv.M() );

      cout << "pion eta = " << pPion->Eta() << endl;
      cout << "proton eta = " << pProton->Eta() << endl;
    }

    
		// kinematics->Kine( numParticles, nameParticle.Data(), 10.2, 12.0, 2.5, 4.00  );

    // Generate the event
    chain->Make();

    // TTable* hits = chain->GetDataSet("bfc/.make/geant/.data/g2t_stg_hit");
    // if ( hits ) {
    //   double nhits = hits->GetNRows();
    //   hNumHits->Fill( double(i), nhits / 4.0 / numParticles );
    //   std::cout << "N hits  = " << nhits << std::endl;
    // }

    // Print the event
    // command("gprint hits stgh");

  }
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void Kinematics()
{
  
  //  gSystem->Load( "libStarGeneratorPoolPythia6_4_23.so" );
  gSystem->Load( "libKinematics.so");
  kinematics = new StarKinematics();
    
  _primary->AddGenerator(kinematics);
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void lambda( Int_t nevents=100, Int_t rngSeed=12352342 )
{ 
  hMll = new TH1F("hMll",";Mll;counts [10MeV]", 200, 2.0, 4.0 );
  cout << "Generating: " << nevents << " events with seed: " << rngSeed << endl;
  cout << "Simulating J/psi->e+e-" << endl;

  gSystem->Load( "libStarRoot.so" );
  gROOT->SetMacroPath(".:/star-sw/StRoot/macros/:./StRoot/macros:./StRoot/macros/graphics:./StRoot/macros/analysis:./StRoot/macros/test:./StRoot/macros/examples:./StRoot/macros/html:./StRoot/macros/qa:./StRoot/macros/calib:./StRoot/macros/mudst:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/graphics:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/analysis:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/test:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/examples:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/html:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/qa:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/calib:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/mudst:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/macros:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/tutorials");

  gROOT->ProcessLine(".L bfc.C");
  {
      TString simple = "sdt20211016 y2023 geant gstar usexgeom agml ";
      bfc(0, simple );
  }

  gSystem->Load( "libVMC.so");

  gSystem->Load( "StarGeneratorUtil.so" );
  gSystem->Load( "StarGeneratorEvent.so" );
  gSystem->Load( "StarGeneratorBase.so" );

  gSystem->Load( "libMathMore.so"   );  
  gSystem->Load( "xgeometry.so"     );

  // Setup RNG seed and map all ROOT TRandom here
  StarRandom::seed( rngSeed );
  StarRandom::capture();

  //
  // Create the primary event generator and insert it
  // before the geant maker
  //
  //  StarPrimaryMaker *
  _primary = new StarPrimaryMaker();
  {
      _primary -> SetFileName( "lambda_fwd_gun.root");
      chain -> AddBefore( "geant", _primary );
  }

  Kinematics();

  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();
  _primary->SetSigma( 0.1, 0.1, 0.1 ); // 1mm x 1mm x 1mm smearing at the vertex
  _primary->SetVertex(0.0, 0.0, 0.0 );

  //
  // Setup geometry and set starsim to use agusread for input
  //
  //geometry("y2012");
  command("gkine -4 0");
  command("gfile o lambda_fwd_gun.fzd");


  hNumHits = new TH1F("hNumEvents","Nhits/plane/incident track vs event number",nevents + 1, -0.5, (float)( nevents ) + 0.5 );

  //
  // Trigger on nevents
  //
  trig( nevents );

  TFile * f = new TFile( "lambda_gen.root", "RECREATE" );
  f->cd();
  hMll->Write();
  f->Write();

  command("call agexit");  // Make sure that STARSIM exits properly

}
// ----------------------------------------------------------------------------

