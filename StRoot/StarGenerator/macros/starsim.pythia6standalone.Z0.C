/**!
 * Example macro for running an event generator in standalone mode.
 *
 * Usage:
 *
 * root4star
 * .L standalone.pythia6.C
 * int nevents=100;
 * standalone( nevents )
 */

class St_geant_Maker;
St_geant_Maker *geant_maker = 0;

class StarGenEvent;
StarGenEvent   *event       = 0;

class StarPrimaryMaker;
StarPrimaryMaker *_primary = 0;

class StarPythia6;
StarPythia6 *_pythia6 = 0;

// ----------------------------------------------------------------------------
void trig( Int_t n=1 )
{
  for ( Int_t i=0; i<n; i++ ) {
    chain->Clear();
    chain->Make();
    //    if (0==i) _primary -> event() -> Print();
  }
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void Pythia6( TString mode="pp:W", Int_t tune=320 )
{
  
  //  gSystem->Load( "libStarGeneratorPoolPythia6_4_23.so" );
  gSystem->Load( "libPythia6_4_28.so");

  StarPythia6 *pythia6 = _pythia6 = new StarPythia6("pythia6");
  if ( mode=="pp:W" )
  {
    pythia6->SetFrame("CMS", 510.0 );
    pythia6->SetBlue("proton");
    pythia6->SetYell("proton");
    if ( tune ) pythia6->PyTune( tune );

    // Setup pythia process
    PySubs_t &pysubs = pythia6->pysubs();
    int& msel = pysubs.msel;
    msel = 12;
    
    // Setup other common block variables / array elements
 //   float& ckin3 = pysubs.ckin(3); 
 //   ckin3 = 4.0;

    //
    // Set particles to be stable so that the decay manager
    // can handle them in the starsim phase
    //
    pythia6 -> SetDecayFlag( +24, 0 ); // W+
    pythia6 -> SetDecayFlag( -24, 0 ); // W-
    pythia6 -> SetDecayFlag( +23, 0 ); // Z0
    pythia6 -> SetDecayFlag( -23, 0 ); // Z0
    pythia6 -> SetDecayFlag( +15, 0 ); // tau+
    pythia6 -> SetDecayFlag( -15, 0 ); // tau-
    
    pythia6 -> CloseDecays(24);
    pythia6 -> OpenDecay( 24, 206, 2 );  // limit decay to electron channel

  }

  if ( mode=="pp:Z" )
  {
    pythia6->SetFrame("CMS", 510.0 );
    pythia6->SetBlue("proton");
    pythia6->SetYell("proton");
    if ( tune ) pythia6->PyTune( tune );

    // Setup pythia process
    PySubs_t& pysubs = pythia6->pysubs();
    int& msel = pysubs.msel;
    msel = 11;
    
    PyPars_t& pypars = pythia6->pypars();
    int& mstp43 = pypars.mstp(43);
    mstp43 = 2;

    //
    // Set particles to be stable so that the decay manager
    // can handle them in the starsim phase
    //
    pythia6 -> SetDecayFlag( +24, 0 ); // W+
    pythia6 -> SetDecayFlag( -24, 0 ); // W-
    pythia6 -> SetDecayFlag( +23, 0 ); // Z0
    pythia6 -> SetDecayFlag( -23, 0 ); // Z0
    pythia6 -> SetDecayFlag( +15, 0 ); // tau+
    pythia6 -> SetDecayFlag( -15, 0 ); // tau-
    
    pythia6 -> CloseDecays(23);
    pythia6 -> OpenDecay( 23, 186, 2 ); 

  }



  if ( mode == "pp:minbias" )
  {
    pythia6->SetFrame("CMS", 510.0 );
    pythia6->SetBlue("proton");
    pythia6->SetYell("proton");
    if ( tune ) pythia6->PyTune( tune );
  }
  if ( mode == "ep" )
  {
    Double_t pblue[]={0.,0.,30.0};
    Double_t pyell[]={0.,0.,-320.0};
    pythia6->SetFrame("3MOM", pblue, pyell );
    pythia6->SetBlue("e-");
    pythia6->SetYell("proton");
    if ( tune ) pythia6->PyTune( tune );
  }
    
  _primary->AddGenerator(pythia6);
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void starsim( Int_t nevents=100, UInt_t rngSeed = 12345 )
{ 

  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = "tables nodefault";
    bfc(0, simple );
  }

  gSystem->Load( "libVMC.so");
  gSystem->Load( "St_g2t.so" );
  gSystem->Load( "St_geant_Maker.so" );
 
  gSystem->Load( "StarGeneratorUtil.so" );
  gSystem->Load( "StarGeneratorEvent.so" );
  gSystem->Load( "StarGeneratorBase.so" );

  gSystem->Load( "libMathMore.so"   );  

  //
  // Create the primary event generator and insert it
  // before the geant maker
  //
  //  StarPrimaryMaker *
  _primary = new StarPrimaryMaker();
  {
    _primary -> SetFileName( "pythia6.standalone.root");
    //  chain -> AddBefore( "geant", primary );
  }

  //
  // Setup an event generator
  //
  Pythia6( "pp:Z" );

  //
  // Initialize random number generator
  //
  StarRandom &random = StarRandom::Instance();
  random.capture(); // maps all ROOT TRandoms to StarRandom
  random.seed( rngSeed );


  //
  // Setup cuts on which particles get passed to geant for
  //   simulation.  (To run generator in standalone mode,
  //   set ptmin=1.0E9.)
  //                    ptmin  ptmax
  _primary->SetPtRange  (1.0E9,  -1.0);         // GeV
  //                    etamin etamax
  _primary->SetEtaRange ( -5.0, +5.0 );
  //                    phimin phimax
  _primary->SetPhiRange ( 0., TMath::TwoPi() );
  
  
  // 
  // Fixed x, y, z vertex
  // 
  _primary->SetVertex( 0., 0., 0. );
  _primary->SetSigma( 0., 0., 0. );

  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();

  //
  // Trigger on nevents
  //
  trig( nevents );

  //command("call pystat");
  _pythia6->PyStat(1);


}
// ----------------------------------------------------------------------------

