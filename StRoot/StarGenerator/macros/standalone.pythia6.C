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

// ----------------------------------------------------------------------------
void trig( Int_t n=1 )
{
  for ( Int_t i=0; i<n; i++ ) {
    chain->Clear();
    chain->Make();
  }
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void Pythia6( TString mode="pp:W", Int_t tune=320 )
{
  
  //  gSystem->Load( "libStarGeneratorPoolPythia6_4_23.so" );
  gSystem->Load( "libPythia6_4_23.so");
  gSystem->Load( "StarPythia6.so"   );

  StarPythia6 *pythia6 = new StarPythia6("pythia6");
  if ( mode=="pp:W" )
  {
    pythia6->SetFrame("CMS", 510.0 );
    pythia6->SetBlue("proton");
    pythia6->SetYell("proton");
    if ( tune ) pythia6->PyTune( tune );

    // Setup pythia process
    PySubs_t &pysubs = pythia6->pysubs();
    pysubs.msel = 12;
    pysubs.ckin(3)=4.0;

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
void standalone( Int_t nevents=100, UInt_t rngSeed = 12345 )
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
    _primary -> SetFileName( "pythia6.starsim.root");
    //  chain -> AddBefore( "geant", _primary );
  }

  //
  // Setup an event generator
  //
  Pythia6( "pp:W" );

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
  _primary->SetEtaRange ( -3.0, +3.0 );
  //                    phimin phimax
  _primary->SetPhiRange ( 0., TMath::TwoPi() );
  
  
  // 
  // Setup a realistic z-vertex distribution:
  //   x = 0 gauss width = 1mm
  //   y = 0 gauss width = 1mm
  //   z = 0 gauss width = 30cm
  // 
  _primary->SetVertex( 0., 0., 0. );
  _primary->SetSigma( 0.1, 0.1, 30.0 );

  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();

  //
  // Trigger on nevents
  //
  trig( nevents );

}
// ----------------------------------------------------------------------------

