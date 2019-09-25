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
    _primary -> event() -> Print();
  }
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void Pythia6( TString mode="pp:W", Int_t tune=320 )
{
  
  //  gSystem->Load( "libStarGeneratorPoolPythia6_4_23.so" );
  gSystem->Load( "libPythia6_4_28.so");

  StarPythia6 *pythia6 = new StarPythia6("pythia6");
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
    float& ckin3 = pysubs.ckin(3); 
    ckin3 = 4.0;

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
void Pythia8( TString config="pp:W", const char* _library="libPythia8_1_62.so" )
{
  gSystem->Load( _library );

  //
  // Create the pythia 8 event generator and add it to 
  // the primary generator
  //
  StarPythia8 *pythia8 = new StarPythia8();    
  if ( config=="pp:W" )
    {
      pythia8->SetFrame("CMS", 510.0);
      pythia8->SetBlue("proton");
      pythia8->SetYell("proton");
      
      pythia8->Set("WeakSingleBoson:all=off");
      pythia8->Set("WeakSingleBoson:ffbar2W=on");
      pythia8->Set("24:onMode=0");              // switch off all W+/- decaus
      pythia8->Set("24:onIfAny 11 -11");        // switch on for decays to e+/-
      
    }
  if ( config=="pp:minbias" )
    {
      pythia8->SetFrame("CMS", 510.0);
      pythia8->SetBlue("proton");
      pythia8->SetYell("proton");            

      pythia8->Set("SoftQCD:minBias = on");
    }

  _primary -> AddGenerator( pythia8 );
  
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void starsim( Int_t nevents=1000, UInt_t rngSeed = 12345 )
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
    _primary -> SetFileName( "pythia8.starsim.root");
    //  chain -> AddBefore( "geant", primary );
  }

  //
  // Setup an event generator
  //
  Pythia8( "pp:minbias" );

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

}
// ----------------------------------------------------------------------------

