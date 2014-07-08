/**
   Macro to run a custom c++ stepping routine in starsim.  The macro runs 
   StRoot/StarGenerator/STEP/AgUStep.cxx on each tracking step.  A tree is
   saved, saving each track and the steps taken on each track.  The energy
   lost in each tracking step is saved, along with the energy loss accumulated
   along the track.  The idTruth of each track is saved to enable offline
   comparison with event reconstruction.
 */

class St_geant_Maker;
St_geant_Maker *geant_maker = 0;

class StarGenEvent;
StarGenEvent   *event       = 0;

class StarPrimaryMaker;
StarPrimaryMaker *primary = 0;

class StarKinematics;
StarKinematics *kinematics = 0;

TF1 *ptDist  = 0;
TF1 *etaDist = 0;

// ----------------------------------------------------------------------------
void geometry( TString tag, Bool_t agml=true )
{
  TString cmd = "DETP GEOM "; cmd += tag; cmd+=" phys_off=1";
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

  //
  // Scan from zvertex 
  //
  Double_t zvertex = -99.5;
  for ( zvertex = -99.5; zvertex < 100.0; zvertex += 1.0 ) {

    // Clear the chain from the previous event
    chain->Clear();

    primary->SetVertex( 0.0, 0.0, zvertex );
    kinematics->Kine( 100, "pi-", 4.995, 5.005, -0.005, 0.005 );

    // Generate the event
    chain->Make();

    // Print the event
    primary->event()->Print();
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
    
  primary->AddGenerator(kinematics);
}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void LoadLibraries( const Char_t *chopts = "y2014a geant gstar usexgeom agml " )
{
  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = chopts; //"y2012 geant gstar usexgeom agml ";
    bfc(0, simple );
  }

  gSystem->Load( "libVMC.so");

  gSystem->Load( "StarGeneratorUtil.so" );
  gSystem->Load( "StarGeneratorEvent.so" );
  gSystem->Load( "StarGeneratorBase.so" );
  gSystem->Load( "StarGeneratorStep.so" );

  gSystem->Load( "libMathMore.so"   );  
  gSystem->Load( "xgeometry.so"     );

}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void starsim( Int_t nevents=1, Int_t rngSeed=1234 )
{ 

  LoadLibraries();

  //
  // Setup custom stepping
  //
  AgUStep *step = AgUStep::Instance(); {
    step->Init( "particle_history.root" );
  }

  // Setup RNG seed and map all ROOT TRandom here
  StarRandom::seed( rngSeed );
  StarRandom::capture();
  
  //
  // Create the primary event generator and insert it
  // before the geant maker
  //
  //  StarPrimaryMaker *
  primary = new StarPrimaryMaker();
  {
    primary -> SetFileName( "kinematics.starsim.root");
    chain -> AddBefore( "geant", primary );
  }
  primary->SetSigma( 0.0, 0.0, 0.0 );

  Kinematics();

  //
  // Initialize primary event generator and all sub makers
  //
  primary -> Init();


  //
  // Setup geometry and agusread for particle input
  //
  geometry("y2014a");
  command("gkine -4 0");

  //
  // Setup output file
  //
  command("gfile o pythia6.starsim.fzd");
  

  //
  // Trigger on events
  //
  trig();

  //
  // Finish up the stepper
  //
  step->Finish();

  command("call agexit");  // Make sure that STARSIM exits properly

}
// ----------------------------------------------------------------------------

