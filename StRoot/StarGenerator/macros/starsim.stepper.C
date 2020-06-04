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
StarPrimaryMaker *_primary = 0;

class StarKinematics;
StarKinematics *kinematics = 0;

TF1 *ptDist  = 0;
TF1 *etaDist = 0;

Double_t _zslice  = 0;
Int_t    _ntracks = 1;
Double_t _pt  = 5.0; // GeV
Double_t _eta = 0.0;
const Char_t *_part = "pi-";


Int_t    rngSeed = 12345;

// ----------------------------------------------------------------------------
void geometry( TString tag, Bool_t agml=true )
{
  TString cmd = "DETP GEOM "; cmd += tag; cmd+=" phys_off=1";

  if ( !geant_maker ) geant_maker = (St_geant_Maker *)chain->GetMaker("geant");
  geant_maker -> LoadGeometry(cmd);

  // make certain AgML loads
  TString  path = "$STAR/StarDb/AgMLGeometry";
  Char_t  *file = gSystem->Which( path.Data(), Form("Geometry.%s.C",tag.Data()), kReadPermission );
  assert(file);

  TString cmdL = Form(".L %s",file);
  TString cmdX = Form("CreateTable()");
  TString cmdU = Form(".U %s",file);
  
  gInterpreter -> ProcessLine( cmdL );
  gInterpreter -> Calc       ( cmdX );
  gInterpreter -> ProcessLine( cmdU );
  
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
  Double_t zvertex = _zslice;

  // Clear the chain from the previous event
  chain->Clear();

  _primary->SetVertex( 0.0, 0.0, zvertex );
  //  kinematics->Kine( _ntracks, "pi-", 4.995, 5.005, -0.005, 0.005 );
  //  kinematics -> Kine( _ntracks, "pi-", 0.39995, 0.40005, -0.0005, 0.0005 );
  kinematics -> Kine( _ntracks, _part, _pt - 0.005, _pt + 0.00t, _eta - 0.0005, _eta + 0.0005 );

  // Generate the event
  chain->Make();

  // Print the event
  //_primary->event()->Print();
    
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
//void starsim( Int_t nevents=1, Int_t rngSeed=1234 )
void starsim( Double_t zslice  = 0.0,
	      Int_t    nevents = 100,
	      Int_t    ntracks = 50,
	      const Char_t *fzname = "stiscan_zslice.fz",                   // MC zebra file
	      const Char_t *roname = "stiscan_zslice.track_history.root",   // Particle history file
	      const Char_t *rcname = "stiscan_zslice.evgen_record.root"     // Event generator record
	      )
{ 

  LoadLibraries();

  _zslice  = zslice;
  _ntracks = ntracks;

  //
  // Setup custom stepping
  //
  AgUStep *step = AgUStep::Instance(); 
  {
    step->Init( roname );
  }

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
    _primary -> SetFileName( rcname );
    chain -> AddBefore( "geant", _primary );
  }
  _primary->SetSigma( 0.0, 0.0, 0.0 );

  Kinematics();

  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();


  //
  // Setup geometry and agusread for particle input
  //
  geometry("y2014a");
  command("gkine -4 0");

  command( "DCAY 0" );
  command( "ANNI 0" );
  command( "BREM 0" );
  command( "COMP 0" );
  command( "HADR 0" );
  command( "MUNU 0" );
  command( "PAIR 0" );
  command( "PFIS 0" );
  command( "PHOT 0" );
  command( "RAYL 0" );
  command( "LOSS 4" );
  command( "DRAY 0" );
  command( "MULS 0" );
  command( "STRA 0" );
  command( "physi"  );

  //
  // Setup output file
  //
  command(  Form("gfile o %s",fzname ) );
  

  //
  // Trigger on events
  //
  for ( Int_t i=0;i<nevents;i++ )  trig();

  //
  // Finish up the stepper
  //
  step->Finish();

  command("call agexit");  // Make sure that STARSIM exits properly

}
// ----------------------------------------------------------------------------

