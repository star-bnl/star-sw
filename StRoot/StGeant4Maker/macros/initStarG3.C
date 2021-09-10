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

TF1 *ptDist  = 0;
TF1 *etaDist = 0;

void initChain(){ /* nada */ } 

// ----------------------------------------------------------------------------
void geometry( TString tag, Bool_t agml=true )
{
  TString cmd = "DETP GEOM "; cmd += tag;
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

    // Generate 1 mu minus at high pT
    kinematics->Kine( 1, "mu-", 10.0, 50.0, -2.0, 2.0 );


    // Generate the event
    chain->Make();

    // Print the event
    _primary->event()->Print();
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
void initStarG3( const char* tag="dev2021", Int_t nevents=0, Int_t rngSeed=1234 )
{ 

  // Add a few things to the include path
  gSystem->AddIncludePath(" -IStRoot -I${STAR}/StRoot -Igeom -IStarVMC -I${STAR}/StarVMC -IStarVMC/Geometry/macros -I.${STAR_HOST_SYS}/include ");
  gEnv->SetValue("Logger.Colors","YES");   

  gSystem->SetAclicMode(TSystem::kDebug);

  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = tag; simple += " geant gstar usexgeom agml ";
    bfc(0, simple );
  }

  gSystem->SetFlagsDebug("-g -std=c++0x");
  gSystem->SetFlagsOpt("-g -std=c++0x");

  gSystem->Load( "libVMC.so");

  gSystem->Load( "StarGeneratorUtil.so" );
  gSystem->Load( "StarGeneratorEvent.so" );
  gSystem->Load( "StarGeneratorBase.so" );

  gSystem->Load( "libMathMore.so"   );  
  gSystem->Load( "xgeometry.so"     );

  gSystem->Load( "StEpdUtil.so" );

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
    _primary -> SetFileName( "kinematics.starsim.root");
    chain -> AddBefore( "geant", _primary );
  }

  Kinematics();

  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();

  //
  // Setup geometry and set starsim to use agusread for input
  //
  //geometry("y2012");
  command("gkine -4 0");
  command("gfile o kinematics.starsim.fzd");
  

  //
  // Setup PT and ETA distributions
  //

  // Double_t pt0 = 3.0;
  // ptDist = new TF1("ptDist","(x/[0])/(1+(x/[0])^2)^6",0.0,10.0);
  // ptDist->SetParameter(0, pt0);
  // ptDist->Draw();

  // etaDist = new TF1("etaDist","-TMath::Erf(x+2.6)*TMath::Erf(x-2.6)",-0.8,+0.8);

  //
  // Trigger on nevents
  //
  trig( nevents );


}
// ----------------------------------------------------------------------------

