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
    kinematics->Kine( 1, "B_c+", 5.0, 10.0, -2.0, 2.0 );
    chain->Make();
    // Print the event
    _primary->event()->Print();
    command("gprint kine");

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
void starsim( Int_t nevents=1, Int_t rngSeed=1234 )
{ 
 
  gROOT->ProcessLine(".L bfc.C");
  {
    TString simple = "y2012 geant gstar usexgeom agml ";
    bfc(0, simple );
  }

  gSystem->Load( "libVMC.so");

  gSystem->Load( "StarGeneratorUtil.so" );
  gSystem->Load( "StarGeneratorEvent.so" );
  gSystem->Load( "StarGeneratorBase.so" );
  gSystem->Load( "StarGeneratorDecay.so" );
  gSystem->Load( "libPythia8_1_86.so");

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
    _primary -> SetFileName( "kinematics.starsim.root");
    chain -> AddBefore( "geant", _primary );
  }

  Kinematics();



  //
  // Setup decay manager
  //
  StarDecayManager   *decayMgr = AgUDecay::Manager();
  StarPythia8Decayer *decayPy8 = new StarPythia8Decayer();
  decayMgr->AddDecayer( 0, decayPy8 ); // Handle any decay requested 
  decayPy8->SetDebug(1);
  decayPy8->Set("WeakSingleBoson:all = on");



  TString name;
  double mass, lifetime, charge;
  int tracktype, pdgcode, g3code;

  // Particle data
  StarParticleData& data = StarParticleData::instance();
  //  One can add a particle to G3 using...
  TParticlePDG* B_c_pl = data.GetParticle("B_c+");
  TParticlePDG* B_c_mn = data.GetParticle("B_c-");
  data.AddParticleToG3( B_c_pl, 50001 ); // pdg = 541
  data.AddParticleToG3( B_c_mn, 50002 ); 
  TParticlePDG* J_Psi = data.GetParticle("J/psi");
  data.AddParticleToG3( J_Psi,  50003 ); // pdg = 443
  decayPy8->Set("541:onMode = 0");
  decayPy8->Set("541:onIfAll = 443 11 12"); // only the J/psi + e + nu modes
  decayPy8->Set("443:onMode = 0");
  decayPy8->Set("443:onIfAll = 11");




  //
  // Initialize primary event generator and all sub makers
  //
  _primary -> Init();
  
  //
  // Setup geometry and set starsim to use agusread for input
  //
  geometry("y2012");
  command("gkine -4 0");
  command("gfile o kinematics.starsim.fzd");
 
  //
  // Trigger on nevents
  //
  trig( nevents );

  command("call agexit");  // Make sure that STARSIM exits properly

}
// ----------------------------------------------------------------------------

