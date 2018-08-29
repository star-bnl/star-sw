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

    // Generate 1 mu minus at high pT
    kinematics->Kine( 1, "rho+", 10.0, 100.0, -2.0, 2.0 );

    // Generate the event
    chain->Make();

    // Print the event
    _primary->event()->Print();
    command("gprint kine");
  }
}
// ----------------------------------------------------------------------------
void Kinematics()
{
  
  //  gSystem->Load( "libStarGeneratorPoolPythia6_4_23.so" );
  gSystem->Load( "libKinematics.so");
  kinematics = new StarKinematics();
    
  _primary->AddGenerator(kinematics);
}
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
  gSystem->Load( "libPythia8_1_62.so");

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
  decayMgr->AddDecayer(    0, decayPy8 ); // Handle any decay requested 
  decayPy8->SetDebug(1);
  decayPy8->Set("WeakSingleBoson:all = on");




  TString name;
  double mass, lifetime, charge;
  int tracktype, pdgcode, g3code;

  // Particle data
  StarParticleData& data = StarParticleData::instance();
  //  One can add a particle to G3 using...
  //data.AddParticleToG3( "MyD0", 0.1865E+01, 0.42800E-12, 0., 3, 421, 37, 0, 0 );
  TParticlePDG* D0     = data.GetParticle("D0");
  TParticlePDG* rho_pl = data.GetParticle("rho+");
  TParticlePDG* rho_mn = data.GetParticle("rho-");
  data.AddParticleToG3( D0, 37 );
  data.AddParticleToG3( rho_pl, 153 );
  data.AddParticleToG3( rho_mn, 154 );


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
  //  trig( nevents );

  //  command("call agexit");  // Make sure that STARSIM exits properly

  trig(1); Validate();

}
// ----------------------------------------------------------------------------

void Validate()
{
  StarGenEvent*    event = _primary->event();
  StarGenParticle* part  = (*event)[1];
  part->Print();
  double Etotal = part->GetEnergy();

  // loop over MC particles
  TTable* gtable = (TTable* ) chain->GetDataSet("g2t_track");
  assert(gtable);
  int     ntable = gtable->GetNRows();

  //  gtable->Print(0,ntable);
  return;

  double Etest = 0;
  for ( int itable=1; itable<ntable; itable++ )
    {
      g2t_track_st* track = (g2t_track_st*)gtable->At(itable);
      if ( 0 == track->eg_label ) {       Etest += track->e; }
    }

  cout << "Egener = " << Etotal << endl;
  cout << "Egeant = " << Etest  << endl;
  cout << "Violation = " << 100*(Etest / Etotal - 1) << "%" << endl;
  
}
